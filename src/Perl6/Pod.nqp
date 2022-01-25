class Perl6::Pod {

    # some helper classes
    class Kv {
        # a class to help construct a key/value pair
        # during %config reconstruction
        has $!key;
        has $!val;
        method reset()     { $!key := ''; $!val := '' }
        method add2key($c) { $!key := nqp::concat($!key, $c) }
        method add2val($c) { $!val := nqp::concat($!val, $c) }
        method pushpair(@arr, :$delete-numbered) {
            nqp::die("FATAL: unexpected push with null key") if !self.key;
            if $delete-numbered && $!key ne 'numbered' {
                @arr.push(nqp::list($!key, $!val));
            }
            self.reset;
        }
        method key() { return $!key }
        method val() { return $!val }
    }

    class Fc {
        # holds data for a formatting code chunk
        has $!code;
        has $!str;
        has @!meta;
        method init() {
            $!code := ''; $!str := ''; @!meta := [];
        }
        method setcode($c)  { $!code := $c }
        method add2str($s)  { $!str := nqp::concat($!str, $s) }
        method add2meta($s) { @!meta.push($s) }
        method code()       { return $!code }
        method string()     { return $!str }
        method meta()       { return @!meta }
    }

    class Stat {
        # a class to keep parser state during %config
        # reconstruction
        has $!inspc;
        has $!inkey;
        has $!inval;
        method setspc() { self.reset; $!inspc := 1;}
        method setkey() { self.reset; $!inkey := 1;}
        method setval() { self.reset; $!inval := 1;}
        method reset() {
            $!inspc := 0;
            $!inkey := 0;
            $!inval := 0;
        }
        method inspc() { return $!inspc;}
        method inkey() { return $!inkey;}
        method inval() { return $!inval;}
    }

    # various helpers for Pod parsing and processing

    my $caption := ''; # var to save table caption values between
                       # subs make_config and table

    # enable use of env vars for debug selections
    # for users
    my $udebug := nqp::ifnull(  nqp::atkey(  nqp::getenvhash(), 'RAKUDO_POD_TABLE_DEBUG'  ), 0  );
    # for developers
    my $debug  := nqp::ifnull(  nqp::atkey(  nqp::getenvhash(), 'RAKUDO_POD_TABLE_DEBUG_DEV'  ), 0  );
    my $debugp := nqp::ifnull(  nqp::atkey(  nqp::getenvhash(), 'RAKUDO_POD_DEBUG_DEV'  ), 0  );

    my $show_warning :=  1; # flag used to track the first warning so no repeated warnings are given
    my $table_num    := -1; # for user debugging, incremented by one on each call to sub table

    # UNICODE HEX VALUES AND NAMES FOR SPACE CHARACTERS
    # (from ftp://ftp.unicode.org/Public/UNIDATA/UnicodeData.txt):
    # (from https://en.wikipedia.org/wiki/Whitespace_character)
    # (which is from https://www.unicode.org/Public/UCD/latest/ucd/PropList.txt)
    #
    # The hex codes for spaces are in the range 0x0009..0x3000.  Those
    # were each inspected, and all chars not matching \h or \v characters were
    # eliminated leaving the following:
    #
    #=== 26 total chars =========
    #=== 2 total no-break chars:
    # 0x00A0 NO-BREAK SPACE ; Zs ; \h space char
    # 0x202F NARROW NO-BREAK SPACE ; Zs ; \h space char
    #=== 7 total vertical chars:
    # 0x000A <control-000A> ; alias: NEW LINE ; Cc ; \v space char
    # 0x000B <control-000B> ; alias: VERTICAL TABULATION ; Cc ; \v space char
    # 0x000C <control-000C> ; alias: FORM FEED ; Cc ; \v space char
    # 0x000D <control-000D> ; alias: CARRIAGE RETURN ; Cc ; \v space char
    # 0x0085 <control-0085> ; alias: NEXT LINE ; Cc ; \v space char
    # 0x2028 LINE SEPARATOR ; Zl ; \v space char
    # 0x2029 PARAGRAPH SEPARATOR ; Zp ; \v space char
    #=== 17 total horizontal chars:
    # 0x0009 <control-0009> ; alias: HORIZONTAL TABULATION; ; Cc ; \h space char
    # 0x0020 SPACE ; Zs ; \h space char
    # 0x1680 OGHAM SPACE MARK ; Zs ; \h space char
    # 0x180E MONGOLIAN VOWEL SEPARATOR ; Cf ; \h space char
    # 0x2000 EN SPACE ; Zs ; \h space char
    # 0x2001 EM SPACE ; Zs ; \h space char
    # 0x2002 EN SPACE ; Zs ; \h space char
    # 0x2003 EM SPACE ; Zs ; \h space char
    # 0x2004 THREE-PER-EM SPACE ; Zs ; \h space char
    # 0x2005 FOUR-PER-EM SPACE ; Zs ; \h space char
    # 0x2006 SIX-PER-EM SPACE ; Zs ; \h space char
    # 0x2007 FIGURE SPACE ; Zs ; \h space char
    # 0x2008 PUNCTUATION SPACE ; Zs ; \h space char
    # 0x2009 THIN SPACE ; Zs ; \h space char
    # 0x200A HAIR SPACE ; Zs ; \h space char
    # 0x205F MEDIUM MATHEMATICAL SPACE ; Zs ; \h space char
    # 0x3000 IDEOGRAPHIC SPACE ; Zs ; \h space char

    # using info from above, define a character class regex for space
    # chars to be used for word breaks and collapsing multiple
    # adjacent breaking spaces to one (normalizing text)
    my $breaking-spaces-regex := /[
                                  # vertical (\v) breaking space chars:
                                  <[
                                     \x[000A] .. \x[000D]
                                     \x[0085]
                                     \x[2028]
                                     \x[2029]
                                  ]>
                                  |
                                  # horizontal (\h) breaking space chars:
                                  <[
                                     \x[0009]
                                     \x[0020]
                                     \x[1680]
                                     \x[180E]
                                     \x[2000] .. \x[200A]
                                     \x[205F]
                                     \x[3000]
                                   ]>
                                 ]+/;
    # literal normal space (U+0020)
    my $SPACE := "\x[0020]";
    # empty string
    my $EMPTY := '';

    # block types
    my $para-block   := 'paragraph';
    my $delim-block  := 'delimited';
    my $abbrev-block := 'abbreviated';

    our sub document($/, $what, $with, :$leading, :$trailing) {
        if $leading && $trailing || !$leading && !$trailing {
            nqp::die("You must provide one of leading or trailing to Perl6::Pod::document");
        }
        if ~$with ne '' {
            if $leading {
                $*W.apply_trait($/, '&trait_mod:<is>', $what, :leading_docs($with));
            }
            else { # trailing
                $*W.apply_trait($/, '&trait_mod:<is>', $what, :trailing_docs($with));
            }
        }
    }

    our sub has-numbered-alias($/) {
        my $val := 0;
        if nqp::defined($<numbered-alias>) {
            $val := $<numbered-alias>.Str;
        }
        if nqp::defined($val) && $val ~~ /'#'/ {
            $val := 1;
            nqp::die("DEBUG FATAL if has '#'") if 0;
        }
        return $val;
    }

    our sub get-numbered-config() {
        # creates a NEW config hash for the '#' numbered alias
        # DO NOT USE THIS IF THE CONFIG HASH NEEDS MODIFYING

        # create key/value pairs to be serialized
        my @pairs := nqp::list();
        my $key   := 'numbered';
        my $val   := 1;
        @pairs.push(
            serialize_object(
                'Pair', :key($key), :value($val)
            ).compile_time_value
        );
        my $config := serialize_object('Hash', |@pairs).compile_time_value;
        return $config;
    }

    our sub defn($/, $blocktype) {
        # produces a Raku instance of Pod::Defn

        my $config := add-numbered-to-config($/);

        my $type := $<type>.Str;
        die("FATAL: the incoming object is NOT a =defn block, type: $type")
            if $type !~~ /^defn/;

        # the final Raku type
        my $p6type := 'Pod::Defn';

        # Get all content lines.  The first line is the term for all
        # block types.
        my @children := [];
        for $<pod_content> {
            # split lines at newlines
            my @lines := lines($_);
            for @lines {
                my $s := normalize_text($_);
                @children.push($s);
            }
        }

        my $term := @children.shift;

        # The remaining @children array should have lines of text with
        # an empty line being a paragraph separator. Combine
        # consecutive paragraph lines into a single line.  TODO ensure
        # formatted code is considered
        my @paras := [];
        my $para-line := '';
        for @children -> $line {
            if $line {
                # concat to the current line
                $para-line := nqp::concat($para-line, $SPACE) if $para-line;
                $para-line := nqp::concat($para-line, $line);
            }
            else {
                # put existing line in the para array and start a new line
                @paras.push($para-line);
                $para-line := '';
            }
        }
        # don't forget the last line
        @paras.push($para-line) if $para-line;

        # now build the Pod::Defn class
        my @pcontents := [];
        for @paras -> $para {
            # each para is a new pod para class with contents that may
            # include format code
            my @contents := nqp::list($para);
            @contents    := serialize_array(@contents).compile_time_value;
            my $obj := serialize_object('Pod::Block::Para', :@contents).compile_time_value;
            @pcontents.push($obj);
        }
        my $contents := serialize_array(@pcontents).compile_time_value;

        # TODO is this right?? should it not be a para?
        my $term-qast := $*W.add_constant(
            'Str', 'str', $term,
        ).compile_time_value;

        # build and return the complete object
        my $qast := serialize_object(
            $p6type,
            :config($config),
            :contents($contents),
            :term($term-qast),
        ).compile_time_value;
        return $qast;
    }

    our sub any_block($/, $blocktype) {
        my $config := add-numbered-to-config($/);

        my @children := [];
        my $type;
        my $leveled;
        my $ident  := $<type>.Str;

        if $ident ~~ /^item \d*$/ {
            $type    := 'Pod::Item';
            $leveled := 1;
        }
        elsif $ident ~~ /^head \d+$/ {
            $type    := 'Pod::Heading';
            $leveled := 1;
        }
        elsif $ident ~~ /^defn / {
            die("FATAL: should not be able to get here with a =defn block");
        }
        else {
            $type := 'Pod::Block::Named';
        }

        for $<pod_content> {
            my $array     := $_.ast;
            my int $elems := $array.elems;
            my int $i     := -1;
            while ++$i < $elems {
                @children.push($array.AT-POS($i));
            }
        }

        my $contents := serialize_array(@children).compile_time_value;
        if $leveled {
            my $level := nqp::substr($<type>.Str, 4);
            my $level-qast;
            if $level eq '' {
                $level := "1";
            }
            $level-qast := $*W.add_constant(
                'Int', 'int', +$level,
            ).compile_time_value;

            my $qast := serialize_object(
                $type,
                :level($level-qast),
                :config($config),
                :contents($contents)
            );
            return $qast.compile_time_value;
        }

        my $name := $*W.add_constant('Str', 'str', $<type>.Str);
        my $qast := serialize_object(
            'Pod::Block::Named',
            :name($name.compile_time_value),
            :config($config),
            :contents($contents),
        );
        return $qast.compile_time_value;
    }

    our sub raw_block($/) {
        my $config := $<pod_configuration>
            ?? $<pod_configuration>.ast
            !! serialize_object('Hash').compile_time_value;
        my $str := $*W.add_constant('Str', 'str', ~$<pod_content>);
        my $contents := serialize_array([$str.compile_time_value]);
        my $past := serialize_object(
            'Pod::Block::Comment', :config($config),
            :contents($contents.compile_time_value),
        );
        return $past.compile_time_value;
    }

    our sub config($/) {
        my $type := $*W.add_constant('Str', 'str', ~$<type>);
        return serialize_object(
            'Pod::Config', :type($type.compile_time_value),
            :config($<pod_configuration>.ast)
        ).compile_time_value
    }

    sub string_to_bigint($src, int $base, int $chars) {
        # code copied from Actions.nqp and locally modified
        my $res := nqp::radix_I($base, ~$src, 0, 2, $*W.find_single_symbol('Int'));
        $src.panic("'$src' is not a valid number"
                   ~ (nqp::iseq_i($base, 10) ?? '' !! " in base $base"))
            unless nqp::iseq_i(nqp::unbox_i(nqp::atpos($res, 2)), $chars);
        nqp::atpos($res, 0);
    }

    sub convert-array(@raw) {
        # input is an array of strings to be converted to a list or hash
        my @arr := nqp::list();
        for @raw -> $S {
            my $s := $S;
            say("===DEBUG: element to be converted: |$s|") if $debugp;

            # Convert the strings of numbers to int (or bigint) or num types.
            #=== integers ====================================================================
            if $s ~~ /^ <[+-]>? \d+ $/ ||
               $s ~~ /^ <[+-]>? \d+ % '_' $/ {
                # decint
                say("       element type is Int (dec)") if $debugp;
                my int $base := 10;
                my $val := string_to_bigint($s, $base, nqp::chars($s));
                @arr.push($val);
            }
            #=== numbers ====================================================================
            elsif $s ~~ /^ <["']>? <[+-]>? \d+ '.' \d+ [ <[eE]>? <[+-]>? \d+ ]? <["']>? $/ ||
                  $s ~~ /^ <["']>? <[+-]>? \d+ <[eE]> <[+-]>? \d+ <["']>? $/ {
                say("       element type is Num") if $debugp;
                my num $i := $s;
                my $val := $*W.add_constant('Num', 'num', $i).compile_time_value;
                @arr.push($val);
            }
            #=== booleans ===================================================================
            elsif $s ~~ /^ True | False $/ {
                # consolidate all True/False boolean handling here
                say("       element type is boolean") if $debugp;
                my $truth := $s ~~ /True/ ?? 1 !! 0;
                my $val   := $*W.add_constant('Bool', 'int', $truth).compile_time_value;
                @arr.push($val);
            }
            #=== strings ====================================================================
            else {
                say("       element type is Str") if $debugp;
                # leave as the default str
                @arr.push($s);
            }
        }
        return @arr;
    }

    sub string2array($Line,
                     :$hash?,
                     :$Delimiter?,
                     :$keep?,
                    ) {
        # Based on the 'parse_line' function in CPAN Perl module
        # Text::ParseString, but with many changes due to Raku and
        # nqp differences from Perl.

        # Options:
        #   $hash      - set true for a hash (default: array)
        #   $Delimiter - may be a regex (defaults are set for hash and array)
        #   $keep      - set true to keep enclosing quotes
        my $delimiter := $Delimiter ?? $Delimiter
                                    !! $hash ?? /','| '=>'/
                                    !! /','/;
        my $line   := $Line;
        my @pieces := [];
        my $word   := '';

        my $regex := /^
                       [
                         # double-quoted string
                         (<["]>)                                         # $0 - $quote
                         # |<== this group should not backtrack
                         ( [:r <-[\\"]>* [ \\ . <-[\\"]>* ]* ] ) <["]>   # $1 - $quoted (:r no backtracking)
                       ]
                     | # --OR--
                       [
                         # single-quoted string
                         (<[']>)                                         # $0 - quote
                         # |<== this group should not backtrack
                         ( [:r <-[\\']>* [ \\ . <-[\\']>* ]* ] ) <[']>   # $1 - $quoted (:r no backtracking)
                       ]
                     | # --OR--
                       [
                         # trimmed, unquoted string
                         \s* ( [ \\ . | <-[\\"']> ]*? ) \s*              # $0
                         # followed by
                         (                                               # $1
                           | $                # EOL
                           | # --OR--
                             $delimiter
                           | # --OR--
                             <before <["']> > # the next quote
                         )
                       ]/;

        my $pass := 0; # for debugging
        while nqp::chars($line) {
            ++$pass;
            my $m := match($line, $regex);
            if !$m {
                say("DEBUG: no line match after pass $pass!") if $debugp;
            }

            # The original algorithm uses s/// but we need to do the match first and
            # then the substitution to delete the matched string from the current,
            # remaining line.
            $line := subst($line, $regex, '');
            say("DEBUG pass $pass, postmatch:\n  \$line    = |$line|") if $debugp;

            # As opposed to the Perl version, only two match vars are recognized:
            # $m[0] and $m[1].
            my $quote    := $m[0];
            my $quoted   := $m[1];
            my $unquoted;
            my $delim;
            if !nqp::defined($quote) {
                say("DEBUG: returning null unexpectedly!");
                return [];
            }

            if $keep && $quote ~~ /^ <['"]> $/ {
                $quoted := nqp::concat($quote, nqp::concat($quoted, $quote));
            }

            if $quote eq '"' {
                $quoted := subst($quoted, /:s\\(.)/, $m[0], :global);
            }
            elsif $quote eq "'" {
                $quoted := subst($quoted, /\\(<[\\']>)/, $m[0], :global);
            }
            else {
                $unquoted := $quote;
                $delim    := $quoted;
                $quote    := nqp::null();
                $quoted   := nqp::null();
            }

            if nqp::chars($quote) || nqp::chars($quoted) {
                $word := nqp::concat($word, $quoted);
            }
            elsif nqp::chars($unquoted) {
                $word := nqp::concat($word, $unquoted);
            }

            if nqp::defined($delim) {
                @pieces.push($word) if nqp::chars($word);
                @pieces.push($delim) if ($keep eq 'delimiters');
                $word := '';
            }
            if !nqp::chars($line) && (nqp::chars($quote) || nqp::chars($word)) {
                @pieces.push($word);
                $word := '';
            }
        }

        # just in case there are chars in word
        if nqp::chars($word) {
            say("WARNING: Unexpected chars not matched.");
            @pieces.push($word);
            $word := '';
        }

        # Array elements should have no enclosing quotes, but they need
        # to be converted to the correct types for their content.
        @pieces := convert-array(@pieces);

        return @pieces;
    }

    sub make-config-list($st) {
        # the typical incoming string format inside the pipes (note the
        # original [] or () have been stripped by the ~$val<semilist>
        # step):
        #   |1, 'b', 3|
        # break into an array
        my @arr := string2array($st);

        if nqp::elems(@arr) == 1 {
            # convert a single-element list to a single value
            my $val := @arr[0];
            return $val;
        }
        else {
            # 0 or 2 or more elements are an array
            return serialize_object('Array', |@arr).compile_time_value;
        }
    }

    sub make-config-hash($st) {
        # the normally valid incoming string format inside the pipes:
        #   |{a => 1, b => 4, c => 10}|
        # strip enclosing curly braces
        my $s := subst($st, /^'{'/, '');
        $s := subst($s, /'}'$/, '');
        # break into an array
        my @arr := string2array($s, :hash);

        my @pairs := [];
        # iterate over the "hash" and create key/value pairs to be serialized
        for @arr -> $k, $v {
            my str $key := $k;
            # TODO check key for 'caption', warn of deprecation for version 6.d if found
            my $val     := $v;
            say("DEBUG hash: '$key' => '$val'") if $debugp;
            @pairs.push(
                serialize_object(
                    'Pair', :key($key), :value($val)
                ).compile_time_value
            );
        }
        return serialize_object('Hash', |@pairs).compile_time_value;
    }

    our sub make_config($/) {
        my @pairs := [];
        for $<colonpair> -> $colonpair {
            my $key := $colonpair<identifier>;
            say("==DEBUG config colonpair key: |$key|") if $debugp;

            my $val;
            if $colonpair<coloncircumfix><circumfix> {
                $val := $colonpair<coloncircumfix><circumfix>;
                say("  DEBUG incoming colonpair circumfix val: |$val|") if $debugp;

                if $val<nibble> {
                    # nibble values have enclosing <> stripped by the current process
                    # and need no further processing
                    $val := $*W.colonpair_nibble_to_str($/, $val<nibble>);
                    say("        nibble is a string literal after processing:   val: |$val|") if $debugp;
                }
                elsif $val<pblock> {
                    # a pblock {} is interpreted as a hash and the
                    # process dies if it doesn't compute as such
                    say("        pblock before processing:   val: |$val|") if $debugp;
                    $val := make-config-hash($val<pblock>);
                }
                elsif $val<semilist> {
                    # a semilist is enclosed in outer () and [] and either is interpreted as a list.
                    # a list with one element is converted to a single value of a str, bool, int, or num.
                    # semilists may have enclosing quotes which need to be stripped
                    say("        semilist BEFORE stringifying val: |$val|") if $debugp;
                    $val := ~$val<semilist>;
                    say("        semilist after stringifying val: |$val|") if $debugp;
                    $val := make-config-list($val);
                }

                # save any caption to the global value for use by sub table
                if $key eq 'caption' {
                    $caption := $val;
                }
            }
            else {
                say("  DEBUG incoming colonpair non-circumfix val: |$colonpair|") if $debugp;
                # issue #2793: should be able to use, e.g., ':nnnfoo' to represent:
                #   foo => nnn
                # new possibilities for non-circumfix val:
                #   foo    === foo(True)  # Bool; prefix = ''
                #   !foo   === foo(False) # Bool; prefix = '!'
                #   nnnfoo === foo(nnn)   # Int;  prefix = 'nnn'
                #
                # colonpair = $prefix ~ $key
                my $prefix := subst($colonpair, /$key/, '');
                $prefix := subst($prefix, /':'/, '');
                my $regex := /^ \d+ $/;

                say("  DEBUG colonpair non-circumfix prefix: |$prefix|") if $debugp;
                if $prefix eq '' {
                    $val := $*W.add_constant('Bool', 'int', 1).compile_time_value;
                }
                elsif $prefix eq '!' {
                    $val := $*W.add_constant('Bool', 'int', 0).compile_time_value;
                }
                elsif $prefix ~~ /^ \d+ $/ {
                    $val := $*W.add_constant('Int', 'int', $prefix).compile_time_value;
                }
                else {
                    nqp::die("FATAL:  Invalid key ($key) / colonpair ($colonpair) combo in pod config string");
                }

            }

            if $key eq 'allow' {
                my $chars := nqp::chars($val);
                my $pos := 0;
                while $pos < $chars {
                    my $char := nqp::substr($val, $pos, 1);
                    # space char
                    #if $char eq " " {
                    if $char eq $SPACE {
                        $pos := $pos + 1;
                    }
                    else {
                        my $bitval := nqp::ord($char) - nqp::ord("A");
                        if $bitval >= 0 && $bitval <= 25 {
                            $*POD_ALLOW_FCODES := $*POD_ALLOW_FCODES +| (2 ** $bitval);
                        }
                        $pos := $pos + 2;
                    }
                }
            }

            $key := $*W.add_constant('Str', 'str', $key).compile_time_value;
            @pairs.push(
                serialize_object(
                    'Pair', :key($key), :value($val)
                ).compile_time_value
            );
        }

        serialize_object('Hash', |@pairs).compile_time_value;
    }

    our sub normalize_text($a) {
        # Given a string of text, possibly including newlines, reduces
        # contiguous breaking whitespace to a single space.
        # Also trims leading and trailing whitespace from the string.
        # Note that non-breaking whitespace is not affected.

        # First, we normalize all breaking spaces.
        my $r := subst($a, $breaking-spaces-regex, $SPACE, :global);
        # Finally, trim the ends of the string.
        return $r := trim-string($r);
    }

    our sub remove_inline_comments($S) {
        # removes all Z<> comments from string $S
        # (note any use of char '>' inside the intended comment will not be handled as intended)
        # this could (should?) be handled by the grammar
        my $s   := $S; # the raw string to clean of comments
        my $ret := ''; # the cleaned string (i.e., without comments)
        my $idx := nqp::index($s, 'Z<'); # find the beginning of a comment
        while $idx > -1 {
            my $idx2 := nqp::index($s, '>', $idx+2); # and the end
            nqp::die("FATAL:  Non-existent closing '>' for inline pod comment in string '$s' in Table $table_num") if $idx2 < 0;
            my $s0 := nqp::substr($s, 0, $idx); # the leading chunk (which may be empty)
            # assemble the cleaned string by parts
            $ret := nqp::concat($ret, $s0);

            # throw away the orig string up to the end of the found comment
            $s := nqp::substr($s, $idx2+1); # the trailing chunk
            # look for another comment in the remaining string
            $idx := nqp::index($s, 'Z<');
        }

        # make sure we use up a non-empty string end
        if $s {
            $ret := nqp::concat($ret, $s);
        }
        return $ret;
    }

    our sub trim-string($a) {
        # Given a string of text, removes all whitespace from the ends,
        # including any newline.
        my $r := subst($a, /^\s*/, '');
        return subst($r, /\s*$/, '');
    }

    our sub pod_strings_from_matches(@string_matches) {
        my @strings := [];
        for @string_matches {
            @strings.push($_.ast);

        }
        return build_pod_strings(@strings);
    }

    our sub lines($S, :$no-chomp) {
        # Takes a string and separates it into an array of lines at
        # each newline.
        #
        # uses nqp::split(str $delimiter, str $string --> Mu)

        # not usual:
        return nqp::split("\n", $S) if $no-chomp;

        # default:
        # remove leading and trailing newlines
        my $s := trim-string($S);
        return nqp::split("\n", $s) if !$no-chomp;
    }

    # Takes an array of arrays of pod characters (normal character or
    # formatting code) returns an array of strings and formatting codes.
    # TODO use this sub to rebuild para term and defs as well as
    #      table cells. can we call back into Actions?
    #      see method pod_formatting_code
    our sub build_pod_strings(@strings) {
        my $in_code := $*POD_IN_CODE_BLOCK;

        sub push_chars(@chars, @where) {
            if @chars {
                my $s := nqp::join('', @chars);
                if ! $in_code {
                    # Collapse adjacent horizontal space characters to
                    # a single space character (unicode code point 0x0020).
                    # Note non-breaking whitespace is # not affected.
                    $s := subst($s, $breaking-spaces-regex, $SPACE, :global);
                }
                $s := $*W.add_constant('Str', 'str', $s).compile_time_value;
                @where.push($s);
            }
        }

        my @res      := [];
        my @chars    := [];
        my int $i    := 0;
        my int $last := nqp::elems(@strings) - 1;
        while $i <= $last {
            my @string := @strings[$i];
            for @string -> $char {
                if nqp::isstr($char) {
                    # don't push the leading whitespace unless code block
                    if $in_code || +@res + @chars != 0 || $char ne ' ' {
                        @chars.push($char);
                    }
                }
                else {
                    # formatting code - join the preceeding characters
                    # to the result, then add the formatting code
                    push_chars(@chars, @res);
                    @chars := [];
                    @res.push($char);
                }
            }

            if $i != $last {
                # space inbetween each string
                @chars.push(' ');
            }
            $i := $i + 1;
        }

        push_chars(@chars, @res);
        return @res;
    }

    our sub build_pod_chars($pod_string_characters) {
        my @chars := [];
        if $pod_string_characters {
            for $pod_string_characters {
                my $char := $_.ast;
                # $char can sometimes be an array because of the way
                # pod_balanced_braces works
                if nqp::istype($char,VMArray) {
                    for $char {
                        @chars.push($_);
                    }
                }
                else {
                    @chars.push($_.ast);
                }
            }
        }
        return @chars;
    }

    # serializes the given array
    our sub serialize_array(@arr) {
        return $*W.add_constant('Array', 'type_new', |@arr);
    }

    # serializes an array of strings
    our sub serialize_aos(@arr) {
        my @cells := [];
        for @arr -> $cell {
            my $p := $*W.add_constant('Str', 'str', ~$cell);
            @cells.push($p.compile_time_value);
        }
        return serialize_array(@cells);
    }

    # serializes an array of arrays of strings
    our sub serialize_aoaos(@rows) {
        my @content := [];
        for @rows -> $row {
            my $p := serialize_aos($row);
            @content.push($*W.scalar_wrap($p.compile_time_value));
        }
        return serialize_array(@content);
    }

    # serializes object of the given type
    our sub serialize_object($type, *@pos, *%named) {
        return $*W.add_constant($type, 'type_new', |@pos, |%named);
    }

    our sub add-numbered-to-config($/) {
        # given a $<pod_configuration>:
        #
        # if it is empty, create a new config with the :numbered(1) key/value
        #   if :numbered is true
        # else create the standard empty config
        # if not empty, extract the data, remove any existing :numbered key,
        #   rebuild the $config, and return the $config to the caller
        #
        # an example input (when stringified):
        #
        #   :c{e => 3, f => 4}  :n :k<a> :z{a => 0, b => 'q'} :last<end> \
        #      :b<foo bar> :numbered(1)
        #
        my $numbered-alias := has-numbered-alias($/);
        my $has-config     := $<pod_configuration> ?? 1 !! 0;
        my $config;
        if $has-config && $numbered-alias {
            my $s := $<pod_configuration>.Str;
            my @kvps := split-pod-config-keys($s, :delete-numbered(1));
            # build the new config
            my @pairs := [];
            for @kvps -> $kvp {
                # the key may need special handling
                my $key   := $kvp[0];
                my $bool  := 0;
                my $truth := 1;
                if $key ~~ /^'!' (.*)/ {
                    # it's negated so it's a boolean
                    $key   := nqp::substr($key, 1);
                    $bool  := 1;
                    $truth := 0;
                }
                $key := $*W.add_constant('Str', 'str', $key).compile_time_value;

                my $v := $kvp[1];
                $bool := 1 unless $v ~~ /\S/;

                # now handle the values
                # those sent to other subs return fully treated
                #   as a compile time value
                if $bool {
                    $v := $truth;
                    $v := $*W.add_constant('Bool', 'int', $truth).compile_time_value;
                }
                elsif $v ~~ /^'(' (.*) ')'$/ {
                    # a list [remove ()]
                    $v := ~$/[0];
                    $v := make-config-list($v);
                }
                elsif $v ~~ /^'[' (.*) ']'$/ {
                    # a list
                    # a list (remove [])
                    $v := ~$/[0];
                    $v := make-config-list($v);
                }
                elsif $v ~~ /^'{' .* '}'$/ {
                    # a hash (keep the {}
                    $v := make-config-hash($v);
                }
                elsif $v ~~ /^'<' (.*) '>'$/ {
                    # a string
                    $v := ~$/[0];
                }
                elsif !$v {
                    # treat as a true boolean
                    nqp::die("FATAL: unexpected empty value");
                }

                @pairs.push(
                    serialize_object(
                        'Pair', :key($key), :value($v)
                    ).compile_time_value
                );
            }
            # don't forget the :numbered
            @pairs.push(
                serialize_object(
                    'Pair', :key('numbered'), :value(1)
                ).compile_time_value
            );
            serialize_object('Hash', |@pairs).compile_time_value;
        }
        elsif $numbered-alias {
            $config := get-numbered-config();
        }
        elsif $has-config {
            $config := $<pod_configuration>.ast
        }
        else {
            # need an empty $config
            $config := serialize_object('Hash').compile_time_value;
        }
        $config;
    }

    our sub split-pod-config-keys($s, :$delete-numbered) {
        # The goal of this sub is to split the string into pairs of
        # keys and value strings. Then take the pairs and feed them
        # back through the generating functions to get a new config
        # hash as a compile_time_value.

        # do this the hard way
        my $kv := nqp::create(Kv);
        $kv.reset;
        my $st := nqp::create(Stat);
        $st.reset;

        my @kvps     := [];
        my $endchar  := $EMPTY;
        my $prevchar := '';

        my %endkeychar := nqp::hash(
            $SPACE, $SPACE,
            '(', ')',
            '{', '}',
            '[', ']',
            '<', '>',
        );

        my $nc := nqp::chars($s);
        my $idx := 0;
        # state doesn't change until the first ':' is seen
        # initial state
        $st.setspc;
        #nqp::say("DEBUG: inspc = {$st.inspc}; inkey = {$st.inkey}; inval = {$st.inval}");
        while $idx < $nc {
            my $c := nqp::substr($s, $idx, 1);
            say("DEBUG: char = '$c'; idx = $idx; prevchar = '$prevchar'") if 0;
            # skip insignificant whitespace
            if $c eq $SPACE && $st.inspc {
                $prevchar := $c;
                ++$idx;
                next;
            }

            if $c eq ':' {
                # usually the start of a new key/val chunk
                # but NOT if in a value
                if $st.inval {
                    $kv.add2val($c);
                }
                elsif $st.inspc && $prevchar eq $SPACE {
                    $kv.pushpair(@kvps, :$delete-numbered) if $kv.key;
                    $st.setkey;
                    # continue checking chars until we reach one of:
                    #   ' ', '{', '(', '<', '['
                }
                else {
                    nqp::say("FATAL: should not get here:");
                    nqp::say("  prevchar = '$prevchar'; char = '$c'; idx = $idx; nchars = $nc");
                    nqp::say("  inspc = {$st.inspc}; inkey = {$st.inkey}; inval = {$st.inval}");
                    nqp::die("  key = '{$kv.key}'; val = '{$kv.val}'");
                }
            }
            elsif $st.inkey {
                if nqp::existskey(%endkeychar, $c) {
                    # we've collected the key name, check validity
                    # look at emacs raku mode for ident regex
                    my $rx := /<[a..zA..Z]>/;

                    $endchar := nqp::atkey(%endkeychar, $c);
                    # if the ending char is a space, we have a null
                    # value and are looking to start another key
                    if $endchar eq $SPACE {
                        $kv.pushpair(@kvps, :$delete-numbered);
                        $endchar := $EMPTY;
                        $st.setspc;
                    }
                    else {
                        # we are in the value chunk looking for the ending char
                        $st.setval;
                        $kv.add2val($c);
                    }
                }
                else {
                    # this char goes with key name
                    $kv.add2key($c);
                }
            }
            elsif $st.inval && $c eq $endchar {
                # finished collecting the value
                $kv.add2val($c);
                $kv.pushpair(@kvps, :$delete-numbered);
                $st.setspc;
                $endchar := $EMPTY;
            }
            else {
                if $st.inval {
                    $kv.add2val($c);
                }
                elsif $st.inkey {
                    nqp::die("FATAL: unexpected space in key ($kv/key)") if $c eq $SPACE;
                    $kv.add2key($c);
                }
            }

            $prevchar := $c;
            ++$idx;
        }
        # clean up at the end
        my $err := 0;
        if $kv.key {
            say("ERROR: unexpected \$key left over");
            say("  '$kv.key'");
            ++$err;
        }
        if $kv.val {
            say("ERROR: unexpected \$val left over");
            say("  '$kv.val'");
            ++$err;
        }
        nqp::die("FATAL: errors at cleanup check") if $err;
        return @kvps;

    }

    our sub table($/, $blocktype) {
        # extract any caption from $config and serialize it
        my $cap := $caption
            ?? $*W.add_constant('Str', 'str', $caption).compile_time_value
            !! serialize_object('Str').compile_time_value;
        # reset global value for use of the next table
        $caption := '';

        my $config := add-numbered-to-config($/);

        # increment the table number for user debugging and reporting
        ++$table_num;

        # some rules for processing tables
        # row separator
        my $is_row_sep   := /^ <[-+_|=\h]>* $/;
        # legal row cell separators
        my $col_sep_pipe := /\h '|' \h | \h '|' $/; # visible
        my $col_sep_plus := /\h '+' \h | \h '+' $/; # visible
        my $col_sep_ws   := /\h \h/;     # invisible: double-space

        my $has_vis_col_sep  := / $col_sep_pipe | $col_sep_plus /;
        my $has_ws_col_sep   := / $col_sep_ws /;
        my $has_col_sep      := / $col_sep_pipe | $col_sep_plus | $col_sep_ws /;
        my $has_col_sep_plus := / $col_sep_plus /;

        # string literals for substitutions
        my $col_sep_pipe_literal := ' | '; # visible
        my $pipe  := '|';
        my $plus  := '+';
        my $space := ' ';

        # some vars for telling caller about table attributes, warnings, or exceptions
        # these are reset upon each call to sub table (i.e., for each table)
        my $table_has_no_col_seps   := 0;
        my @orig_rows               := [];
        my $has_shown_table_matrix  := 0;

        my $table_has_col_sep_plus      := 0;
        #=== for fatal conditions
        my $fatals := 0;
        my $table_has_multiple_row_seps := 0; # set true if multiple consecutive interior row seps
        my $table_has_vis_col_seps      := 0;
        my $table_has_ws_col_seps       := 0;
        my $table_has_data              := 0; # die if not set true
        #=== for warning conditions
        my $warns := 0;
        my $unbalanced_row_cells    := 0; # set true if all rows don't have same number of cells
        my $num_row_cells           := 0; # all table rows must have the same number of cells
        my $table_has_leading_row_seps             := 0;
        my $table_has_trailing_row_seps            := 0;
        my $table_has_border_vis_col_seps          := 0;
        my $table_has_leading_border_vis_col_seps  := 0;
        my $table_has_trailing_border_vis_col_seps := 0;

        # form the rows from the pod table parse match
        my @rows := [];
        nqp::say("===DEBUG NEW TABLE $table_num") if $debug;

        my $first_line            := 0; # set true when first non-row-sep line is seen
        my $last_line_was_row_sep := 0;
        for $<table_row> {
            # stringify the row for analysis and further handling
            my $row := $_.ast;
            # remove ending newline (AKA chomp)
            $row := subst($row, /\n$/, '');
            @orig_rows.push($row);
            if $row ~~ $is_row_sep {
                # leading row sep lines are deleted and warned about
                # two consecutive interior row sep lines are illegal
                if !$first_line {
                    # ignore it for now but create a warning
                    ++$table_has_leading_row_seps;
                    next;
                }
                if $last_line_was_row_sep {
                    # an invalid table if inside it
                    ++$table_has_multiple_row_seps;
                }
                else {
                    $last_line_was_row_sep := 1;
                }
            }
            else {
                $last_line_was_row_sep := 0;
                # this is a table data line
                $first_line := 1;
            }

            nqp::say("DEBUG RAW ROW (after chomp): '$row'") if $debug;
            # remove inline pod comment (Z<some comment to be ignored>)
            $row := remove_inline_comments($row) if $row ~~ /'Z<'/;
            unless $row ~~ $is_row_sep {
                ++$table_has_data;
                if $row ~~ $has_vis_col_sep {
                    #nqp::say("      VIS COL SEP ROW: '$row'") if $debug;
                    ++$table_has_col_sep_plus if $row ~~ $has_col_sep_plus;
                }
            }
            @rows.push($row);
        }
        nqp::say("===DEBUG END OF TABLE INPUT for table $table_num") if $debug;

        # remove trailing blank lines BEFORE checking for col separators
        @rows.pop while @rows && @rows[+@rows - 1] ~~ /^ \s* $/;

        # check for trailing row sep lines
        if @rows && @rows[+@rows - 1] ~~ $is_row_sep {
            ++$table_has_trailing_row_seps;
            @rows.pop while @rows && @rows[+@rows - 1] ~~ $is_row_sep;
        }

        # check for col seps AFTER trailing row separator lines have been removed
        for @rows -> $row {
            unless $row ~~ $is_row_sep {
                # Test the row for type of col seps. If a vis type is
                # found, then if a ws type also exists it shouldn't affect further
                # analysis. But then check for a ws type if a vis type is not found.
                if $row ~~ $has_vis_col_sep {
                    ++$table_has_vis_col_seps;
                }
                elsif $row ~~ $has_ws_col_sep {
                    ++$table_has_ws_col_seps;
                }
            }
        }

        # note warnings and fatal conditions are handled in a sub AFTER the next phases:

        # all tables have excess leading ws trimmed evenly
        @rows := trim_row_leading_ws(@rows);

        # break the data rows into cells
        if $table_has_vis_col_seps {
            @rows := split_vis_col_sep_rows(@rows);
        }
        elsif $table_has_ws_col_seps {
            @rows := split_ws_col_sep_rows(@rows);
        }
        else {
            # don't forget the single-column tables!
            @rows := split_ws_col_sep_rows(@rows);
        }

        # all warnings and invalid conditions should be known by now
        # time to warn or throw as appropriate
        handle_table_issues(@orig_rows);

        # Now separate the table into headers (if any) and content.
        #
        # We need to know 3 things about the row separators:
        #   + is there more than one?
        #   + where is the first one?
        #   + are they different from each other?
        #
        # Given no separators, our table is just an ordinary table
        #   of single lines.
        # If there is one separator, the table has a header and
        #   the actual content. If the separator is further than on the
        #   second row, then the header is multi-lined.
        # If there's more than one separator, the table has a multi-line
        #   header and multi-line content.
        #
        # Tricky, isn't it? Let's try to handle it sanely.
        my $firstsep;
        my int $sepnum        := 0;
        my int $firstsepindex := 0;
        my int $differentseps := 0;
        my int $i := -1;
        while ++$i < +@rows {
            unless nqp::islist(@rows[$i]) {
                $sepnum := $sepnum + 1;
                unless $firstsepindex {
                    $firstsepindex := $i
                }
                if $firstsep {
                    if $firstsep ne @rows[$i] {
                        $differentseps := 1
                    }
                }
                else {
                    $firstsep := @rows[$i];
                }
            }
            # multi-line rows are not yet merged, but we may need to
            # add empty cells to some rows
            if $unbalanced_row_cells && nqp::islist(@rows[$i]) {
                my $ncells := nqp::elems(@rows[$i]);
                if $ncells != $num_row_cells {
                    # pad row with needed empty cells
                    my $n := $num_row_cells - $ncells;
                    while $n > 0 {
                        nqp::push(@rows[$i], '');
                        --$n;
                    }
                }
            }
        }

        my $headers := [];
        my $content := [];

        if $sepnum == 0 {
            # ordinary table, no headers, one-lined rows
            $content := @rows;
        }
        elsif $sepnum == 1 {
            if $firstsepindex == 1 {
                # one-lined header, one-lined rows
                $headers := @rows.shift;
                @rows.shift; # remove the row separator
                $content := @rows;
            }
            else {
                # multi-line header, one-lined rows
                my @hlines := [];
                my int $i := -1;
                while ++$i < $firstsepindex {
                    @hlines.push(@rows.shift);
                }
                $headers := merge_rows(@hlines);
                @rows.shift; # remove the row separator
                $content := @rows;
            }
        }
        else {
            my @hlines := [];
            my int $i := -1;
            if $differentseps {
                while ++$i < $firstsepindex {
                    @hlines.push(@rows.shift);
                }
                @rows.shift; # remove the row separator
                $headers := merge_rows(@hlines);
            }
            # let's go through the rows and merge the multi-line ones
            my @newrows := [];
            my @tmp  := [];
            $i       := -1;
            while ++$i < +@rows {
                if nqp::islist(@rows[$i]) {
                    @tmp.push(@rows[$i]);
                }
                else {
                    @newrows.push(merge_rows(@tmp));
                    @tmp := [];
                }
            }
            if +@tmp > 0 {
                @newrows.push(merge_rows(@tmp));
            }
            $content := @newrows;
        }

        # show final table matrix
        if $debug || $udebug {
            # show final table matrix
            show_final_table_matrix($headers, $content);
        }

        # the table data are separated properly, now pack up
        # everything into a table object
        my $past := serialize_object(
            'Pod::Block::Table', :config($config), :caption($cap),
            :headers(serialize_aos($headers).compile_time_value),
            :contents(serialize_aoaos($content).compile_time_value),
        );

        # must return here before the sub subs begin
        return $past.compile_time_value;

        #===== TABLE-SPECIFIC SUBROUTINES =====
        sub unescape-col-seps($S) {
            my $s := subst($S, /'\+'/, '+', :global);
            $s    := subst($s, /'\|'/, '|', :global);
            return $s;
        }

        sub handle_table_issues(@rows) {
            my $t := $table_num;
            #=== invalid tables generate exceptions
            if $table_has_vis_col_seps && $table_has_ws_col_seps {
                ++$fatals;
                nqp::say("===FATAL: Table $t has a mixture of visible and invisible column-separator types.");
            }
            if $table_has_multiple_row_seps {
                ++$fatals;
                nqp::say("===FATAL: Table $t has multiple interior row separator lines.");
            }
            if !$table_has_data {
                ++$fatals;
                nqp::say("===FATAL: Table $t has no data.");
            }

            # bail out with data for a fatal
            if $fatals {
                nqp::say($_) for @rows; # the original table as input
                nqp::say("===end FATAL table $t input");
                nqp::die(1);
            }

            #=== warnings (only if $udebug)
            if $table_has_leading_row_seps || $table_has_trailing_row_seps {
                ++$warns;
                if $udebug {
                    nqp::say("===WARNING: Table $t has unneeded leading or trailing row separators.");
                }
            }

            if $table_has_border_vis_col_seps {
                ++$warns;
                if $udebug {
                    nqp::say("===WARNING: Table $t has unneeded border vis col separators.");
                }
            }

            if $warns && $udebug {
                nqp::say($_) for @rows; # the original table as input
                nqp::say("===end WARNING table $t input rows");
            }
        }

        sub normalize_vis_col_sep_rows(@Rows) {
            # leading and trailing column separators are handled and warned about
            my @rows     := @Rows;
            my int $nlp      := 0; # number of leading pipes
            my int $ntp      := 0; # number of trailing pipes
            my int $nr       := 0; # number of data rows
            my int $leading  := 0;
            my int $trailing := 0;
            my int $i        := -1;
            while ++$i < +@rows {
                unless @rows[$i] ~~ $is_row_sep {
                    ++$nr;
                    @rows[$i] := normalize_row_cells(@rows[$i]);
                    # check for leading pipes
                    if @rows[$i] ~~ /^ \s* '|' / {
                        ++$nlp;
                        ++$table_has_leading_border_vis_col_seps;
                        ++$table_has_border_vis_col_seps;
                    }
                    # check for trailing pipes
                    if @rows[$i] ~~ / '|' \s* $/ {
                        ++$ntp;
                        ++$table_has_trailing_border_vis_col_seps;
                        ++$table_has_border_vis_col_seps;
                    }
                    say("DEBUG: i $i, nlp $nlp, ntp $ntp, nr $nr") if $debug;
                }
            }
            ++$leading if $nlp == $nr;
            ++$trailing if $ntp == $nr;
            if $leading || $trailing {
                say("DEBUG: REMOVING BORDER PIPES") if $debug;
                # all data rows have the leading or trailng pipe, so
                # remove all including surrounding ws
                @rows := remove_border_pipes(@rows, $leading, $trailing);
            }
            return @rows;
        }

        sub remove_border_pipes(@Rows, $leading, $trailing) {
            my @rows :=  @Rows;
            my int $i := -1; # BUG: nqp did NOT warn about missing $i
            while ++$i < +@rows {
                unless @rows[$i] ~~ $is_row_sep {
                    if $leading || $trailing {
                        say("DEBUG BEFORE rm border: '@rows[$i]'") if $debug;
                    }
                    if $leading {
                        # remove the leading pipe and surrounding ws
                        @rows[$i] := subst(@rows[$i], /^ \h* '|' \h* /, '');
                    }
                    if $trailing {
                        # remove the trailing pipe and surrounding ws
                        @rows[$i] := subst(@rows[$i], / \h* '|' \h* $/, '');
                    }
                    if $leading || $trailing {
                        say("DEBUG AFTER rm border: '@rows[$i]'") if $debug;
                    }
                }
            }
            return @rows;
        }

        sub trim_row_leading_ws(@Rows) {
            # find the shortest leading whitespace and strip it
            # from every row
            my $w := 999999; # the shortest leading whitespace
            my @rows := @Rows;
            for @rows -> $row {
                next if $row ~~ /^\s*$/;
                my $match := $row ~~ /^\s+/;
                my $n := 0;
                $n := $match.to if $match;
                if $n < $w {
                    $w := $n;
                }
            }

            my int $i := -1;
            while ++$i < +@rows {
                unless @rows[$i] ~~ /^\s*$/ {
                    if $w != -1 {
                        @rows[$i] := nqp::substr(@rows[$i], $w);
                    }
                }
            }
            return @rows;
        }

        sub split_vis_col_sep_rows(@Rows) {
            my @rows := normalize_vis_col_sep_rows(@Rows);

            # split the vis col sep rows between cells
            # note we don't merge multiple rows yet
            my @res;
            my int $i := 0;
            for @rows -> $row {
                if $row ~~ $is_row_sep {
                    @res.push($row);
                }
                elsif nqp::isstr($row) {
                    # just split the row
                    nqp::say("VIS BEFORE SPLIT: '$row'") if $debug;
                    # split on ' | '
                    # unescape | and +
                    my @t := nqp::split(' | ', $row);
                    my @tmp := [];
                    my $j := 0;
                    for @t {
                        my $c := $_;
                        nqp::say("  CELL $j: '$c'") if $debug;
                        $c := normalize_text(unescape-col-seps($c));
                        nqp::say("  CELL $j normalized: '$c'") if $debug;
                        @tmp.push($c);
                        ++$j;
                    }
                    # this is the check for vis col sep rows
                    my $n := +@tmp;
                    check_num_row_cells($n);
                    @res.push(@tmp);
                }
                else {
                    nqp::say("WEIRD ROW number $i '$row'") if $debug;
                }
                ++$i;
            }
            return @res;
        }

        sub merge_rows(@rows) {
            my @result := @rows[0];
            my int $i := 1;
            while $i < +@rows {
                my $j := -1;
                while ++$j < +@rows[$i] {
                    if @rows[$i][$j] {
                        @result[$j] := normalize_text(
                            ~@result[$j] ~ ' ' ~ ~@rows[$i][$j]
                        );
                    }
                }
                ++$i;
            }
            return @result;
        }

        # takes an array of strings (rows of a table)
        # returns array of arrays of strings (cells)
        # NOTE: this only works for tables with double-space cell
        # separators!
        sub split_ws_col_sep_rows(@Rows) {
            my @rows := @Rows;

            my @suspects := [];  # positions that might be cell delimiters
            # values: 1     - impossible!
            #         unset - maybe

            # collect cell delimiters per row
            my int $i := -1;
            while ++$i < +@rows {
                unless @rows[$i] ~~ $is_row_sep {
                    my @line := nqp::split('', @rows[$i]);
                    my int $j := -1;
                    while ++$j < +@line {
                        unless @suspects[$j] {
                            if @line[$j] ne ' ' {
                                @suspects[$j] := 1;
                            }
                        }
                    }
                }
            }

            # now let's skip the single spaces by marking them impossible
            $i := -1;
            while ++$i < +@suspects {
                unless @suspects[$i] {
                    if @suspects[$i-1] && @suspects[$i+1] {
                        @suspects[$i] := 1;
                    }
                }
            }

            # now we're doing some magic which will
            # turn those positions into cell ranges
            # so, e.g., for 1 values in positions: 13 14 15   30 31 32 33
            # we get [0, 13, 16, 30, 34, 0] (last 0 as a guard)

            my $wasone := 1;
            my @ranges := [];
            @ranges.push(0);

            $i := -1;
            while ++$i < +@suspects {
                if !$wasone && @suspects[$i] == 1 {
                    @ranges.push($i);
                    $wasone := 1;
                }
                elsif $wasone && @suspects[$i] != 1 {
                    @ranges.push($i);
                    $wasone := 0;
                }
            }
            @ranges.push(0); # guard

            my @ret := [];
            for @rows -> $row {
                if $row ~~ $is_row_sep {
                    @ret.push($row);
                    next;
                }
                my @tmp := [];
                for @ranges -> $a, $b {
                    next if $a > nqp::chars($row);
                    if $b {
                        @tmp.push(
                            # must unescape | and +
                            normalize_text(unescape-col-seps(nqp::substr($row, $a, $b - $a)))
                        );
                    }
                    else {
                        @tmp.push(
                            # must unescape | and +
                            normalize_text(unescape-col-seps(nqp::substr($row, $a)))
                        );
                    }
                }
                # this is the check for ws col sep rows
                check_num_row_cells(+@tmp);
                @ret.push(@tmp);
            }
            return @ret;
        }

        sub check_num_row_cells($num_cells) {
            if !$num_cells {return}
            # checks a row's number of cells against the global value
            if !$num_row_cells {
                # the first row checked sets the mark
                $num_row_cells := $num_cells;
            }
            elsif $num_cells > $num_row_cells {
                $num_row_cells := $num_cells;
                ++$unbalanced_row_cells;
            }
            elsif $num_cells < $num_row_cells {
                ++$unbalanced_row_cells;
            }
        }

        sub normalize_row_cells($row) {
            # forces a pipe separator as the row cell separator
            my $s := $row;
            # add trailing ws to ensure correct split later
            $s := nqp::concat($s, ' ');
            $s := subst($s, /\h'+'\h/, $col_sep_pipe_literal, :global);
            return $s;
        }

        sub show_final_table_matrix($headers, $content) {
            nqp::say("===DEBUG: final cell layout for table $table_num.");
            nqp::say("=== cell contents are enclosed in single quotes");
            nqp::say("=== cell separators are shown as pipes ('|')");
            nqp::say("=== headers");
            if $headers {
                my $i := 0;
                for $headers -> $h {
                    nqp::print(" | ") if $i;
                    nqp::print("'$h'");
                    ++$i;
                }
                nqp::print("\n");
            }
            else {
                nqp::print(" (no headers)\n");
            }
            nqp::say("=== contents");
            for $content -> $row {
                my $i := 0;
                for $row -> $cell {
                    nqp::print(" | ") if $i;
                    nqp::print("'$cell'");
                    ++$i;
                }
                nqp::print("\n");
            }
            nqp::say("===DEBUG: end final cell layout for table $table_num.");
        }
        #===== END OF TABLE-SPECIFIC SUBROUTINES =====

    } # end sub table

#?if !jvm
    # Java 64K method limit can't compile this
    # all-lowercase HTML5 character entities, derived from
    # https://github.com/w3c/html/blob/master/entities.json
    my %entities := nqp::hash(
        'Aacute',[193],'aacute',[225],'Abreve',[258],'abreve',[259],'ac',[8766],'acd',[8767],
        'acE',[8766,819],'Acirc',[194],'acirc',[226],'acute',[180],'Acy',[1040],'acy',[1072],
        'AElig',[198],'aelig',[230],'af',[8289],'Afr',[120068],'afr',[120094],'Agrave',[192],
        'agrave',[224],'alefsym',[8501],'aleph',[8501],'Alpha',[913],'alpha',[945],'Amacr',[256],
        'amacr',[257],'amalg',[10815],'AMP',[38],'amp',[38],'And',[10835],'and',[8743],
        'andand',[10837],'andd',[10844],'andslope',[10840],'andv',[10842],'ang',[8736],'ange',[10660],
        'angle',[8736],'angmsd',[8737],'angmsdaa',[10664],'angmsdab',[10665],'angmsdac',[10666],'angmsdad',[10667],
        'angmsdae',[10668],'angmsdaf',[10669],'angmsdag',[10670],'angmsdah',[10671],'angrt',[8735],'angrtvb',[8894],
        'angrtvbd',[10653],'angsph',[8738],'angst',[197],'angzarr',[9084],'Aogon',[260],'aogon',[261],
        'Aopf',[120120],'aopf',[120146],'ap',[8776],'apacir',[10863],'apE',[10864],'ape',[8778],
        'apid',[8779],'apos',[39],'ApplyFunction',[8289],'approx',[8776],'approxeq',[8778],'Aring',[197],
        'aring',[229],'Ascr',[119964],'ascr',[119990],'Assign',[8788],'ast',[42],'asymp',[8776],
        'asympeq',[8781],'Atilde',[195],'atilde',[227],'Auml',[196],'auml',[228],'awconint',[8755],
        'awint',[10769],'backcong',[8780],'backepsilon',[1014],'backprime',[8245],'backsim',[8765],'backsimeq',[8909],
        'Backslash',[8726],'Barv',[10983],'barvee',[8893],'Barwed',[8966],'barwed',[8965],'barwedge',[8965],
        'bbrk',[9141],'bbrktbrk',[9142],'bcong',[8780],'Bcy',[1041],'bcy',[1073],'bdquo',[8222],
        'becaus',[8757],'Because',[8757],'because',[8757],'bemptyv',[10672],'bepsi',[1014],'bernou',[8492],
        'Bernoullis',[8492],'Beta',[914],'beta',[946],'beth',[8502],'between',[8812],'Bfr',[120069],
        'bfr',[120095],'bigcap',[8898],'bigcirc',[9711],'bigcup',[8899],'bigodot',[10752],'bigoplus',[10753],
        'bigotimes',[10754],'bigsqcup',[10758],'bigstar',[9733],'bigtriangledown',[9661],'bigtriangleup',[9651],'biguplus',[10756],
        'bigvee',[8897],'bigwedge',[8896],'bkarow',[10509],'blacklozenge',[10731],'blacksquare',[9642],'blacktriangle',[9652],
        'blacktriangledown',[9662],'blacktriangleleft',[9666],'blacktriangleright',[9656],'blank',[9251],'blk12',[9618],'blk14',[9617],
        'blk34',[9619],'block',[9608],'bne',[61,8421],'bnequiv',[8801,8421],'bNot',[10989],'bnot',[8976],
        'Bopf',[120121],'bopf',[120147],'bot',[8869],'bottom',[8869],'bowtie',[8904],'boxbox',[10697],
        'boxDL',[9559],'boxDl',[9558],'boxdL',[9557],'boxdl',[9488],'boxDR',[9556],'boxDr',[9555],
        'boxdR',[9554],'boxdr',[9484],'boxH',[9552],'boxh',[9472],'boxHD',[9574],'boxHd',[9572],
        'boxhD',[9573],'boxhd',[9516],'boxHU',[9577],'boxHu',[9575],'boxhU',[9576],'boxhu',[9524],
        'boxminus',[8863],'boxplus',[8862],'boxtimes',[8864],'boxUL',[9565],'boxUl',[9564],'boxuL',[9563],
        'boxul',[9496],'boxUR',[9562],'boxUr',[9561],'boxuR',[9560],'boxur',[9492],'boxV',[9553],
        'boxv',[9474],'boxVH',[9580],'boxVh',[9579],'boxvH',[9578],'boxvh',[9532],'boxVL',[9571],
        'boxVl',[9570],'boxvL',[9569],'boxvl',[9508],'boxVR',[9568],'boxVr',[9567],'boxvR',[9566],
        'boxvr',[9500],'bprime',[8245],'Breve',[728],'breve',[728],'brvbar',[166],'Bscr',[8492],
        'bscr',[119991],'bsemi',[8271],'bsim',[8765],'bsime',[8909],'bsol',[92],'bsolb',[10693],
        'bsolhsub',[10184],'bull',[8226],'bullet',[8226],'bump',[8782],'bumpE',[10926],'bumpe',[8783],
        'Bumpeq',[8782],'bumpeq',[8783],'Cacute',[262],'cacute',[263],'Cap',[8914],'cap',[8745],
        'capand',[10820],'capbrcup',[10825],'capcap',[10827],'capcup',[10823],'capdot',[10816],'CapitalDifferentialD',[8517],
        'caps',[8745,65024],'caret',[8257],'caron',[711],'Cayleys',[8493],'ccaps',[10829],'Ccaron',[268],
        'ccaron',[269],'Ccedil',[199],'ccedil',[231],'Ccirc',[264],'ccirc',[265],'Cconint',[8752],
        'ccups',[10828],'ccupssm',[10832],'Cdot',[266],'cdot',[267],'cedil',[184],'Cedilla',[184],
        'cemptyv',[10674],'cent',[162],'CenterDot',[183],'centerdot',[183],'Cfr',[8493],'cfr',[120096],
        'CHcy',[1063],'chcy',[1095],'check',[10003],'checkmark',[10003],'Chi',[935],'chi',[967],
        'cir',[9675],'circ',[710],'circeq',[8791],'circlearrowleft',[8634],'circlearrowright',[8635],'circledast',[8859],
        'circledcirc',[8858],'circleddash',[8861],'CircleDot',[8857],'circledR',[174],'circledS',[9416],'CircleMinus',[8854],
        'CirclePlus',[8853],'CircleTimes',[8855],'cirE',[10691],'cire',[8791],'cirfnint',[10768],'cirmid',[10991],
        'cirscir',[10690],'ClockwiseContourIntegral',[8754],'CloseCurlyDoubleQuote',[8221],'CloseCurlyQuote',[8217],'clubs',[9827],'clubsuit',[9827],
        'Colon',[8759],'colon',[58],'Colone',[10868],'colone',[8788],'coloneq',[8788],'comma',[44],
        'commat',[64],'comp',[8705],'compfn',[8728],'complement',[8705],'complexes',[8450],'cong',[8773],
        'congdot',[10861],'Congruent',[8801],'Conint',[8751],'conint',[8750],'ContourIntegral',[8750],'Copf',[8450],
        'copf',[120148],'coprod',[8720],'Coproduct',[8720],'COPY',[169],'copy',[169],'copysr',[8471],
        'CounterClockwiseContourIntegral',[8755],'crarr',[8629],'Cross',[10799],'cross',[10007],'Cscr',[119966],'cscr',[119992],
        'csub',[10959],'csube',[10961],'csup',[10960],'csupe',[10962],'ctdot',[8943],'cudarrl',[10552],
        'cudarrr',[10549],'cuepr',[8926],'cuesc',[8927],'cularr',[8630],'cularrp',[10557],'Cup',[8915],
        'cup',[8746],'cupbrcap',[10824],'CupCap',[8781],'cupcap',[10822],'cupcup',[10826],'cupdot',[8845],
        'cupor',[10821],'cups',[8746,65024],'curarr',[8631],'curarrm',[10556],'curlyeqprec',[8926],'curlyeqsucc',[8927],
        'curlyvee',[8910],'curlywedge',[8911],'curren',[164],'curvearrowleft',[8630],'curvearrowright',[8631],'cuvee',[8910],
        'cuwed',[8911],'cwconint',[8754],'cwint',[8753],'cylcty',[9005],'Dagger',[8225],'dagger',[8224],
        'daleth',[8504],'Darr',[8609],'dArr',[8659],'darr',[8595],'dash',[8208],'Dashv',[10980],
        'dashv',[8867],'dbkarow',[10511],'dblac',[733],'Dcaron',[270],'dcaron',[271],'Dcy',[1044],
        'dcy',[1076],'DD',[8517],'dd',[8518],'ddagger',[8225],'ddarr',[8650],'DDotrahd',[10513],
        'ddotseq',[10871],'deg',[176],'Del',[8711],'Delta',[916],'delta',[948],'demptyv',[10673],
        'dfisht',[10623],'Dfr',[120071],'dfr',[120097],'dHar',[10597],'dharl',[8643],'dharr',[8642],
        'DiacriticalAcute',[180],'DiacriticalDot',[729],'DiacriticalDoubleAcute',[733],'DiacriticalGrave',[96],'DiacriticalTilde',[732],'diam',[8900],
        'Diamond',[8900],'diamond',[8900],'diamondsuit',[9830],'diams',[9830],'die',[168],'DifferentialD',[8518],
        'digamma',[989],'disin',[8946],'div',[247],'divide',[247],'divideontimes',[8903],'divonx',[8903],
        'DJcy',[1026],'djcy',[1106],'dlcorn',[8990],'dlcrop',[8973],'dollar',[36],'Dopf',[120123],
        'dopf',[120149],'Dot',[168],'dot',[729],'DotDot',[8412],'doteq',[8784],'doteqdot',[8785],
        'DotEqual',[8784],'dotminus',[8760],'dotplus',[8724],'dotsquare',[8865],'doublebarwedge',[8966],'DoubleContourIntegral',[8751],
        'DoubleDot',[168],'DoubleDownArrow',[8659],'DoubleLeftArrow',[8656],'DoubleLeftRightArrow',[8660],'DoubleLeftTee',[10980],'DoubleLongLeftArrow',[10232],
        'DoubleLongLeftRightArrow',[10234],'DoubleLongRightArrow',[10233],'DoubleRightArrow',[8658],'DoubleRightTee',[8872],'DoubleUpArrow',[8657],'DoubleUpDownArrow',[8661],
        'DoubleVerticalBar',[8741],'DownArrow',[8595],'Downarrow',[8659],'downarrow',[8595],'DownArrowBar',[10515],'DownArrowUpArrow',[8693],
        'DownBreve',[785],'downdownarrows',[8650],'downharpoonleft',[8643],'downharpoonright',[8642],'DownLeftRightVector',[10576],'DownLeftTeeVector',[10590],
        'DownLeftVector',[8637],'DownLeftVectorBar',[10582],'DownRightTeeVector',[10591],'DownRightVector',[8641],'DownRightVectorBar',[10583],'DownTee',[8868],
        'DownTeeArrow',[8615],'drbkarow',[10512],'drcorn',[8991],'drcrop',[8972],'Dscr',[119967],'dscr',[119993],
        'DScy',[1029],'dscy',[1109],'dsol',[10742],'Dstrok',[272],'dstrok',[273],'dtdot',[8945],
        'dtri',[9663],'dtrif',[9662],'duarr',[8693],'duhar',[10607],'dwangle',[10662],'DZcy',[1039],
        'dzcy',[1119],'dzigrarr',[10239],'Eacute',[201],'eacute',[233],'easter',[10862],'Ecaron',[282],
        'ecaron',[283],'ecir',[8790],'Ecirc',[202],'ecirc',[234],'ecolon',[8789],'Ecy',[1069],
        'ecy',[1101],'eDDot',[10871],'Edot',[278],'eDot',[8785],'edot',[279],'ee',[8519],
        'efDot',[8786],'Efr',[120072],'efr',[120098],'eg',[10906],'Egrave',[200],'egrave',[232],
        'egs',[10902],'egsdot',[10904],'el',[10905],'Element',[8712],'elinters',[9191],'ell',[8467],
        'els',[10901],'elsdot',[10903],'Emacr',[274],'emacr',[275],'empty',[8709],'emptyset',[8709],
        'EmptySmallSquare',[9723],'emptyv',[8709],'EmptyVerySmallSquare',[9643],'emsp',[8195],'emsp13',[8196],'emsp14',[8197],
        'ENG',[330],'eng',[331],'ensp',[8194],'Eogon',[280],'eogon',[281],'Eopf',[120124],
        'eopf',[120150],'epar',[8917],'eparsl',[10723],'eplus',[10865],'epsi',[949],'Epsilon',[917],
        'epsilon',[949],'epsiv',[1013],'eqcirc',[8790],'eqcolon',[8789],'eqsim',[8770],'eqslantgtr',[10902],
        'eqslantless',[10901],'Equal',[10869],'equals',[61],'EqualTilde',[8770],'equest',[8799],'Equilibrium',[8652],
        'equiv',[8801],'equivDD',[10872],'eqvparsl',[10725],'erarr',[10609],'erDot',[8787],'Escr',[8496],
        'escr',[8495],'esdot',[8784],'Esim',[10867],'esim',[8770],'Eta',[919],'eta',[951],
        'ETH',[208],'eth',[240],'Euml',[203],'euml',[235],'euro',[8364],'excl',[33],
        'exist',[8707],'Exists',[8707],'expectation',[8496],'ExponentialE',[8519],'exponentiale',[8519],'fallingdotseq',[8786],
        'Fcy',[1060],'fcy',[1092],'female',[9792],'ffilig',[64259],'fflig',[64256],'ffllig',[64260],
        'Ffr',[120073],'ffr',[120099],'filig',[64257],'FilledSmallSquare',[9724],'FilledVerySmallSquare',[9642],'fjlig',[102,106],
        'flat',[9837],'fllig',[64258],'fltns',[9649],'fnof',[402],'Fopf',[120125],'fopf',[120151],
        'ForAll',[8704],'forall',[8704],'fork',[8916],'forkv',[10969],'Fouriertrf',[8497],'fpartint',[10765],
        'frac12',[189],'frac13',[8531],'frac14',[188],'frac15',[8533],'frac16',[8537],'frac18',[8539],
        'frac23',[8532],'frac25',[8534],'frac34',[190],'frac35',[8535],'frac38',[8540],'frac45',[8536],
        'frac56',[8538],'frac58',[8541],'frac78',[8542],'frasl',[8260],'frown',[8994],'Fscr',[8497],
        'fscr',[119995],'gacute',[501],'Gamma',[915],'gamma',[947],'Gammad',[988],'gammad',[989],
        'gap',[10886],'Gbreve',[286],'gbreve',[287],'Gcedil',[290],'Gcirc',[284],'gcirc',[285],
        'Gcy',[1043],'gcy',[1075],'Gdot',[288],'gdot',[289],'gE',[8807],'ge',[8805],
        'gEl',[10892],'gel',[8923],'geq',[8805],'geqq',[8807],'geqslant',[10878],'ges',[10878],
        'gescc',[10921],'gesdot',[10880],'gesdoto',[10882],'gesdotol',[10884],'gesl',[8923,65024],'gesles',[10900],
        'Gfr',[120074],'gfr',[120100],'Gg',[8921],'gg',[8811],'ggg',[8921],'gimel',[8503],
        'GJcy',[1027],'gjcy',[1107],'gl',[8823],'gla',[10917],'glE',[10898],'glj',[10916],
        'gnap',[10890],'gnapprox',[10890],'gnE',[8809],'gne',[10888],'gneq',[10888],'gneqq',[8809],
        'gnsim',[8935],'Gopf',[120126],'gopf',[120152],'grave',[96],'GreaterEqual',[8805],'GreaterEqualLess',[8923],
        'GreaterFullEqual',[8807],'GreaterGreater',[10914],'GreaterLess',[8823],'GreaterSlantEqual',[10878],'GreaterTilde',[8819],'Gscr',[119970],
        'gscr',[8458],'gsim',[8819],'gsime',[10894],'gsiml',[10896],'GT',[62],'Gt',[8811],
        'gt',[62],'gtcc',[10919],'gtcir',[10874],'gtdot',[8919],'gtlPar',[10645],'gtquest',[10876],
        'gtrapprox',[10886],'gtrarr',[10616],'gtrdot',[8919],'gtreqless',[8923],'gtreqqless',[10892],'gtrless',[8823],
        'gtrsim',[8819],'gvertneqq',[8809,65024],'gvnE',[8809,65024],'Hacek',[711],'hairsp',[8202],'half',[189],
        'hamilt',[8459],'HARDcy',[1066],'hardcy',[1098],'hArr',[8660],'harr',[8596],'harrcir',[10568],
        'harrw',[8621],'Hat',[94],'hbar',[8463],'Hcirc',[292],'hcirc',[293],'hearts',[9829],
        'heartsuit',[9829],'hellip',[8230],'hercon',[8889],'Hfr',[8460],'hfr',[120101],'HilbertSpace',[8459],
        'hksearow',[10533],'hkswarow',[10534],'hoarr',[8703],'homtht',[8763],'hookleftarrow',[8617],'hookrightarrow',[8618],
        'Hopf',[8461],'hopf',[120153],'horbar',[8213],'HorizontalLine',[9472],'Hscr',[8459],'hscr',[119997],
        'hslash',[8463],'Hstrok',[294],'hstrok',[295],'HumpDownHump',[8782],'HumpEqual',[8783],'hybull',[8259],
        'hyphen',[8208],'Iacute',[205],'iacute',[237],'ic',[8291],'Icirc',[206],'icirc',[238],
        'Icy',[1048],'icy',[1080],'Idot',[304],'IEcy',[1045],'iecy',[1077],'iexcl',[161],
        'iff',[8660],'Ifr',[8465],'ifr',[120102],'Igrave',[204],'igrave',[236],'ii',[8520],
        'iiiint',[10764],'iiint',[8749],'iinfin',[10716],'iiota',[8489],'IJlig',[306],'ijlig',[307],
        'Im',[8465],'Imacr',[298],'imacr',[299],'image',[8465],'ImaginaryI',[8520],'imagline',[8464],
        'imagpart',[8465],'imath',[305],'imof',[8887],'imped',[437],'Implies',[8658],'in',[8712],
        'incare',[8453],'infin',[8734],'infintie',[10717],'inodot',[305],'Int',[8748],'int',[8747],
        'intcal',[8890],'integers',[8484],'Integral',[8747],'intercal',[8890],'Intersection',[8898],'intlarhk',[10775],
        'intprod',[10812],'InvisibleComma',[8291],'InvisibleTimes',[8290],'IOcy',[1025],'iocy',[1105],'Iogon',[302],
        'iogon',[303],'Iopf',[120128],'iopf',[120154],'Iota',[921],'iota',[953],'iprod',[10812],
        'iquest',[191],'Iscr',[8464],'iscr',[119998],'isin',[8712],'isindot',[8949],'isinE',[8953],
        'isins',[8948],'isinsv',[8947],'isinv',[8712],'it',[8290],'Itilde',[296],'itilde',[297],
        'Iukcy',[1030],'iukcy',[1110],'Iuml',[207],'iuml',[239],'Jcirc',[308],'jcirc',[309],
        'Jcy',[1049],'jcy',[1081],'Jfr',[120077],'jfr',[120103],'jmath',[567],'Jopf',[120129],
        'jopf',[120155],'Jscr',[119973],'jscr',[119999],'Jsercy',[1032],'jsercy',[1112],'Jukcy',[1028],
        'jukcy',[1108],'Kappa',[922],'kappa',[954],'kappav',[1008],'Kcedil',[310],'kcedil',[311],
        'Kcy',[1050],'kcy',[1082],'Kfr',[120078],'kfr',[120104],'kgreen',[312],'KHcy',[1061],
        'khcy',[1093],'KJcy',[1036],'kjcy',[1116],'Kopf',[120130],'kopf',[120156],'Kscr',[119974],
        'kscr',[120000],'lAarr',[8666],'Lacute',[313],'lacute',[314],'laemptyv',[10676],'lagran',[8466],
        'Lambda',[923],'lambda',[955],'Lang',[10218],'lang',[10216],'langd',[10641],'langle',[10216],
        'lap',[10885],'Laplacetrf',[8466],'laquo',[171],'Larr',[8606],'lArr',[8656],'larr',[8592],
        'larrb',[8676],'larrbfs',[10527],'larrfs',[10525],'larrhk',[8617],'larrlp',[8619],'larrpl',[10553],
        'larrsim',[10611],'larrtl',[8610],'lat',[10923],'lAtail',[10523],'latail',[10521],'late',[10925],
        'lates',[10925,65024],'lBarr',[10510],'lbarr',[10508],'lbbrk',[10098],'lbrace',[123],'lbrack',[91],
        'lbrke',[10635],'lbrksld',[10639],'lbrkslu',[10637],'Lcaron',[317],'lcaron',[318],'Lcedil',[315],
        'lcedil',[316],'lceil',[8968],'lcub',[123],'Lcy',[1051],'lcy',[1083],'ldca',[10550],
        'ldquo',[8220],'ldquor',[8222],'ldrdhar',[10599],'ldrushar',[10571],'ldsh',[8626],'lE',[8806],
        'le',[8804],'LeftAngleBracket',[10216],'LeftArrow',[8592],'Leftarrow',[8656],'leftarrow',[8592],'LeftArrowBar',[8676],
        'LeftArrowRightArrow',[8646],'leftarrowtail',[8610],'LeftCeiling',[8968],'LeftDoubleBracket',[10214],'LeftDownTeeVector',[10593],'LeftDownVector',[8643],
        'LeftDownVectorBar',[10585],'LeftFloor',[8970],'leftharpoondown',[8637],'leftharpoonup',[8636],'leftleftarrows',[8647],'LeftRightArrow',[8596],
        'Leftrightarrow',[8660],'leftrightarrow',[8596],'leftrightarrows',[8646],'leftrightharpoons',[8651],'leftrightsquigarrow',[8621],'LeftRightVector',[10574],
        'LeftTee',[8867],'LeftTeeArrow',[8612],'LeftTeeVector',[10586],'leftthreetimes',[8907],'LeftTriangle',[8882],'LeftTriangleBar',[10703],
        'LeftTriangleEqual',[8884],'LeftUpDownVector',[10577],'LeftUpTeeVector',[10592],'LeftUpVector',[8639],'LeftUpVectorBar',[10584],'LeftVector',[8636],
        'LeftVectorBar',[10578],'lEg',[10891],'leg',[8922],'leq',[8804],'leqq',[8806],'leqslant',[10877],
        'les',[10877],'lescc',[10920],'lesdot',[10879],'lesdoto',[10881],'lesdotor',[10883],'lesg',[8922,65024],
        'lesges',[10899],'lessapprox',[10885],'lessdot',[8918],'lesseqgtr',[8922],'lesseqqgtr',[10891],'LessEqualGreater',[8922],
        'LessFullEqual',[8806],'LessGreater',[8822],'lessgtr',[8822],'LessLess',[10913],'lesssim',[8818],'LessSlantEqual',[10877],
        'LessTilde',[8818],'lfisht',[10620],'lfloor',[8970],'Lfr',[120079],'lfr',[120105],'lg',[8822],
        'lgE',[10897],'lHar',[10594],'lhard',[8637],'lharu',[8636],'lharul',[10602],'lhblk',[9604],
        'LJcy',[1033],'ljcy',[1113],'Ll',[8920],'ll',[8810],'llarr',[8647],'llcorner',[8990],
        'Lleftarrow',[8666],'llhard',[10603],'lltri',[9722],'Lmidot',[319],'lmidot',[320],'lmoust',[9136],
        'lmoustache',[9136],'lnap',[10889],'lnapprox',[10889],'lnE',[8808],'lne',[10887],'lneq',[10887],
        'lneqq',[8808],'lnsim',[8934],'loang',[10220],'loarr',[8701],'lobrk',[10214],'LongLeftArrow',[10229],
        'Longleftarrow',[10232],'longleftarrow',[10229],'LongLeftRightArrow',[10231],'Longleftrightarrow',[10234],'longleftrightarrow',[10231],'longmapsto',[10236],
        'LongRightArrow',[10230],'Longrightarrow',[10233],'longrightarrow',[10230],'looparrowleft',[8619],'looparrowright',[8620],'lopar',[10629],
        'Lopf',[120131],'lopf',[120157],'loplus',[10797],'lotimes',[10804],'lowast',[8727],'lowbar',[95],
        'LowerLeftArrow',[8601],'LowerRightArrow',[8600],'loz',[9674],'lozenge',[9674],'lozf',[10731],'lpar',[40],
        'lparlt',[10643],'lrarr',[8646],'lrcorner',[8991],'lrhar',[8651],'lrhard',[10605],'lrm',[8206],
        'lrtri',[8895],'lsaquo',[8249],'Lscr',[8466],'lscr',[120001],'Lsh',[8624],'lsh',[8624],
        'lsim',[8818],'lsime',[10893],'lsimg',[10895],'lsqb',[91],'lsquo',[8216],'lsquor',[8218],
        'Lstrok',[321],'lstrok',[322],'LT',[60],'Lt',[8810],'lt',[60],'ltcc',[10918],
        'ltcir',[10873],'ltdot',[8918],'lthree',[8907],'ltimes',[8905],'ltlarr',[10614],'ltquest',[10875],
        'ltri',[9667],'ltrie',[8884],'ltrif',[9666],'ltrPar',[10646],'lurdshar',[10570],'luruhar',[10598],
        'lvertneqq',[8808,65024],'lvnE',[8808,65024],'macr',[175],'male',[9794],'malt',[10016],'maltese',[10016],
        'Map',[10501],'map',[8614],'mapsto',[8614],'mapstodown',[8615],'mapstoleft',[8612],'mapstoup',[8613],
        'marker',[9646],'mcomma',[10793],'Mcy',[1052],'mcy',[1084],'mdash',[8212],'mDDot',[8762],
        'measuredangle',[8737],'MediumSpace',[8287],'Mellintrf',[8499],'Mfr',[120080],'mfr',[120106],'mho',[8487],
        'micro',[181],'mid',[8739],'midast',[42],'midcir',[10992],'middot',[183],'minus',[8722],
        'minusb',[8863],'minusd',[8760],'minusdu',[10794],'MinusPlus',[8723],'mlcp',[10971],'mldr',[8230],
        'mnplus',[8723],'models',[8871],'Mopf',[120132],'mopf',[120158],'mp',[8723],'Mscr',[8499],
        'mscr',[120002],'mstpos',[8766],'Mu',[924],'mu',[956],'multimap',[8888],'mumap',[8888],
        'nabla',[8711],'Nacute',[323],'nacute',[324],'nang',[8736,8402],'nap',[8777],'napE',[10864,824],
        'napid',[8779,824],'napos',[329],'napprox',[8777],'natur',[9838],'natural',[9838],'naturals',[8469],
        'nbsp',[160],'nbump',[8782,824],'nbumpe',[8783,824],'ncap',[10819],'Ncaron',[327],'ncaron',[328],
        'Ncedil',[325],'ncedil',[326],'ncong',[8775],'ncongdot',[10861,824],'ncup',[10818],'Ncy',[1053],
        'ncy',[1085],'ndash',[8211],'ne',[8800],'nearhk',[10532],'neArr',[8663],'nearr',[8599],
        'nearrow',[8599],'nedot',[8784,824],'NegativeMediumSpace',[8203],'NegativeThickSpace',[8203],'NegativeThinSpace',[8203],'NegativeVeryThinSpace',[8203],
        'nequiv',[8802],'nesear',[10536],'nesim',[8770,824],'NestedGreaterGreater',[8811],'NestedLessLess',[8810],'NewLine',[10],
        'nexist',[8708],'nexists',[8708],'Nfr',[120081],'nfr',[120107],'ngE',[8807,824],'nge',[8817],
        'ngeq',[8817],'ngeqq',[8807,824],'ngeqslant',[10878,824],'nges',[10878,824],'nGg',[8921,824],'ngsim',[8821],
        'nGt',[8811,8402],'ngt',[8815],'ngtr',[8815],'nGtv',[8811,824],'nhArr',[8654],'nharr',[8622],
        'nhpar',[10994],'ni',[8715],'nis',[8956],'nisd',[8954],'niv',[8715],'NJcy',[1034],
        'njcy',[1114],'nlArr',[8653],'nlarr',[8602],'nldr',[8229],'nlE',[8806,824],'nle',[8816],
        'nLeftarrow',[8653],'nleftarrow',[8602],'nLeftrightarrow',[8654],'nleftrightarrow',[8622],'nleq',[8816],'nleqq',[8806,824],
        'nleqslant',[10877,824],'nles',[10877,824],'nless',[8814],'nLl',[8920,824],'nlsim',[8820],'nLt',[8810,8402],
        'nlt',[8814],'nltri',[8938],'nltrie',[8940],'nLtv',[8810,824],'nmid',[8740],'NoBreak',[8288],
        'NonBreakingSpace',[160],'Nopf',[8469],'nopf',[120159],'Not',[10988],'not',[172],'NotCongruent',[8802],
        'NotCupCap',[8813],'NotDoubleVerticalBar',[8742],'NotElement',[8713],'NotEqual',[8800],'NotEqualTilde',[8770,824],'NotExists',[8708],
        'NotGreater',[8815],'NotGreaterEqual',[8817],'NotGreaterFullEqual',[8807,824],'NotGreaterGreater',[8811,824],'NotGreaterLess',[8825],'NotGreaterSlantEqual',[10878,824],
        'NotGreaterTilde',[8821],'NotHumpDownHump',[8782,824],'NotHumpEqual',[8783,824],'notin',[8713],'notindot',[8949,824],'notinE',[8953,824],
        'notinva',[8713],'notinvb',[8951],'notinvc',[8950],'NotLeftTriangle',[8938],'NotLeftTriangleBar',[10703,824],'NotLeftTriangleEqual',[8940],
        'NotLess',[8814],'NotLessEqual',[8816],'NotLessGreater',[8824],'NotLessLess',[8810,824],'NotLessSlantEqual',[10877,824],'NotLessTilde',[8820],
        'NotNestedGreaterGreater',[10914,824],'NotNestedLessLess',[10913,824],'notni',[8716],'notniva',[8716],'notnivb',[8958],'notnivc',[8957],
        'NotPrecedes',[8832],'NotPrecedesEqual',[10927,824],'NotPrecedesSlantEqual',[8928],'NotReverseElement',[8716],'NotRightTriangle',[8939],'NotRightTriangleBar',[10704,824],
        'NotRightTriangleEqual',[8941],'NotSquareSubset',[8847,824],'NotSquareSubsetEqual',[8930],'NotSquareSuperset',[8848,824],'NotSquareSupersetEqual',[8931],'NotSubset',[8834,8402],
        'NotSubsetEqual',[8840],'NotSucceeds',[8833],'NotSucceedsEqual',[10928,824],'NotSucceedsSlantEqual',[8929],'NotSucceedsTilde',[8831,824],'NotSuperset',[8835,8402],
        'NotSupersetEqual',[8841],'NotTilde',[8769],'NotTildeEqual',[8772],'NotTildeFullEqual',[8775],'NotTildeTilde',[8777],'NotVerticalBar',[8740],
        'npar',[8742],'nparallel',[8742],'nparsl',[11005,8421],'npart',[8706,824],'npolint',[10772],'npr',[8832],
        'nprcue',[8928],'npre',[10927,824],'nprec',[8832],'npreceq',[10927,824],'nrArr',[8655],'nrarr',[8603],
        'nrarrc',[10547,824],'nrarrw',[8605,824],'nRightarrow',[8655],'nrightarrow',[8603],'nrtri',[8939],'nrtrie',[8941],
        'nsc',[8833],'nsccue',[8929],'nsce',[10928,824],'Nscr',[119977],'nscr',[120003],'nshortmid',[8740],
        'nshortparallel',[8742],'nsim',[8769],'nsime',[8772],'nsimeq',[8772],'nsmid',[8740],'nspar',[8742],
        'nsqsube',[8930],'nsqsupe',[8931],'nsub',[8836],'nsubE',[10949,824],'nsube',[8840],'nsubset',[8834,8402],
        'nsubseteq',[8840],'nsubseteqq',[10949,824],'nsucc',[8833],'nsucceq',[10928,824],'nsup',[8837],'nsupE',[10950,824],
        'nsupe',[8841],'nsupset',[8835,8402],'nsupseteq',[8841],'nsupseteqq',[10950,824],'ntgl',[8825],'Ntilde',[209],
        'ntilde',[241],'ntlg',[8824],'ntriangleleft',[8938],'ntrianglelefteq',[8940],'ntriangleright',[8939],'ntrianglerighteq',[8941],
        'Nu',[925],'nu',[957],'num',[35],'numero',[8470],'numsp',[8199],'nvap',[8781,8402],
        'nVDash',[8879],'nVdash',[8878],'nvDash',[8877],'nvdash',[8876],'nvge',[8805,8402],'nvgt',[62,8402],
        'nvHarr',[10500],'nvinfin',[10718],'nvlArr',[10498],'nvle',[8804,8402],'nvlt',[60,8402],'nvltrie',[8884,8402],
        'nvrArr',[10499],'nvrtrie',[8885,8402],'nvsim',[8764,8402],'nwarhk',[10531],'nwArr',[8662],'nwarr',[8598],
        'nwarrow',[8598],'nwnear',[10535],'Oacute',[211],'oacute',[243],'oast',[8859],'ocir',[8858],
        'Ocirc',[212],'ocirc',[244],'Ocy',[1054],'ocy',[1086],'odash',[8861],'Odblac',[336],
        'odblac',[337],'odiv',[10808],'odot',[8857],'odsold',[10684],'OElig',[338],'oelig',[339],
        'ofcir',[10687],'Ofr',[120082],'ofr',[120108],'ogon',[731],'Ograve',[210],'ograve',[242],
        'ogt',[10689],'ohbar',[10677],'ohm',[937],'oint',[8750],'olarr',[8634],'olcir',[10686],
        'olcross',[10683],'oline',[8254],'olt',[10688],'Omacr',[332],'omacr',[333],'Omega',[937],
        'omega',[969],'Omicron',[927],'omicron',[959],'omid',[10678],'ominus',[8854],'Oopf',[120134],
        'oopf',[120160],'opar',[10679],'OpenCurlyDoubleQuote',[8220],'OpenCurlyQuote',[8216],'operp',[10681],'oplus',[8853],
        'Or',[10836],'or',[8744],'orarr',[8635],'ord',[10845],'order',[8500],'orderof',[8500],
        'ordf',[170],'ordm',[186],'origof',[8886],'oror',[10838],'orslope',[10839],'orv',[10843],
        'oS',[9416],'Oscr',[119978],'oscr',[8500],'Oslash',[216],'oslash',[248],'osol',[8856],
        'Otilde',[213],'otilde',[245],'Otimes',[10807],'otimes',[8855],'otimesas',[10806],'Ouml',[214],
        'ouml',[246],'ovbar',[9021],'OverBar',[8254],'OverBrace',[9182],'OverBracket',[9140],'OverParenthesis',[9180],
        'par',[8741],'para',[182],'parallel',[8741],'parsim',[10995],'parsl',[11005],'part',[8706],
        'PartialD',[8706],'Pcy',[1055],'pcy',[1087],'percnt',[37],'period',[46],'permil',[8240],
        'perp',[8869],'pertenk',[8241],'Pfr',[120083],'pfr',[120109],'Phi',[934],'phi',[966],
        'phiv',[981],'phmmat',[8499],'phone',[9742],'Pi',[928],'pi',[960],'pitchfork',[8916],
        'piv',[982],'planck',[8463],'planckh',[8462],'plankv',[8463],'plus',[43],'plusacir',[10787],
        'plusb',[8862],'pluscir',[10786],'plusdo',[8724],'plusdu',[10789],'pluse',[10866],'PlusMinus',[177],
        'plusmn',[177],'plussim',[10790],'plustwo',[10791],'pm',[177],'Poincareplane',[8460],'pointint',[10773],
        'Popf',[8473],'popf',[120161],'pound',[163],'Pr',[10939],'pr',[8826],'prap',[10935],
        'prcue',[8828],'prE',[10931],'pre',[10927],'prec',[8826],'precapprox',[10935],'preccurlyeq',[8828],
        'Precedes',[8826],'PrecedesEqual',[10927],'PrecedesSlantEqual',[8828],'PrecedesTilde',[8830],'preceq',[10927],'precnapprox',[10937],
        'precneqq',[10933],'precnsim',[8936],'precsim',[8830],'Prime',[8243],'prime',[8242],'primes',[8473],
        'prnap',[10937],'prnE',[10933],'prnsim',[8936],'prod',[8719],'Product',[8719],'profalar',[9006],
        'profline',[8978],'profsurf',[8979],'prop',[8733],'Proportion',[8759],'Proportional',[8733],'propto',[8733],
        'prsim',[8830],'prurel',[8880],'Pscr',[119979],'pscr',[120005],'Psi',[936],'psi',[968],
        'puncsp',[8200],'Qfr',[120084],'qfr',[120110],'qint',[10764],'Qopf',[8474],'qopf',[120162],
        'qprime',[8279],'Qscr',[119980],'qscr',[120006],'quaternions',[8461],'quatint',[10774],'quest',[63],
        'questeq',[8799],'QUOT',[34],'quot',[34],'rAarr',[8667],'race',[8765,817],'Racute',[340],
        'racute',[341],'radic',[8730],'raemptyv',[10675],'Rang',[10219],'rang',[10217],'rangd',[10642],
        'range',[10661],'rangle',[10217],'raquo',[187],'Rarr',[8608],'rArr',[8658],'rarr',[8594],
        'rarrap',[10613],'rarrb',[8677],'rarrbfs',[10528],'rarrc',[10547],'rarrfs',[10526],'rarrhk',[8618],
        'rarrlp',[8620],'rarrpl',[10565],'rarrsim',[10612],'Rarrtl',[10518],'rarrtl',[8611],'rarrw',[8605],
        'rAtail',[10524],'ratail',[10522],'ratio',[8758],'rationals',[8474],'RBarr',[10512],'rBarr',[10511],
        'rbarr',[10509],'rbbrk',[10099],'rbrace',[125],'rbrack',[93],'rbrke',[10636],'rbrksld',[10638],
        'rbrkslu',[10640],'Rcaron',[344],'rcaron',[345],'Rcedil',[342],'rcedil',[343],'rceil',[8969],
        'rcub',[125],'Rcy',[1056],'rcy',[1088],'rdca',[10551],'rdldhar',[10601],'rdquo',[8221],
        'rdquor',[8221],'rdsh',[8627],'Re',[8476],'real',[8476],'realine',[8475],'realpart',[8476],
        'reals',[8477],'rect',[9645],'REG',[174],'reg',[174],'ReverseElement',[8715],'ReverseEquilibrium',[8651],
        'ReverseUpEquilibrium',[10607],'rfisht',[10621],'rfloor',[8971],'Rfr',[8476],'rfr',[120111],'rHar',[10596],
        'rhard',[8641],'rharu',[8640],'rharul',[10604],'Rho',[929],'rho',[961],'rhov',[1009],
        'RightAngleBracket',[10217],'RightArrow',[8594],'Rightarrow',[8658],'rightarrow',[8594],'RightArrowBar',[8677],'RightArrowLeftArrow',[8644],
        'rightarrowtail',[8611],'RightCeiling',[8969],'RightDoubleBracket',[10215],'RightDownTeeVector',[10589],'RightDownVector',[8642],'RightDownVectorBar',[10581],
        'RightFloor',[8971],'rightharpoondown',[8641],'rightharpoonup',[8640],'rightleftarrows',[8644],'rightleftharpoons',[8652],'rightrightarrows',[8649],
        'rightsquigarrow',[8605],'RightTee',[8866],'RightTeeArrow',[8614],'RightTeeVector',[10587],'rightthreetimes',[8908],'RightTriangle',[8883],
        'RightTriangleBar',[10704],'RightTriangleEqual',[8885],'RightUpDownVector',[10575],'RightUpTeeVector',[10588],'RightUpVector',[8638],'RightUpVectorBar',[10580],
        'RightVector',[8640],'RightVectorBar',[10579],'ring',[730],'risingdotseq',[8787],'rlarr',[8644],'rlhar',[8652],
        'rlm',[8207],'rmoust',[9137],'rmoustache',[9137],'rnmid',[10990],'roang',[10221],'roarr',[8702],
        'robrk',[10215],'ropar',[10630],'Ropf',[8477],'ropf',[120163],'roplus',[10798],'rotimes',[10805],
        'RoundImplies',[10608],'rpar',[41],'rpargt',[10644],'rppolint',[10770],'rrarr',[8649],'Rrightarrow',[8667],
        'rsaquo',[8250],'Rscr',[8475],'rscr',[120007],'Rsh',[8625],'rsh',[8625],'rsqb',[93],
        'rsquo',[8217],'rsquor',[8217],'rthree',[8908],'rtimes',[8906],'rtri',[9657],'rtrie',[8885],
        'rtrif',[9656],'rtriltri',[10702],'RuleDelayed',[10740],'ruluhar',[10600],'rx',[8478],'Sacute',[346],
        'sacute',[347],'sbquo',[8218],'Sc',[10940],'sc',[8827],'scap',[10936],'Scaron',[352],
        'scaron',[353],'sccue',[8829],'scE',[10932],'sce',[10928],'Scedil',[350],'scedil',[351],
        'Scirc',[348],'scirc',[349],'scnap',[10938],'scnE',[10934],'scnsim',[8937],'scpolint',[10771],
        'scsim',[8831],'Scy',[1057],'scy',[1089],'sdot',[8901],'sdotb',[8865],'sdote',[10854],
        'searhk',[10533],'seArr',[8664],'searr',[8600],'searrow',[8600],'sect',[167],'semi',[59],
        'seswar',[10537],'setminus',[8726],'setmn',[8726],'sext',[10038],'Sfr',[120086],'sfr',[120112],
        'sfrown',[8994],'sharp',[9839],'SHCHcy',[1065],'shchcy',[1097],'SHcy',[1064],'shcy',[1096],
        'ShortDownArrow',[8595],'ShortLeftArrow',[8592],'shortmid',[8739],'shortparallel',[8741],'ShortRightArrow',[8594],'ShortUpArrow',[8593],
        'shy',[173],'Sigma',[931],'sigma',[963],'sigmaf',[962],'sigmav',[962],'sim',[8764],
        'simdot',[10858],'sime',[8771],'simeq',[8771],'simg',[10910],'simgE',[10912],'siml',[10909],
        'simlE',[10911],'simne',[8774],'simplus',[10788],'simrarr',[10610],'slarr',[8592],'SmallCircle',[8728],
        'smallsetminus',[8726],'smashp',[10803],'smeparsl',[10724],'smid',[8739],'smile',[8995],'smt',[10922],
        'smte',[10924],'smtes',[10924,65024],'SOFTcy',[1068],'softcy',[1100],'sol',[47],'solb',[10692],
        'solbar',[9023],'Sopf',[120138],'sopf',[120164],'spades',[9824],'spadesuit',[9824],'spar',[8741],
        'sqcap',[8851],'sqcaps',[8851,65024],'sqcup',[8852],'sqcups',[8852,65024],'Sqrt',[8730],'sqsub',[8847],
        'sqsube',[8849],'sqsubset',[8847],'sqsubseteq',[8849],'sqsup',[8848],'sqsupe',[8850],'sqsupset',[8848],
        'sqsupseteq',[8850],'squ',[9633],'Square',[9633],'square',[9633],'SquareIntersection',[8851],'SquareSubset',[8847],
        'SquareSubsetEqual',[8849],'SquareSuperset',[8848],'SquareSupersetEqual',[8850],'SquareUnion',[8852],'squarf',[9642],'squf',[9642],
        'srarr',[8594],'Sscr',[119982],'sscr',[120008],'ssetmn',[8726],'ssmile',[8995],'sstarf',[8902],
        'Star',[8902],'star',[9734],'starf',[9733],'straightepsilon',[1013],'straightphi',[981],'strns',[175],
        'Sub',[8912],'sub',[8834],'subdot',[10941],'subE',[10949],'sube',[8838],'subedot',[10947],
        'submult',[10945],'subnE',[10955],'subne',[8842],'subplus',[10943],'subrarr',[10617],'Subset',[8912],
        'subset',[8834],'subseteq',[8838],'subseteqq',[10949],'SubsetEqual',[8838],'subsetneq',[8842],'subsetneqq',[10955],
        'subsim',[10951],'subsub',[10965],'subsup',[10963],'succ',[8827],'succapprox',[10936],'succcurlyeq',[8829],
        'Succeeds',[8827],'SucceedsEqual',[10928],'SucceedsSlantEqual',[8829],'SucceedsTilde',[8831],'succeq',[10928],'succnapprox',[10938],
        'succneqq',[10934],'succnsim',[8937],'succsim',[8831],'SuchThat',[8715],'Sum',[8721],'sum',[8721],
        'sung',[9834],'Sup',[8913],'sup',[8835],'sup1',[185],'sup2',[178],'sup3',[179],
        'supdot',[10942],'supdsub',[10968],'supE',[10950],'supe',[8839],'supedot',[10948],'Superset',[8835],
        'SupersetEqual',[8839],'suphsol',[10185],'suphsub',[10967],'suplarr',[10619],'supmult',[10946],'supnE',[10956],
        'supne',[8843],'supplus',[10944],'Supset',[8913],'supset',[8835],'supseteq',[8839],'supseteqq',[10950],
        'supsetneq',[8843],'supsetneqq',[10956],'supsim',[10952],'supsub',[10964],'supsup',[10966],'swarhk',[10534],
        'swArr',[8665],'swarr',[8601],'swarrow',[8601],'swnwar',[10538],'szlig',[223],'Tab',[9],
        'target',[8982],'Tau',[932],'tau',[964],'tbrk',[9140],'Tcaron',[356],'tcaron',[357],
        'Tcedil',[354],'tcedil',[355],'Tcy',[1058],'tcy',[1090],'tdot',[8411],'telrec',[8981],
        'Tfr',[120087],'tfr',[120113],'there4',[8756],'Therefore',[8756],'therefore',[8756],'Theta',[920],
        'theta',[952],'thetasym',[977],'thetav',[977],'thickapprox',[8776],'thicksim',[8764],'ThickSpace',[8287,8202],
        'thinsp',[8201],'ThinSpace',[8201],'thkap',[8776],'thksim',[8764],'THORN',[222],'thorn',[254],
        'Tilde',[8764],'tilde',[732],'TildeEqual',[8771],'TildeFullEqual',[8773],'TildeTilde',[8776],'times',[215],
        'timesb',[8864],'timesbar',[10801],'timesd',[10800],'tint',[8749],'toea',[10536],'top',[8868],
        'topbot',[9014],'topcir',[10993],'Topf',[120139],'topf',[120165],'topfork',[10970],'tosa',[10537],
        'tprime',[8244],'TRADE',[8482],'trade',[8482],'triangle',[9653],'triangledown',[9663],'triangleleft',[9667],
        'trianglelefteq',[8884],'triangleq',[8796],'triangleright',[9657],'trianglerighteq',[8885],'tridot',[9708],'trie',[8796],
        'triminus',[10810],'TripleDot',[8411],'triplus',[10809],'trisb',[10701],'tritime',[10811],'trpezium',[9186],
        'Tscr',[119983],'tscr',[120009],'TScy',[1062],'tscy',[1094],'TSHcy',[1035],'tshcy',[1115],
        'Tstrok',[358],'tstrok',[359],'twixt',[8812],'twoheadleftarrow',[8606],'twoheadrightarrow',[8608],'Uacute',[218],
        'uacute',[250],'Uarr',[8607],'uArr',[8657],'uarr',[8593],'Uarrocir',[10569],'Ubrcy',[1038],
        'ubrcy',[1118],'Ubreve',[364],'ubreve',[365],'Ucirc',[219],'ucirc',[251],'Ucy',[1059],
        'ucy',[1091],'udarr',[8645],'Udblac',[368],'udblac',[369],'udhar',[10606],'ufisht',[10622],
        'Ufr',[120088],'ufr',[120114],'Ugrave',[217],'ugrave',[249],'uHar',[10595],'uharl',[8639],
        'uharr',[8638],'uhblk',[9600],'ulcorn',[8988],'ulcorner',[8988],'ulcrop',[8975],'ultri',[9720],
        'Umacr',[362],'umacr',[363],'uml',[168],'UnderBar',[95],'UnderBrace',[9183],'UnderBracket',[9141],
        'UnderParenthesis',[9181],'Union',[8899],'UnionPlus',[8846],'Uogon',[370],'uogon',[371],'Uopf',[120140],
        'uopf',[120166],'UpArrow',[8593],'Uparrow',[8657],'uparrow',[8593],'UpArrowBar',[10514],'UpArrowDownArrow',[8645],
        'UpDownArrow',[8597],'Updownarrow',[8661],'updownarrow',[8597],'UpEquilibrium',[10606],'upharpoonleft',[8639],'upharpoonright',[8638],
        'uplus',[8846],'UpperLeftArrow',[8598],'UpperRightArrow',[8599],'Upsi',[978],'upsi',[965],'upsih',[978],
        'Upsilon',[933],'upsilon',[965],'UpTee',[8869],'UpTeeArrow',[8613],'upuparrows',[8648],'urcorn',[8989],
        'urcorner',[8989],'urcrop',[8974],'Uring',[366],'uring',[367],'urtri',[9721],'Uscr',[119984],
        'uscr',[120010],'utdot',[8944],'Utilde',[360],'utilde',[361],'utri',[9653],'utrif',[9652],
        'uuarr',[8648],'Uuml',[220],'uuml',[252],'uwangle',[10663],'vangrt',[10652],'varepsilon',[1013],
        'varkappa',[1008],'varnothing',[8709],'varphi',[981],'varpi',[982],'varpropto',[8733],'vArr',[8661],
        'varr',[8597],'varrho',[1009],'varsigma',[962],'varsubsetneq',[8842,65024],'varsubsetneqq',[10955,65024],'varsupsetneq',[8843,65024],
        'varsupsetneqq',[10956,65024],'vartheta',[977],'vartriangleleft',[8882],'vartriangleright',[8883],'Vbar',[10987],'vBar',[10984],
        'vBarv',[10985],'Vcy',[1042],'vcy',[1074],'VDash',[8875],'Vdash',[8873],'vDash',[8872],
        'vdash',[8866],'Vdashl',[10982],'Vee',[8897],'vee',[8744],'veebar',[8891],'veeeq',[8794],
        'vellip',[8942],'Verbar',[8214],'verbar',[124],'Vert',[8214],'vert',[124],'VerticalBar',[8739],
        'VerticalLine',[124],'VerticalSeparator',[10072],'VerticalTilde',[8768],'VeryThinSpace',[8202],'Vfr',[120089],'vfr',[120115],
        'vltri',[8882],'vnsub',[8834,8402],'vnsup',[8835,8402],'Vopf',[120141],'vopf',[120167],'vprop',[8733],
        'vrtri',[8883],'Vscr',[119985],'vscr',[120011],'vsubnE',[10955,65024],'vsubne',[8842,65024],'vsupnE',[10956,65024],
        'vsupne',[8843,65024],'Vvdash',[8874],'vzigzag',[10650],'Wcirc',[372],'wcirc',[373],'wedbar',[10847],
        'Wedge',[8896],'wedge',[8743],'wedgeq',[8793],'weierp',[8472],'Wfr',[120090],'wfr',[120116],
        'Wopf',[120142],'wopf',[120168],'wp',[8472],'wr',[8768],'wreath',[8768],'Wscr',[119986],
        'wscr',[120012],'xcap',[8898],'xcirc',[9711],'xcup',[8899],'xdtri',[9661],'Xfr',[120091],
        'xfr',[120117],'xhArr',[10234],'xharr',[10231],'Xi',[926],'xi',[958],'xlArr',[10232],
        'xlarr',[10229],'xmap',[10236],'xnis',[8955],'xodot',[10752],'Xopf',[120143],'xopf',[120169],
        'xoplus',[10753],'xotime',[10754],'xrArr',[10233],'xrarr',[10230],'Xscr',[119987],'xscr',[120013],
        'xsqcup',[10758],'xuplus',[10756],'xutri',[9651],'xvee',[8897],'xwedge',[8896],'Yacute',[221],
        'yacute',[253],'YAcy',[1071],'yacy',[1103],'Ycirc',[374],'ycirc',[375],'Ycy',[1067],
        'ycy',[1099],'yen',[165],'Yfr',[120092],'yfr',[120118],'YIcy',[1031],'yicy',[1111],
        'Yopf',[120144],'yopf',[120170],'Yscr',[119988],'yscr',[120014],'YUcy',[1070],'yucy',[1102],
        'Yuml',[376],'yuml',[255],'Zacute',[377],'zacute',[378],'Zcaron',[381],'zcaron',[382],
        'Zcy',[1047],'zcy',[1079],'Zdot',[379],'zdot',[380],'zeetrf',[8488],'ZeroWidthSpace',[8203],
        'Zeta',[918],'zeta',[950],'Zfr',[8488],'zfr',[120119],'ZHcy',[1046],'zhcy',[1078],
        'zigrarr',[8669],'Zopf',[8484],'zopf',[120171],'Zscr',[119989],'zscr',[120015],'zwj',[8205],
        'zwnj',[8204]
    );
    our sub str_from_entity($e) {
        my $r := '';
        for %entities{$e} // [] {
            $r := $r ~ nqp::chr($_);
        }
        $r
    }
#?endif
} # end class Perl6::Pod

# vim: expandtab sw=4
