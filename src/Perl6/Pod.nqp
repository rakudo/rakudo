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
        $src.panic("'$src' is not a valid number")
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

} # end class Perl6::Pod

# vim: expandtab sw=4
