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

    # all-lowercase HTML5 character entities, derived from
    # https://github.com/w3c/html/blob/master/entities.json
    my %entities;
    # NQP doesn't support assignment of entire hashes :(
    # so we have to iterate
    for 'aacute',[225],'aacute',[225],'abreve',[259],'ac',[8766],'acd',[8767],'acirc',[226],
        'acirc',[226],'acute',[180],'acute',[180],'acy',[1072],'aelig',[230],'aelig',[230],
        'af',[8289],'afr',[120094],'agrave',[224],'agrave',[224],'alefsym',[8501],'aleph',[8501],
        'alpha',[945],'amacr',[257],'amalg',[10815],'amp',[38],'amp',[38],'andand',[10837],
        'and',[8743],'andd',[10844],'andslope',[10840],'andv',[10842],'ang',[8736],'ange',[10660],
        'angle',[8736],'angmsdaa',[10664],'angmsdab',[10665],'angmsdac',[10666],'angmsdad',[10667],'angmsdae',[10668],
        'angmsdaf',[10669],'angmsdag',[10670],'angmsdah',[10671],'angmsd',[8737],'angrt',[8735],'angrtvb',[8894],
        'angrtvbd',[10653],'angsph',[8738],'angst',[197],'angzarr',[9084],'aogon',[261],'aopf',[120146],
        'apacir',[10863],'ap',[8776],'ape',[8778],'apid',[8779],'apos',[39],'approx',[8776],
        'approxeq',[8778],'aring',[229],'aring',[229],'ascr',[119990],'ast',[42],'asymp',[8776],
        'asympeq',[8781],'atilde',[227],'atilde',[227],'auml',[228],'auml',[228],'awconint',[8755],
        'awint',[10769],'backcong',[8780],'backepsilon',[1014],'backprime',[8245],'backsim',[8765],'backsimeq',[8909],
        'barvee',[8893],'barwed',[8965],'barwedge',[8965],'bbrk',[9141],'bbrktbrk',[9142],'bcong',[8780],
        'bcy',[1073],'bdquo',[8222],'becaus',[8757],'because',[8757],'bemptyv',[10672],'bepsi',[1014],
        'bernou',[8492],'beta',[946],'beth',[8502],'between',[8812],'bfr',[120095],'bigcap',[8898],
        'bigcirc',[9711],'bigcup',[8899],'bigodot',[10752],'bigoplus',[10753],'bigotimes',[10754],'bigsqcup',[10758],
        'bigstar',[9733],'bigtriangledown',[9661],'bigtriangleup',[9651],'biguplus',[10756],'bigvee',[8897],'bigwedge',[8896],
        'bkarow',[10509],'blacklozenge',[10731],'blacksquare',[9642],'blacktriangle',[9652],'blacktriangledown',[9662],'blacktriangleleft',[9666],
        'blacktriangleright',[9656],'blank',[9251],'block',[9608],'bne',[61,8421],'bnequiv',[8801,8421],'bnot',[8976],
        'bopf',[120147],'bot',[8869],'bottom',[8869],'bowtie',[8904],'boxbox',[10697],'boxdl',[9488],
        'boxdr',[9484],'boxh',[9472],'boxhd',[9516],'boxhu',[9524],'boxminus',[8863],'boxplus',[8862],
        'boxtimes',[8864],'boxul',[9496],'boxur',[9492],'boxv',[9474],'boxvh',[9532],'boxvl',[9508],
        'boxvr',[9500],'bprime',[8245],'breve',[728],'brvbar',[166],'brvbar',[166],'bscr',[119991],
        'bsemi',[8271],'bsim',[8765],'bsime',[8909],'bsolb',[10693],'bsol',[92],'bsolhsub',[10184],
        'bull',[8226],'bullet',[8226],'bump',[8782],'bumpe',[8783],'bumpeq',[8783],'cacute',[263],
        'capand',[10820],'capbrcup',[10825],'capcap',[10827],'cap',[8745],'capcup',[10823],'capdot',[10816],
        'caps',[8745,65024],'caret',[8257],'caron',[711],'ccaps',[10829],'ccaron',[269],'ccedil',[231],
        'ccedil',[231],'ccirc',[265],'ccups',[10828],'ccupssm',[10832],'cdot',[267],'cedil',[184],
        'cedil',[184],'cemptyv',[10674],'cent',[162],'cent',[162],'centerdot',[183],'cfr',[120096],
        'chcy',[1095],'check',[10003],'checkmark',[10003],'chi',[967],'circ',[710],'circeq',[8791],
        'circlearrowleft',[8634],'circlearrowright',[8635],'circledast',[8859],'circledcirc',[8858],'circleddash',[8861],'cir',[9675],
        'cire',[8791],'cirfnint',[10768],'cirmid',[10991],'cirscir',[10690],'clubs',[9827],'clubsuit',[9827],
        'colon',[58],'colone',[8788],'coloneq',[8788],'comma',[44],'commat',[64],'comp',[8705],
        'compfn',[8728],'complement',[8705],'complexes',[8450],'cong',[8773],'congdot',[10861],'conint',[8750],
        'copf',[120148],'coprod',[8720],'copy',[169],'copy',[169],'copysr',[8471],'crarr',[8629],
        'cross',[10007],'cscr',[119992],'csub',[10959],'csube',[10961],'csup',[10960],'csupe',[10962],
        'ctdot',[8943],'cudarrl',[10552],'cudarrr',[10549],'cuepr',[8926],'cuesc',[8927],'cularr',[8630],
        'cularrp',[10557],'cupbrcap',[10824],'cupcap',[10822],'cup',[8746],'cupcup',[10826],'cupdot',[8845],
        'cupor',[10821],'cups',[8746,65024],'curarr',[8631],'curarrm',[10556],'curlyeqprec',[8926],'curlyeqsucc',[8927],
        'curlyvee',[8910],'curlywedge',[8911],'curren',[164],'curren',[164],'curvearrowleft',[8630],'curvearrowright',[8631],
        'cuvee',[8910],'cuwed',[8911],'cwconint',[8754],'cwint',[8753],'cylcty',[9005],'dagger',[8224],
        'daleth',[8504],'darr',[8595],'dash',[8208],'dashv',[8867],'dbkarow',[10511],'dblac',[733],
        'dcaron',[271],'dcy',[1076],'ddagger',[8225],'ddarr',[8650],'dd',[8518],'ddotseq',[10871],
        'deg',[176],'deg',[176],'delta',[948],'demptyv',[10673],'dfisht',[10623],'dfr',[120097],
        'dharl',[8643],'dharr',[8642],'diam',[8900],'diamond',[8900],'diamondsuit',[9830],'diams',[9830],
        'die',[168],'digamma',[989],'disin',[8946],'div',[247],'divide',[247],'divide',[247],
        'divideontimes',[8903],'divonx',[8903],'djcy',[1106],'dlcorn',[8990],'dlcrop',[8973],'dollar',[36],
        'dopf',[120149],'dot',[729],'doteq',[8784],'doteqdot',[8785],'dotminus',[8760],'dotplus',[8724],
        'dotsquare',[8865],'doublebarwedge',[8966],'downarrow',[8595],'downdownarrows',[8650],'downharpoonleft',[8643],'downharpoonright',[8642],
        'drbkarow',[10512],'drcorn',[8991],'drcrop',[8972],'dscr',[119993],'dscy',[1109],'dsol',[10742],
        'dstrok',[273],'dtdot',[8945],'dtri',[9663],'dtrif',[9662],'duarr',[8693],'duhar',[10607],
        'dwangle',[10662],'dzcy',[1119],'dzigrarr',[10239],'eacute',[233],'eacute',[233],'easter',[10862],
        'ecaron',[283],'ecirc',[234],'ecirc',[234],'ecir',[8790],'ecolon',[8789],'ecy',[1101],
        'edot',[279],'ee',[8519],'efr',[120098],'eg',[10906],'egrave',[232],'egrave',[232],
        'egs',[10902],'egsdot',[10904],'el',[10905],'elinters',[9191],'ell',[8467],'els',[10901],
        'elsdot',[10903],'emacr',[275],'empty',[8709],'emptyset',[8709],'emptyv',[8709],'emsp',[8195],
        'eng',[331],'ensp',[8194],'eogon',[281],'eopf',[120150],'epar',[8917],'eparsl',[10723],
        'eplus',[10865],'epsi',[949],'epsilon',[949],'epsiv',[1013],'eqcirc',[8790],'eqcolon',[8789],
        'eqsim',[8770],'eqslantgtr',[10902],'eqslantless',[10901],'equals',[61],'equest',[8799],'equiv',[8801],
        'eqvparsl',[10725],'erarr',[10609],'escr',[8495],'esdot',[8784],'esim',[8770],'eta',[951],
        'eth',[240],'eth',[240],'euml',[235],'euml',[235],'euro',[8364],'excl',[33],
        'exist',[8707],'expectation',[8496],'exponentiale',[8519],'fallingdotseq',[8786],'fcy',[1092],'female',[9792],
        'ffilig',[64259],'fflig',[64256],'ffllig',[64260],'ffr',[120099],'filig',[64257],'fjlig',[102,106],
        'flat',[9837],'fllig',[64258],'fltns',[9649],'fnof',[402],'fopf',[120151],'forall',[8704],
        'fork',[8916],'forkv',[10969],'fpartint',[10765],'frasl',[8260],'frown',[8994],'fscr',[119995],
        'gacute',[501],'gamma',[947],'gammad',[989],'gap',[10886],'gbreve',[287],'gcirc',[285],
        'gcy',[1075],'gdot',[289],'ge',[8805],'gel',[8923],'geq',[8805],'geqq',[8807],
        'geqslant',[10878],'gescc',[10921],'ges',[10878],'gesdot',[10880],'gesdoto',[10882],'gesdotol',[10884],
        'gesl',[8923,65024],'gesles',[10900],'gfr',[120100],'gg',[8811],'ggg',[8921],'gimel',[8503],
        'gjcy',[1107],'gla',[10917],'gl',[8823],'glj',[10916],'gnap',[10890],'gnapprox',[10890],
        'gne',[10888],'gneq',[10888],'gneqq',[8809],'gnsim',[8935],'gopf',[120152],'grave',[96],
        'gscr',[8458],'gsim',[8819],'gsime',[10894],'gsiml',[10896],'gtcc',[10919],'gtcir',[10874],
        'gt',[62],'gt',[62],'gtdot',[8919],'gtquest',[10876],'gtrapprox',[10886],'gtrarr',[10616],
        'gtrdot',[8919],'gtreqless',[8923],'gtreqqless',[10892],'gtrless',[8823],'gtrsim',[8819],'gvertneqq',[8809,65024],
        'hairsp',[8202],'half',[189],'hamilt',[8459],'hardcy',[1098],'harrcir',[10568],'harr',[8596],
        'harrw',[8621],'hbar',[8463],'hcirc',[293],'hearts',[9829],'heartsuit',[9829],'hellip',[8230],
        'hercon',[8889],'hfr',[120101],'hksearow',[10533],'hkswarow',[10534],'hoarr',[8703],'homtht',[8763],
        'hookleftarrow',[8617],'hookrightarrow',[8618],'hopf',[120153],'horbar',[8213],'hscr',[119997],'hslash',[8463],
        'hstrok',[295],'hybull',[8259],'hyphen',[8208],'iacute',[237],'iacute',[237],'ic',[8291],
        'icirc',[238],'icirc',[238],'icy',[1080],'iecy',[1077],'iexcl',[161],'iexcl',[161],
        'iff',[8660],'ifr',[120102],'igrave',[236],'igrave',[236],'ii',[8520],'iiiint',[10764],
        'iiint',[8749],'iinfin',[10716],'iiota',[8489],'ijlig',[307],'imacr',[299],'image',[8465],
        'imagline',[8464],'imagpart',[8465],'imath',[305],'imof',[8887],'imped',[437],'incare',[8453],
        'in',[8712],'infin',[8734],'infintie',[10717],'inodot',[305],'intcal',[8890],'int',[8747],
        'integers',[8484],'intercal',[8890],'intlarhk',[10775],'intprod',[10812],'iocy',[1105],'iogon',[303],
        'iopf',[120154],'iota',[953],'iprod',[10812],'iquest',[191],'iquest',[191],'iscr',[119998],
        'isin',[8712],'isindot',[8949],'isins',[8948],'isinsv',[8947],'isinv',[8712],'it',[8290],
        'itilde',[297],'iukcy',[1110],'iuml',[239],'iuml',[239],'jcirc',[309],'jcy',[1081],
        'jfr',[120103],'jmath',[567],'jopf',[120155],'jscr',[119999],'jsercy',[1112],'jukcy',[1108],
        'kappa',[954],'kappav',[1008],'kcedil',[311],'kcy',[1082],'kfr',[120104],'kgreen',[312],
        'khcy',[1093],'kjcy',[1116],'kopf',[120156],'kscr',[120000],'lacute',[314],'laemptyv',[10676],
        'lagran',[8466],'lambda',[955],'lang',[10216],'langd',[10641],'langle',[10216],'lap',[10885],
        'laquo',[171],'laquo',[171],'larrb',[8676],'larrbfs',[10527],'larr',[8592],'larrfs',[10525],
        'larrhk',[8617],'larrlp',[8619],'larrpl',[10553],'larrsim',[10611],'larrtl',[8610],'latail',[10521],
        'lat',[10923],'late',[10925],'lates',[10925,65024],'lbarr',[10508],'lbbrk',[10098],'lbrace',[123],
        'lbrack',[91],'lbrke',[10635],'lbrksld',[10639],'lbrkslu',[10637],'lcaron',[318],'lcedil',[316],
        'lceil',[8968],'lcub',[123],'lcy',[1083],'ldca',[10550],'ldquo',[8220],'ldquor',[8222],
        'ldrdhar',[10599],'ldrushar',[10571],'ldsh',[8626],'le',[8804],'leftarrow',[8592],'leftarrowtail',[8610],
        'leftharpoondown',[8637],'leftharpoonup',[8636],'leftleftarrows',[8647],'leftrightarrow',[8596],'leftrightarrows',[8646],'leftrightharpoons',[8651],
        'leftrightsquigarrow',[8621],'leftthreetimes',[8907],'leg',[8922],'leq',[8804],'leqq',[8806],'leqslant',[10877],
        'lescc',[10920],'les',[10877],'lesdot',[10879],'lesdoto',[10881],'lesdotor',[10883],'lesg',[8922,65024],
        'lesges',[10899],'lessapprox',[10885],'lessdot',[8918],'lesseqgtr',[8922],'lesseqqgtr',[10891],'lessgtr',[8822],
        'lesssim',[8818],'lfisht',[10620],'lfloor',[8970],'lfr',[120105],'lg',[8822],'lhard',[8637],
        'lharu',[8636],'lharul',[10602],'lhblk',[9604],'ljcy',[1113],'llarr',[8647],'ll',[8810],
        'llcorner',[8990],'llhard',[10603],'lltri',[9722],'lmidot',[320],'lmoustache',[9136],'lmoust',[9136],
        'lnap',[10889],'lnapprox',[10889],'lne',[10887],'lneq',[10887],'lneqq',[8808],'lnsim',[8934],
        'loang',[10220],'loarr',[8701],'lobrk',[10214],'longleftarrow',[10229],'longleftrightarrow',[10231],'longmapsto',[10236],
        'longrightarrow',[10230],'looparrowleft',[8619],'looparrowright',[8620],'lopar',[10629],'lopf',[120157],'loplus',[10797],
        'lotimes',[10804],'lowast',[8727],'lowbar',[95],'loz',[9674],'lozenge',[9674],'lozf',[10731],
        'lpar',[40],'lparlt',[10643],'lrarr',[8646],'lrcorner',[8991],'lrhar',[8651],'lrhard',[10605],
        'lrm',[8206],'lrtri',[8895],'lsaquo',[8249],'lscr',[120001],'lsh',[8624],'lsim',[8818],
        'lsime',[10893],'lsimg',[10895],'lsqb',[91],'lsquo',[8216],'lsquor',[8218],'lstrok',[322],
        'ltcc',[10918],'ltcir',[10873],'lt',[60],'lt',[60],'ltdot',[8918],'lthree',[8907],
        'ltimes',[8905],'ltlarr',[10614],'ltquest',[10875],'ltri',[9667],'ltrie',[8884],'ltrif',[9666],
        'lurdshar',[10570],'luruhar',[10598],'lvertneqq',[8808,65024],'macr',[175],'macr',[175],'male',[9794],
        'malt',[10016],'maltese',[10016],'map',[8614],'mapsto',[8614],'mapstodown',[8615],'mapstoleft',[8612],
        'mapstoup',[8613],'marker',[9646],'mcomma',[10793],'mcy',[1084],'mdash',[8212],'measuredangle',[8737],
        'mfr',[120106],'mho',[8487],'micro',[181],'micro',[181],'midast',[42],'midcir',[10992],
        'mid',[8739],'middot',[183],'middot',[183],'minusb',[8863],'minus',[8722],'minusd',[8760],
        'minusdu',[10794],'mlcp',[10971],'mldr',[8230],'mnplus',[8723],'models',[8871],'mopf',[120158],
        'mp',[8723],'mscr',[120002],'mstpos',[8766],'mu',[956],'multimap',[8888],'mumap',[8888],
        'nabla',[8711],'nacute',[324],'nang',[8736,8402],'nap',[8777],'napid',[8779,824],'napos',[329],
        'napprox',[8777],'natural',[9838],'naturals',[8469],'natur',[9838],'nbsp',[160],'nbsp',[160],
        'nbump',[8782,824],'nbumpe',[8783,824],'ncap',[10819],'ncaron',[328],'ncedil',[326],'ncong',[8775],
        'ncongdot',[10861,824],'ncup',[10818],'ncy',[1085],'ndash',[8211],'nearhk',[10532],'nearr',[8599],
        'nearrow',[8599],'ne',[8800],'nedot',[8784,824],'nequiv',[8802],'nesear',[10536],'nesim',[8770,824],
        'nexist',[8708],'nexists',[8708],'nfr',[120107],'nge',[8817],'ngeq',[8817],'ngeqq',[8807,824],
        'ngeqslant',[10878,824],'nges',[10878,824],'ngsim',[8821],'ngt',[8815],'ngtr',[8815],'nharr',[8622],
        'nhpar',[10994],'ni',[8715],'nis',[8956],'nisd',[8954],'niv',[8715],'njcy',[1114],
        'nlarr',[8602],'nldr',[8229],'nle',[8816],'nleftarrow',[8602],'nleftrightarrow',[8622],'nleq',[8816],
        'nleqq',[8806,824],'nleqslant',[10877,824],'nles',[10877,824],'nless',[8814],'nlsim',[8820],'nlt',[8814],
        'nltri',[8938],'nltrie',[8940],'nmid',[8740],'nopf',[120159],'not',[172],'not',[172],
        'notin',[8713],'notindot',[8949,824],'notinva',[8713],'notinvb',[8951],'notinvc',[8950],'notni',[8716],
        'notniva',[8716],'notnivb',[8958],'notnivc',[8957],'nparallel',[8742],'npar',[8742],'nparsl',[11005,8421],
        'npart',[8706,824],'npolint',[10772],'npr',[8832],'nprcue',[8928],'nprec',[8832],'npreceq',[10927,824],
        'npre',[10927,824],'nrarrc',[10547,824],'nrarr',[8603],'nrarrw',[8605,824],'nrightarrow',[8603],'nrtri',[8939],
        'nrtrie',[8941],'nsc',[8833],'nsccue',[8929],'nsce',[10928,824],'nscr',[120003],'nshortmid',[8740],
        'nshortparallel',[8742],'nsim',[8769],'nsime',[8772],'nsimeq',[8772],'nsmid',[8740],'nspar',[8742],
        'nsqsube',[8930],'nsqsupe',[8931],'nsub',[8836],'nsube',[8840],'nsubset',[8834,8402],'nsubseteq',[8840],
        'nsubseteqq',[10949,824],'nsucc',[8833],'nsucceq',[10928,824],'nsup',[8837],'nsupe',[8841],'nsupset',[8835,8402],
        'nsupseteq',[8841],'nsupseteqq',[10950,824],'ntgl',[8825],'ntilde',[241],'ntilde',[241],'ntlg',[8824],
        'ntriangleleft',[8938],'ntrianglelefteq',[8940],'ntriangleright',[8939],'ntrianglerighteq',[8941],'nu',[957],'num',[35],
        'numero',[8470],'numsp',[8199],'nvap',[8781,8402],'nvdash',[8876],'nvge',[8805,8402],'nvgt',[62,8402],
        'nvinfin',[10718],'nvle',[8804,8402],'nvlt',[60,8402],'nvltrie',[8884,8402],'nvrtrie',[8885,8402],'nvsim',[8764,8402],
        'nwarhk',[10531],'nwarr',[8598],'nwarrow',[8598],'nwnear',[10535],'oacute',[243],'oacute',[243],
        'oast',[8859],'ocirc',[244],'ocirc',[244],'ocir',[8858],'ocy',[1086],'odash',[8861],
        'odblac',[337],'odiv',[10808],'odot',[8857],'odsold',[10684],'oelig',[339],'ofcir',[10687],
        'ofr',[120108],'ogon',[731],'ograve',[242],'ograve',[242],'ogt',[10689],'ohbar',[10677],
        'ohm',[937],'oint',[8750],'olarr',[8634],'olcir',[10686],'olcross',[10683],'oline',[8254],
        'olt',[10688],'omacr',[333],'omega',[969],'omicron',[959],'omid',[10678],'ominus',[8854],
        'oopf',[120160],'opar',[10679],'operp',[10681],'oplus',[8853],'orarr',[8635],'or',[8744],
        'ord',[10845],'order',[8500],'orderof',[8500],'ordf',[170],'ordf',[170],'ordm',[186],
        'ordm',[186],'origof',[8886],'oror',[10838],'orslope',[10839],'orv',[10843],'oscr',[8500],
        'oslash',[248],'oslash',[248],'osol',[8856],'otilde',[245],'otilde',[245],'otimesas',[10806],
        'otimes',[8855],'ouml',[246],'ouml',[246],'ovbar',[9021],'para',[182],'para',[182],
        'parallel',[8741],'par',[8741],'parsim',[10995],'parsl',[11005],'part',[8706],'pcy',[1087],
        'percnt',[37],'period',[46],'permil',[8240],'perp',[8869],'pertenk',[8241],'pfr',[120109],
        'phi',[966],'phiv',[981],'phmmat',[8499],'phone',[9742],'pi',[960],'pitchfork',[8916],
        'piv',[982],'planck',[8463],'planckh',[8462],'plankv',[8463],'plusacir',[10787],'plusb',[8862],
        'pluscir',[10786],'plus',[43],'plusdo',[8724],'plusdu',[10789],'pluse',[10866],'plusmn',[177],
        'plusmn',[177],'plussim',[10790],'plustwo',[10791],'pm',[177],'pointint',[10773],'popf',[120161],
        'pound',[163],'pound',[163],'prap',[10935],'pr',[8826],'prcue',[8828],'precapprox',[10935],
        'prec',[8826],'preccurlyeq',[8828],'preceq',[10927],'precnapprox',[10937],'precneqq',[10933],'precnsim',[8936],
        'pre',[10927],'precsim',[8830],'prime',[8242],'primes',[8473],'prnap',[10937],'prnsim',[8936],
        'prod',[8719],'profalar',[9006],'profline',[8978],'profsurf',[8979],'prop',[8733],'propto',[8733],
        'prsim',[8830],'prurel',[8880],'pscr',[120005],'psi',[968],'puncsp',[8200],'qfr',[120110],
        'qint',[10764],'qopf',[120162],'qprime',[8279],'qscr',[120006],'quaternions',[8461],'quatint',[10774],
        'quest',[63],'questeq',[8799],'quot',[34],'quot',[34],'race',[8765,817],'racute',[341],
        'radic',[8730],'raemptyv',[10675],'rang',[10217],'rangd',[10642],'range',[10661],'rangle',[10217],
        'raquo',[187],'raquo',[187],'rarrap',[10613],'rarrb',[8677],'rarrbfs',[10528],'rarrc',[10547],
        'rarr',[8594],'rarrfs',[10526],'rarrhk',[8618],'rarrlp',[8620],'rarrpl',[10565],'rarrsim',[10612],
        'rarrtl',[8611],'rarrw',[8605],'ratail',[10522],'ratio',[8758],'rationals',[8474],'rbarr',[10509],
        'rbbrk',[10099],'rbrace',[125],'rbrack',[93],'rbrke',[10636],'rbrksld',[10638],'rbrkslu',[10640],
        'rcaron',[345],'rcedil',[343],'rceil',[8969],'rcub',[125],'rcy',[1088],'rdca',[10551],
        'rdldhar',[10601],'rdquo',[8221],'rdquor',[8221],'rdsh',[8627],'real',[8476],'realine',[8475],
        'realpart',[8476],'reals',[8477],'rect',[9645],'reg',[174],'reg',[174],'rfisht',[10621],
        'rfloor',[8971],'rfr',[120111],'rhard',[8641],'rharu',[8640],'rharul',[10604],'rho',[961],
        'rhov',[1009],'rightarrow',[8594],'rightarrowtail',[8611],'rightharpoondown',[8641],'rightharpoonup',[8640],'rightleftarrows',[8644],
        'rightleftharpoons',[8652],'rightrightarrows',[8649],'rightsquigarrow',[8605],'rightthreetimes',[8908],'ring',[730],'risingdotseq',[8787],
        'rlarr',[8644],'rlhar',[8652],'rlm',[8207],'rmoustache',[9137],'rmoust',[9137],'rnmid',[10990],
        'roang',[10221],'roarr',[8702],'robrk',[10215],'ropar',[10630],'ropf',[120163],'roplus',[10798],
        'rotimes',[10805],'rpar',[41],'rpargt',[10644],'rppolint',[10770],'rrarr',[8649],'rsaquo',[8250],
        'rscr',[120007],'rsh',[8625],'rsqb',[93],'rsquo',[8217],'rsquor',[8217],'rthree',[8908],
        'rtimes',[8906],'rtri',[9657],'rtrie',[8885],'rtrif',[9656],'rtriltri',[10702],'ruluhar',[10600],
        'rx',[8478],'sacute',[347],'sbquo',[8218],'scap',[10936],'scaron',[353],'sc',[8827],
        'sccue',[8829],'sce',[10928],'scedil',[351],'scirc',[349],'scnap',[10938],'scnsim',[8937],
        'scpolint',[10771],'scsim',[8831],'scy',[1089],'sdotb',[8865],'sdot',[8901],'sdote',[10854],
        'searhk',[10533],'searr',[8600],'searrow',[8600],'sect',[167],'sect',[167],'semi',[59],
        'seswar',[10537],'setminus',[8726],'setmn',[8726],'sext',[10038],'sfr',[120112],'sfrown',[8994],
        'sharp',[9839],'shchcy',[1097],'shcy',[1096],'shortmid',[8739],'shortparallel',[8741],'shy',[173],
        'shy',[173],'sigma',[963],'sigmaf',[962],'sigmav',[962],'sim',[8764],'simdot',[10858],
        'sime',[8771],'simeq',[8771],'simg',[10910],'siml',[10909],'simne',[8774],'simplus',[10788],
        'simrarr',[10610],'slarr',[8592],'smallsetminus',[8726],'smashp',[10803],'smeparsl',[10724],'smid',[8739],
        'smile',[8995],'smt',[10922],'smte',[10924],'smtes',[10924,65024],'softcy',[1100],'solbar',[9023],
        'solb',[10692],'sol',[47],'sopf',[120164],'spades',[9824],'spadesuit',[9824],'spar',[8741],
        'sqcap',[8851],'sqcaps',[8851,65024],'sqcup',[8852],'sqcups',[8852,65024],'sqsub',[8847],'sqsube',[8849],
        'sqsubset',[8847],'sqsubseteq',[8849],'sqsup',[8848],'sqsupe',[8850],'sqsupset',[8848],'sqsupseteq',[8850],
        'square',[9633],'squarf',[9642],'squ',[9633],'squf',[9642],'srarr',[8594],'sscr',[120008],
        'ssetmn',[8726],'ssmile',[8995],'sstarf',[8902],'star',[9734],'starf',[9733],'straightepsilon',[1013],
        'straightphi',[981],'strns',[175],'sub',[8834],'subdot',[10941],'sube',[8838],'subedot',[10947],
        'submult',[10945],'subne',[8842],'subplus',[10943],'subrarr',[10617],'subset',[8834],'subseteq',[8838],
        'subseteqq',[10949],'subsetneq',[8842],'subsetneqq',[10955],'subsim',[10951],'subsub',[10965],'subsup',[10963],
        'succapprox',[10936],'succ',[8827],'succcurlyeq',[8829],'succeq',[10928],'succnapprox',[10938],'succneqq',[10934],
        'succnsim',[8937],'succsim',[8831],'sum',[8721],'sung',[9834],'sup',[8835],'supdot',[10942],
        'supdsub',[10968],'supe',[8839],'supedot',[10948],'suphsol',[10185],'suphsub',[10967],'suplarr',[10619],
        'supmult',[10946],'supne',[8843],'supplus',[10944],'supset',[8835],'supseteq',[8839],'supseteqq',[10950],
        'supsetneq',[8843],'supsetneqq',[10956],'supsim',[10952],'supsub',[10964],'supsup',[10966],'swarhk',[10534],
        'swarr',[8601],'swarrow',[8601],'swnwar',[10538],'szlig',[223],'szlig',[223],'target',[8982],
        'tau',[964],'tbrk',[9140],'tcaron',[357],'tcedil',[355],'tcy',[1090],'tdot',[8411],
        'telrec',[8981],'tfr',[120113],'therefore',[8756],'theta',[952],'thetasym',[977],'thetav',[977],
        'thickapprox',[8776],'thicksim',[8764],'thinsp',[8201],'thkap',[8776],'thksim',[8764],'thorn',[254],
        'thorn',[254],'tilde',[732],'timesbar',[10801],'timesb',[8864],'times',[215],'times',[215],
        'timesd',[10800],'tint',[8749],'toea',[10536],'topbot',[9014],'topcir',[10993],'top',[8868],
        'topf',[120165],'topfork',[10970],'tosa',[10537],'tprime',[8244],'trade',[8482],'triangle',[9653],
        'triangledown',[9663],'triangleleft',[9667],'trianglelefteq',[8884],'triangleq',[8796],'triangleright',[9657],'trianglerighteq',[8885],
        'tridot',[9708],'trie',[8796],'triminus',[10810],'triplus',[10809],'trisb',[10701],'tritime',[10811],
        'trpezium',[9186],'tscr',[120009],'tscy',[1094],'tshcy',[1115],'tstrok',[359],'twixt',[8812],
        'twoheadleftarrow',[8606],'twoheadrightarrow',[8608],'uacute',[250],'uacute',[250],'uarr',[8593],'ubrcy',[1118],
        'ubreve',[365],'ucirc',[251],'ucirc',[251],'ucy',[1091],'udarr',[8645],'udblac',[369],
        'udhar',[10606],'ufisht',[10622],'ufr',[120114],'ugrave',[249],'ugrave',[249],'uharl',[8639],
        'uharr',[8638],'uhblk',[9600],'ulcorn',[8988],'ulcorner',[8988],'ulcrop',[8975],'ultri',[9720],
        'umacr',[363],'uml',[168],'uml',[168],'uogon',[371],'uopf',[120166],'uparrow',[8593],
        'updownarrow',[8597],'upharpoonleft',[8639],'upharpoonright',[8638],'uplus',[8846],'upsi',[965],'upsih',[978],
        'upsilon',[965],'upuparrows',[8648],'urcorn',[8989],'urcorner',[8989],'urcrop',[8974],'uring',[367],
        'urtri',[9721],'uscr',[120010],'utdot',[8944],'utilde',[361],'utri',[9653],'utrif',[9652],
        'uuarr',[8648],'uuml',[252],'uuml',[252],'uwangle',[10663],'vangrt',[10652],'varepsilon',[1013],
        'varkappa',[1008],'varnothing',[8709],'varphi',[981],'varpi',[982],'varpropto',[8733],'varr',[8597],
        'varrho',[1009],'varsigma',[962],'varsubsetneq',[8842,65024],'varsubsetneqq',[10955,65024],'varsupsetneq',[8843,65024],'varsupsetneqq',[10956,65024],
        'vartheta',[977],'vartriangleleft',[8882],'vartriangleright',[8883],'vcy',[1074],'vdash',[8866],'veebar',[8891],
        'vee',[8744],'veeeq',[8794],'vellip',[8942],'verbar',[124],'vert',[124],'vfr',[120115],
        'vltri',[8882],'vnsub',[8834,8402],'vnsup',[8835,8402],'vopf',[120167],'vprop',[8733],'vrtri',[8883],
        'vscr',[120011],'vsubne',[8842,65024],'vsupne',[8843,65024],'vzigzag',[10650],'wcirc',[373],'wedbar',[10847],
        'wedge',[8743],'wedgeq',[8793],'weierp',[8472],'wfr',[120116],'wopf',[120168],'wp',[8472],
        'wr',[8768],'wreath',[8768],'wscr',[120012],'xcap',[8898],'xcirc',[9711],'xcup',[8899],
        'xdtri',[9661],'xfr',[120117],'xharr',[10231],'xi',[958],'xlarr',[10229],'xmap',[10236],
        'xnis',[8955],'xodot',[10752],'xopf',[120169],'xoplus',[10753],'xotime',[10754],'xrarr',[10230],
        'xscr',[120013],'xsqcup',[10758],'xuplus',[10756],'xutri',[9651],'xvee',[8897],'xwedge',[8896],
        'yacute',[253],'yacute',[253],'yacy',[1103],'ycirc',[375],'ycy',[1099],'yen',[165],
        'yen',[165],'yfr',[120118],'yicy',[1111],'yopf',[120170],'yscr',[120014],'yucy',[1102],
        'yuml',[255],'yuml',[255],'zacute',[378],'zcaron',[382],'zcy',[1079],'zdot',[380],
        'zeetrf',[8488],'zeta',[950],'zfr',[120119],'zhcy',[1078],'zigrarr',[8669],'zopf',[120171],
        'zscr',[120015],'zwj',[8205],'zwnj',[8204]
    -> $k, $v { %entities{$k} := $v }
    our sub str_from_entity($e) {
        my $r := '';
        for %entities{$e} // [] {
            $r := $r ~ nqp::chr($_);
        }
        $r
    }
} # end class Perl6::Pod

# vim: expandtab sw=4
