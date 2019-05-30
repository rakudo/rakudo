my class JSONException is Exception {
    has $.text;

    method message {
        'Invalid JSON: ' ~ $!text
    }
}

# A slightly modified version of https://github.com/timo/json_fast/blob/5ce76c039dc143fa9a068f1dfa47b42e58046821/lib/JSON/Fast.pm6
# Key differences:
#  - to-json stringifies Version objects
#  - Removes $*JSON_NAN_INF_SUPPORT and the Falsey code path(s) that use it
#  - Custom code for stringifying some exception related things
my class Rakudo::Internals::JSON {

    my multi sub to-surrogate-pair(Int $ord) {
        my int $base = $ord - 0x10000;
        my $top = $base +& 0b1_1111_1111_1100_0000_0000 +> 10;
        my $bottom = $base +&            0b11_1111_1111;
        "\\u" ~ (0xD800 + $top).base(16) ~ "\\u" ~ (0xDC00 + $bottom).base(16);
    }

    my multi sub to-surrogate-pair(Str $input) {
        to-surrogate-pair(nqp::ordat($input, 0));
    }

    my sub str-escape(str $text is copy) {
        return $text unless $text ~~ /:m <[\x[5C] \x[22] \x[00]..\x[1F] \x[10000]..\x[10FFFF]]>/;

        $text .= subst(/ :m <[\\ "]> /,
            -> $/ {
                my str $str = $/.Str;
                if $str eq "\\" {
                    "\\\\"
                } elsif nqp::ordat($str, 0) == 92 {
                    "\\\\" ~ tear-off-combiners($str, 0)
                } elsif $str eq "\"" {
                    "\\\""
                } else {
                    "\\\"" ~ tear-off-combiners($str, 0)
                }
            }, :g);
        $text .= subst(/ <[\x[10000]..\x[10FFFF]]> /,
            -> $/ {
                to-surrogate-pair($/.Str);
            }, :g);
        $text .= subst(/ :m <[\x[10000]..\x[10FFFF]]> /,
            -> $/ {
                to-surrogate-pair($/.Str) ~ tear-off-combiners($/.Str, 0);
            }, :g);
        for flat 0..8, 11, 12, 14..0x1f -> $ord {
            my str $chr = chr($ord);
            if $text.contains($chr) {
                $text .= subst($chr, '\\u' ~ $ord.fmt("%04x"), :g);
            }
        }
        $text = $text.subst("\r\n", '\\r\\n',:g)\
                    .subst("\n", '\\n',     :g)\
                    .subst("\r", '\\r',     :g)\
                    .subst("\t", '\\t',     :g);
        $text;
    }

    method to-json($obj is copy, Bool :$pretty = True, Int :$level = 0, Int :$spacing = 2, Bool :$sorted-keys = False) {
        return $obj ?? 'true' !! 'false' if $obj ~~ Bool;

        return 'null' if not $obj.defined;

        if $obj ~~ Exception {
           return $.to-json($obj.^name => Hash.new(
              (message => nqp::can($obj,"message") ?? $obj.message !! Nil),
              $obj.^attributes.grep(*.has_accessor).map: {
                  with .name.substr(2) -> $attr {
                      $attr => (
                        (.defined and not $_ ~~ Real|Positional|Associative)
                          ?? .Str !! $_
                      ) given $obj."$attr"()
                  }
              }
            ), :$pretty, :$level, :$spacing, :$sorted-keys);
        }
        
        # Handle allomorphs like IntStr.new(0, '') properly.
        return $obj.Int.Str if $obj ~~ Int;
        return $.to-json($obj.Rat, :$pretty, :$level, :$spacing, :$sorted-keys) if $obj ~~ RatStr;

        if $obj ~~ Rat {
            my $result = $obj.Str;
            unless $obj.contains(".") {
                return $result ~ ".0";
            }
            return $result;
        }

        if $obj ~~ Num {
            # Allomorph support for NumStr, too.
            $obj = $obj.Num;
            if $obj === NaN || $obj === -Inf || $obj === Inf {
                return $obj.Str;
            } else {
                my $result = $obj.Str;
                unless $result.contains("e") {
                    return $result ~ "e0";
                }
                return $result;
            }
        }

        return "\"" ~ str-escape(~$obj) ~ "\"" if $obj ~~ Str|Version;

        return “"$obj"” if $obj ~~ Dateish;
        return “"{$obj.DateTime.Str}"” if $obj ~~ Instant;

        if $obj ~~ Seq {
            $obj = $obj.cache
        }

        my int  $lvl  = $level;
        my Bool $arr  = $obj ~~ Positional;
        my str  $out ~= $arr ?? '[' !! '{';
        my $spacer   := sub {
            $out ~= "\n" ~ (' ' x $lvl*$spacing) if $pretty;
        };

        $lvl++;
        $spacer();
        if $arr {
            for @($obj) -> $i {
              $out ~= $.to-json($i, :level($level+1), :$spacing, :$pretty, :$sorted-keys) ~ ',';
              $spacer();
            }
        }
        else {
            my @keys = $obj.keys;

            if ($sorted-keys) {
                @keys = @keys.sort;
            }

            for @keys -> $key {
                $out ~= "\"" ~
                        ($key ~~ Str ?? str-escape($key) !! $key) ~
                        "\": " ~
                        $.to-json($obj{$key}, :level($level+1), :$spacing, :$pretty, :$sorted-keys) ~
                        ',';
                $spacer();
            }
        }
        $out .=subst(/',' \s* $/, '');
        $lvl--;
        $spacer();
        $out ~= $arr ?? ']' !! '}';
        return $out;
    }

    my $ws := nqp::list_i;
    nqp::bindpos_i($ws,$_ + 1,1) for 9,10,13,32;  # allow for -1 as value
    my sub nom-ws(str $text, int $pos is rw --> Nil) {
        nqp::while(
          nqp::atpos_i($ws,nqp::ordat($text,$pos) + 1),
          $pos = $pos + 1
        );
        die "reached end of string when looking for something"
          if $pos == nqp::chars($text);
    }

    my sub tear-off-combiners(str $text, int $pos) {
        my str $combinerstuff = nqp::substr($text, $pos, 1);
        my @parts = $combinerstuff.NFD.list;
        return @parts.skip(1).map({
                if $^ord > 0x10000 {
                    to-surrogate-pair($ord);
                } else {
                    $ord.chr()
                }
            }).join()
    }

    my Mu $hexdigits := nqp::hash(
        '97', 1, '98', 1, '99', 1, '100', 1, '101', 1, '102', 1,
        '48', 1, '49', 1, '50', 1, '51', 1, '52', 1, '53', 1, '54', 1, '55', 1, '56', 1, '57', 1,
        '65', 1, '66', 1, '67', 1, '68', 1, '69', 1, '70', 1);

    my Mu $escapees := nqp::hash(
        "34", '"', "47", "/", "92", "\\", "98", "\b", "102", "\f", "110", "\n", "114", "\r", "116", "\t");

    my sub parse-string(str $text, int $pos is rw) {
        # first we gallop until the end of the string
        my int $startpos = $pos;
        my int $endpos;
        my int $textlength = nqp::chars($text);

        my int $ord;
        my int $has_hexcodes;
        my int $has_treacherous;
        my str $startcombiner = "";
        my Mu $treacherous;
        my Mu $escape_counts := nqp::hash();

        unless nqp::eqat($text, '"', $startpos - 1) {
            $startcombiner = tear-off-combiners($text, $startpos - 1);
        }

        loop {
            $ord = nqp::ordat($text, $pos);
            $pos = $pos + 1;

            if $pos > $textlength {
                die "unexpected end of document in string";
            }

            if nqp::eqat($text, '"', $pos - 1) {
                $endpos = $pos - 1;
                last;
            } elsif $ord == 92 {
                if     nqp::eqat($text, '"', $pos) or nqp::eqat($text, '\\', $pos) or nqp::eqat($text, 'b', $pos)
                    or nqp::eqat($text, 'f', $pos) or nqp::eqat($text,  'n', $pos) or nqp::eqat($text, 'r', $pos)
                    or nqp::eqat($text, 't', $pos) or nqp::eqat($text,  '/', $pos) {
                    my str $character = nqp::substr($text, $pos, 1);
                    if nqp::existskey($escape_counts, $character) {
                        nqp::bindkey($escape_counts, $character, nqp::atkey($escape_counts, $character) + 1);
                    } else {
                        nqp::bindkey($escape_counts, $character, 1);
                    }
                    $pos = $pos + 1;
                } elsif nqp::eqat($text, 'u', $pos) {
                    loop {
                        die "unexpected end of document; was looking for four hexdigits." if $textlength - $pos < 5;
                        if      nqp::existskey($hexdigits, nqp::ordat($text, $pos + 1))
                            and nqp::existskey($hexdigits, nqp::ordat($text, $pos + 2))
                            and nqp::existskey($hexdigits, nqp::ordat($text, $pos + 3))
                            and nqp::existskey($hexdigits, nqp::ordat($text, $pos + 4)) {
                            $pos = $pos + 4;
                        } else {
                            die "expected hexadecimals after \\u, but got \"{ nqp::substr($text, $pos - 1, 6) }\" at $pos";
                        }
                        $pos++;
                        if nqp::eqat($text, '\u', $pos) {
                            $pos++;
                        } else {
                            last
                        }
                    }
                    $has_hexcodes++;
                } elsif nqp::existskey($escapees, nqp::ordat($text, $pos)) {
                    # treacherous!
                    $has_treacherous++;
                    $treacherous := nqp::hash() unless $treacherous;
                    my int $treach_ord = nqp::ordat($text, $pos);
                    if nqp::existskey($treacherous, $treach_ord) {
                        nqp::bindkey($treacherous, $treach_ord, nqp::atkey($treacherous, $treach_ord) + 1)
                    } else {
                        nqp::bindkey($treacherous, $treach_ord, 1)
                    }
                    $pos++;
                } else {
                    die "don't understand escape sequence '\\{ nqp::substr($text, $pos, 1) }' at $pos";
                }
            } elsif $ord == 9 or $ord == 10 {
                die "this kind of whitespace is not allowed in a string: { nqp::substr($text, $pos - 1, 1).perl } at {$pos - 1}";
            }
        }

        $pos = $pos + 1;

        my str $raw = nqp::substr($text, $startpos, $endpos - $startpos);
        if $startcombiner {
            $raw = $startcombiner ~ $raw
        }
        if not $has_treacherous and not $has_hexcodes and $escape_counts {
            my str @a;
            my str @b;
            if nqp::existskey($escape_counts, "n") and nqp::existskey($escape_counts, "r") {
                @a.push("\\r\\n"); @b.push("\r\n");
            }
            if nqp::existskey($escape_counts, "n") {
                @a.push("\\n"); @b.push("\n");
            }
            if nqp::existskey($escape_counts, "r") {
                @a.push("\\r"); @b.push("\r");
            }
            if nqp::existskey($escape_counts, "t") {
                @a.push("\\t"); @b.push("\t");
            }
            if nqp::existskey($escape_counts, '"') {
                @a.push('\\"'); @b.push('"');
            }
            if nqp::existskey($escape_counts, "/") {
                @a.push("\\/"); @b.push("/");
            }
            if nqp::existskey($escape_counts, "\\") {
                @a.push("\\\\"); @b.push("\\");
            }
            $raw .= trans(@a => @b) if @a;
        } elsif $has_hexcodes or nqp::elems($escape_counts) {
            $raw = $raw.subst(/ \\ (<-[uU]>) || [\\ (<[uU]>) (<[a..f 0..9 A..F]> ** 3)]+ %(<[a..f 0..9 A..F]>) (:m <[a..f 0..9 A..F]>) /,
                -> $/ {
                    if $0.elems > 1 || $0.Str eq "u" || $0.Str eq "U" {
                        my str @caps = $/.caps>>.value>>.Str;
                        my $result = $/;
                        my str $endpiece = "";
                        if my $lastchar = nqp::chr(nqp::ord(@caps.tail)) ne @caps.tail {
                            $endpiece = tear-off-combiners(@caps.tail, 0);
                            @caps.pop;
                            @caps.push($lastchar);
                        }
                        my int @hexes;
                        for @caps -> $u, $first, $second {
                            @hexes.push(:16($first ~ $second).self);
                        }

                        CATCH {
                            die "Couldn't decode hexadecimal unicode escape { $result.Str } at { $startpos + $result.from }";
                        }

                        utf16.new(@hexes).decode ~ $endpiece;
                    } else {
                        if nqp::existskey($escapees, nqp::ordat($0.Str, 0)) {
                            my str $replacement = nqp::atkey($escapees, nqp::ordat($0.Str, 0));
                            $replacement ~ tear-off-combiners($0.Str, 0);
                        } else {
                            die "stumbled over unexpected escape code \\{ chr(nqp::ordat($0.Str, 0)) } at { $startpos + $/.from }";
                        }
                    }
                }, :g);
        }

        $pos = $pos - 1;

        $raw;
    }

    my sub parse-numeric(str $text, int $pos is rw) {
        my int $startpos = $pos;

        $pos = $pos + 1 while nqp::iscclass(nqp::const::CCLASS_NUMERIC, $text, $pos);

        my $residual := nqp::substr($text, $pos, 1);

        if $residual eq '.' {
            $pos = $pos + 1;

            $pos = $pos + 1 while nqp::iscclass(nqp::const::CCLASS_NUMERIC, $text, $pos);

            $residual := nqp::substr($text, $pos, 1);
        }

        if $residual eq 'e' || $residual eq 'E' {
            $pos = $pos + 1;

            if nqp::eqat($text, '-', $pos) || nqp::eqat($text, '+', $pos) {
                $pos = $pos + 1;
            }

            $pos = $pos + 1 while nqp::iscclass(nqp::const::CCLASS_NUMERIC, $text, $pos);
        }

        +(my $result := nqp::substr($text, $startpos - 1, $pos - $startpos + 1)) // die "at $pos: invalid number token $result.perl()";
    }

    my sub parse-obj(str $text, int $pos is rw) {
        my %result;

        my $key;
        my $value;

        nom-ws($text, $pos);

        if nqp::eqat($text, '}', $pos) {
            $pos = $pos + 1;
            %();
        } else {
            my $thing;
            loop {
                $thing = Any;

                if $key.DEFINITE {
                    $thing = parse-thing($text, $pos)
                } else {
                    nom-ws($text, $pos);

                    if nqp::ordat($text, $pos) == 34 { # "
                        $pos = $pos + 1;
                        $thing = parse-string($text, $pos)
                    } else {
                        die "at end of string: expected a quoted string for an object key" if $pos == nqp::chars($text);
                        die "at $pos: json requires object keys to be strings";
                    }
                }
                nom-ws($text, $pos);

                #my str $partitioner = nqp::substr($text, $pos, 1);

                if      nqp::eqat($text, ':', $pos) and   !($key.DEFINITE or      $value.DEFINITE) {
                    $key = $thing;
                } elsif nqp::eqat($text, ',', $pos) and     $key.DEFINITE and not $value.DEFINITE {
                    $value = $thing;

                    %result{$key} = $value;

                    $key   = Any;
                    $value = Any;
                } elsif nqp::eqat($text, '}', $pos) and     $key.DEFINITE and not $value.DEFINITE {
                    $value = $thing;

                    %result{$key} = $value;
                    $pos = $pos + 1;
                    last;
                } else {
                    die "at end of string: unexpected end of object." if $pos == nqp::chars($text);
                    die "unexpected { nqp::substr($text, $pos, 1) } in an object at $pos";
                }

                $pos = $pos + 1;
            }

            %result;
        }
    }

    my sub parse-array(str $text, int $pos is rw) {
        my @result;

        nom-ws($text, $pos);

        if nqp::eqat($text, ']', $pos) {
            $pos = $pos + 1;
            [];
        } else {
            my $thing;
            my str $partitioner;
            loop {
                $thing = parse-thing($text, $pos);
                nom-ws($text, $pos);

                $partitioner = nqp::substr($text, $pos, 1);
                $pos = $pos + 1;

                if $partitioner eq ']' {
                    @result.push: $thing;
                    last;
                } elsif $partitioner eq "," {
                    @result.push: $thing;
                } else {
                    die "at $pos, unexpected $partitioner inside list of things in an array";
                }
            }
            @result;
        }
    }

    my sub parse-thing(str $text, int $pos is rw) {
        nom-ws($text, $pos);

        my str $initial = nqp::substr($text, $pos, 1);

        $pos = $pos + 1;

        if nqp::ord($initial) == 34 { # "
            parse-string($text, $pos);
        } elsif $initial eq '[' {
            parse-array($text, $pos);
        } elsif $initial eq '{' {
            parse-obj($text, $pos);
        } elsif nqp::iscclass(nqp::const::CCLASS_NUMERIC, $initial, 0) || $initial eq '-' {
            parse-numeric($text, $pos);
        } elsif $initial eq 'n' {
            if nqp::eqat($text, 'ull', $pos) {
                $pos += 3;
                Any;
            } else {
                die "at $pos: i was expecting a 'null' but there wasn't one: { nqp::substr($text, $pos - 1, 10) }"
            }
        } elsif $initial eq 't' {
            if nqp::eqat($text, 'rue', $pos) {
                $pos = $pos + 3;
                True
            } else {
                die "at $pos: expected 'true', found { $initial ~ nqp::substr($text, $pos, 3) } instead.";
            }
        } elsif $initial eq 'f' {
            if nqp::eqat($text, 'alse', $pos) {
                $pos = $pos + 4;
                False
            } else {
                die "at $pos: expected 'false', found { $initial ~ nqp::substr($text, $pos, 4) } instead.";
            }
        } else {
            my str $rest = nqp::substr($text, $pos - 1, 8).perl;
            die "at $pos: expected a json object, but got $initial (context: $rest)"
        }
    }

    method from-json(Str() $text) {
        CATCH { when X::AdHoc { die JSONException.new(:text($_)) } }

        my str $ntext = $text;
        my int $length = $text.chars;

        my int $pos = 0;

        my $result = parse-thing($text, $pos);

        try nom-ws($text, $pos);

        if $pos != nqp::chars($text) {
            die "additional text after the end of the document: { substr($text, $pos).perl }";
        }

        $result;
    }
}

# vim: ft=perl6 expandtab sw=4
