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
        my int $base   = $ord - 0x10000;
        my int $top    = $base +& 0b1_1111_1111_1100_0000_0000 +> 10;
        my int $bottom = $base +&               0b11_1111_1111;
        Q/\u/ ~ (0xD800 + $top).base(16) ~ Q/\u/ ~ (0xDC00 + $bottom).base(16);
    }

    my multi sub to-surrogate-pair(Str $input) {
        to-surrogate-pair(nqp::ordat($input, 0));
    }

#?if jvm
    # Older version of str-escape, since the JVM backend does not have .NFD yet
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
#?endif

#?if !jvm
    my $tab := nqp::list_i(92,116); # \t
    my $lf  := nqp::list_i(92,110); # \n
    my $cr  := nqp::list_i(92,114); # \r
    my $qq  := nqp::list_i(92, 34); # \"
    my $bs  := nqp::list_i(92, 92); # \\

    # Convert string to decomposed codepoints.  Run over that integer array
    # and inject whatever is necessary, don't do anything if simple ascii.
    # Then convert back to string and return that.
    sub str-escape(\text) {
        my $codes := text.NFD;
        my int $i = -1;

        nqp::while(
          nqp::islt_i(++$i,nqp::elems($codes)),
          nqp::if(
            nqp::isle_i((my int $code = nqp::atpos_i($codes,$i)),92)
              || nqp::isge_i($code,128),
            nqp::if(                                       # not ascii
              nqp::isle_i($code,31),
              nqp::if(                                      # control
                nqp::iseq_i($code,10),
                nqp::splice($codes,$lf,$i++,1),              # \n
                nqp::if(
                  nqp::iseq_i($code,13),
                  nqp::splice($codes,$cr,$i++,1),             # \r
                  nqp::if(
                    nqp::iseq_i($code,9),
                    nqp::splice($codes,$tab,$i++,1),           # \t
                    nqp::stmts(                                # other control
                      nqp::splice($codes,$code.fmt(Q/\u%04x/).NFD,$i,1),
                      ($i = nqp::add_i($i,5))
                    )
                  )
                )
              ),
              nqp::if(                                      # not control
                nqp::iseq_i($code,34),
                nqp::splice($codes,$qq,$i++,1),              # "
                nqp::if(
                  nqp::iseq_i($code,92),
                  nqp::splice($codes,$bs,$i++,1),             # \
                  nqp::if(
                    nqp::isge_i($code,0x10000),
                    nqp::stmts(                                # surrogates
                      nqp::splice(
                        $codes,
                        (my $surrogate := to-surrogate-pair($code.chr).NFD),
                        $i,
                        1
                      ),
                      ($i = nqp::sub_i(nqp::add_i($i,nqp::elems($surrogate)),1))
                    )
                  )
                )
              )
            )
          )
        );

        nqp::strfromcodes($codes)
    }
#?endif

    method to-json(
      \obj,
      Bool :$pretty        = False,
      Int  :$level         = 0,
      int  :$spacing       = 2,
      Bool :$sorted-keys   = False,
    ) {

        my $out := nqp::list_s;  # cannot use str @out because of JVM
        my str $spaces = ' ' x $spacing;
        my str $comma  = ",\n" ~ $spaces x $level;

#-- helper subs from here, with visibility to the above lexicals

        sub pretty-positional(\positional --> Nil) {
            $comma = nqp::concat($comma,$spaces);
            nqp::push_s($out,'[');
            nqp::push_s($out,nqp::substr($comma,1));

            for positional.list {
                jsonify($_);
                nqp::push_s($out,$comma);
            }
            nqp::pop_s($out);  # lose last comma

            $comma = nqp::substr($comma,0,nqp::sub_i(nqp::chars($comma),$spacing));
            nqp::push_s($out,nqp::substr($comma,1));
            nqp::push_s($out,']');
        }

        sub pretty-associative(\associative --> Nil) {
            $comma = nqp::concat($comma,$spaces);
            nqp::push_s($out,'{');
            nqp::push_s($out,nqp::substr($comma,1));
            my \pairs := $sorted-keys
              ?? associative.sort(*.key)
              !! associative.list;

            for pairs {
                jsonify(.key);
                nqp::push_s($out,": ");
                jsonify(.value);
                nqp::push_s($out,$comma);
            }
            nqp::pop_s($out);  # lose last comma

            $comma = nqp::substr($comma,0,nqp::sub_i(nqp::chars($comma),$spacing));
            nqp::push_s($out,nqp::substr($comma,1));
            nqp::push_s($out,'}');
        }

        sub unpretty-positional(\positional --> Nil) {
            nqp::push_s($out,'[');
            my int $before = nqp::elems($out);
            for positional.list {
                jsonify($_);
                nqp::push_s($out,",");
            }
            nqp::pop_s($out) if nqp::elems($out) > $before;  # lose last comma
            nqp::push_s($out,']');
        }

        sub unpretty-associative(\associative --> Nil) {
            nqp::push_s($out,'{');
            my \pairs := $sorted-keys
              ?? associative.sort(*.key)
              !! associative.list;

            my int $before = nqp::elems($out);
            for pairs {
                jsonify(.key);
                nqp::push_s($out,":");
                jsonify(.value);
                nqp::push_s($out,",");
            }
            nqp::pop_s($out) if nqp::elems($out) > $before;  # lose last comma
            nqp::push_s($out,'}');
        }

        sub jsonify(\obj --> Nil) {

            with obj {

                # basic ones
                when Bool {
                    nqp::push_s($out,obj ?? "true" !! "false");
                }
                when IntStr {
                    jsonify(.Int);
                }
                when RatStr {
                    jsonify(.Rat);
                }
                when NumStr {
                    jsonify(.Num);
                }
                when Str {
                    nqp::push_s($out,'"');
                    nqp::push_s($out,str-escape(obj));
                    nqp::push_s($out,'"');
                }

                # numeric ones
                when Int {
                    nqp::push_s($out,.Str);
                }
                when Rat {
                    nqp::push_s($out,.contains(".") ?? $_ !! "$_.0")
                      given .Str;
                }
                when FatRat {
                    nqp::push_s($out,.contains(".") ?? $_ !! "$_.0")
                      given .Str;
                }
                when Num {
                    if nqp::isnanorinf($_) {
                        nqp::push_s(
                          $out,
                          $*JSON_NAN_INF_SUPPORT ?? obj.Str !! "null"
                        );
                    }
                    else {
                        nqp::push_s($out,.contains("e") ?? $_ !! $_ ~ "e0")
                          given .Str;
                    }
                }

                # iterating ones
                when Seq {
                    jsonify(.cache);
                }
                when Positional {
                    $pretty
                      ?? pretty-positional($_)
                      !! unpretty-positional($_);
                }
                when Associative {
                    $pretty
                      ?? pretty-associative($_)
                      !! unpretty-associative($_);
                }

                # rarer ones
                when Dateish {
                    nqp::push_s($out,qq/"$_"/);
                }
                when Instant {
                    nqp::push_s($out,qq/"{.DateTime}"/)
                }
                when Version {
                    jsonify(.Str)
                }

                # also handle exceptions here
                when Exception {
                    jsonify(obj.^name => Hash.new(
                      (message => nqp::can(obj,"message")
                        ?? obj.message !! Nil
                      ),
                      obj.^attributes.grep(*.has_accessor).map: {
                          with .name.substr(2) -> $attr {
                              $attr => (
                                (.defined and not $_ ~~ Real|Positional|Associative)
                                  ?? .Str !! $_
                              ) given obj."$attr"()
                          }
                      }
                    ));
                }

                # huh, what?
                default {
                    jsonify( { 0 => 'null' } );
                }
            }
            else {
                nqp::push_s($out,'null');
            }
        }

#-- do the actual work

        jsonify(obj);
        nqp::join("",$out)
    }

    # there's a new version of from-json and friends that's a lot faster,
    # but it relies on the existence of the Uni type.
    # It doesn't exist on jvm, unfortunately.
#?if moar
    my $ws := nqp::list_i;
    nqp::bindpos_i($ws,  9, 1);  # \t
    nqp::bindpos_i($ws, 10, 1);  # \n
    nqp::bindpos_i($ws, 13, 1);  # \r
    nqp::bindpos_i($ws, 32, 1);  # space
    nqp::push_i($ws, 0);  # allow for -1 as value

    my sub nom-ws(str $text, int $pos is rw --> Nil) {
        nqp::while(
          nqp::atpos_i($ws, nqp::ordat($text, $pos)),
          $pos = nqp::add_i($pos, 1)
        )
    }


    my $hexdigits := nqp::list;
    nqp::bindpos($hexdigits,  48,  0);  # 0
    nqp::bindpos($hexdigits,  49,  1);  # 1
    nqp::bindpos($hexdigits,  50,  2);  # 2
    nqp::bindpos($hexdigits,  51,  3);  # 3
    nqp::bindpos($hexdigits,  52,  4);  # 4
    nqp::bindpos($hexdigits,  53,  5);  # 5
    nqp::bindpos($hexdigits,  54,  6);  # 6
    nqp::bindpos($hexdigits,  55,  7);  # 7
    nqp::bindpos($hexdigits,  56,  8);  # 8
    nqp::bindpos($hexdigits,  57,  9);  # 9
    nqp::bindpos($hexdigits,  65, 10);  # A
    nqp::bindpos($hexdigits,  66, 11);  # B
    nqp::bindpos($hexdigits,  67, 12);  # C
    nqp::bindpos($hexdigits,  68, 13);  # D
    nqp::bindpos($hexdigits,  69, 14);  # E
    nqp::bindpos($hexdigits,  70, 15);  # F
    nqp::bindpos($hexdigits,  97, 10);  # a
    nqp::bindpos($hexdigits,  98, 11);  # b
    nqp::bindpos($hexdigits,  99, 12);  # c
    nqp::bindpos($hexdigits, 100, 13);  # d
    nqp::bindpos($hexdigits, 101, 14);  # e
    nqp::bindpos($hexdigits, 102, 15);  # f

    my $escapees := nqp::list_i;
    nqp::bindpos_i($escapees,  34, 34);  # "
    nqp::bindpos_i($escapees,  47, 47);  # /
    nqp::bindpos_i($escapees,  92, 92);  # \
    nqp::bindpos_i($escapees,  98,  8);  # b
    nqp::bindpos_i($escapees, 102, 12);  # f
    nqp::bindpos_i($escapees, 110, 10);  # n
    nqp::bindpos_i($escapees, 114, 13);  # r
    nqp::bindpos_i($escapees, 116,  9);  # t


    my sub parse-string(str $text, int $pos is rw) {
        nqp::if(
          nqp::eqat($text, '"', nqp::sub_i($pos,1))  # starts with clean "
            && nqp::eqat($text, '"',                 # ends with clean "
                 (my int $end = nqp::findnotcclass(nqp::const::CCLASS_WORD,
                   $text, $pos, nqp::sub_i(nqp::chars($text),$pos)))
          ),
          nqp::stmts(
            (my $string := nqp::substr($text, $pos, nqp::sub_i($end, $pos))),
            ($pos = nqp::add_i($end,1)),
            $string
          ),
          parse-string-slow($text, $pos)
        )
    }

    # Slower parsing of string if the string does not exist of 0 or more
    # alphanumeric characters
    my sub parse-string-slow(str $text, int $pos is rw) {

        my int $start = nqp::sub_i($pos,1);  # include starter in string
        nqp::until(
          nqp::iseq_i((my $end := nqp::index($text, '"', $pos)), -1),
          nqp::stmts(
            ($pos = $end + 1),
            (my int $index = 1),
            nqp::while(
              nqp::eqat($text, '\\', nqp::sub_i($end, $index)),
              ($index = nqp::add_i($index, 1))
            ),
            nqp::if(
              nqp::bitand_i($index, 1),
              (return unjsonify-string(      # preceded by an even number of \
                nqp::strtocodes(
                  nqp::substr($text, $start, $end - $start),
                  nqp::const::NORMALIZE_NFD,
                  nqp::create(NFD)
                ),
                $pos
              ))
            )
          )
        );
        die "unexpected end of input in string";
    }

    # convert a sequence of Uni elements into a string, with the initial
    # quoter as the first element.
    my sub unjsonify-string(Uni:D \codes, int $pos) {
        nqp::shift_i(codes);  # lose the " without any decoration

        # fetch a single codepoint from the next 4 Uni elements
        my sub fetch-codepoint() {
            my int $codepoint = 0;
            my int $times = 5;

            nqp::while(
              ($times = nqp::sub_i($times, 1)),
              nqp::if(
                nqp::elems(codes),
                nqp::if(
                  nqp::iseq_i(
                    (my uint32 $ordinal = nqp::shift_i(codes)),
                    48  # 0
                  ),
                  ($codepoint = nqp::mul_i($codepoint, 16)),
                  nqp::if(
                    (my int $adder = nqp::atpos($hexdigits, $ordinal)),
                    ($codepoint = nqp::add_i(
                      nqp::mul_i($codepoint, 16),
                      $adder
                    )),
                    (die "invalid hexadecimal char {
                        nqp::chr($ordinal).perl
                    } in \\u sequence at $pos")
                  )
                ),
                (die "incomplete \\u sequence in string near $pos")
              )
            );

            $codepoint
        }

        my $output := nqp::create(Uni);
        nqp::while(
          nqp::elems(codes),
          nqp::if(
            nqp::iseq_i(
              (my uint32 $ordinal = nqp::shift_i(codes)),
              92  # \
            ),
            nqp::if(                                           # haz an escape
              nqp::iseq_i(($ordinal = nqp::shift_i(codes)), 117),  # u
              nqp::stmts(                                      # has a \u escape
                nqp::if(
                  nqp::isge_i((my int $codepoint = fetch-codepoint), 0xD800)
                    && nqp::islt_i($codepoint, 0xE000),
                  nqp::if(                                     # high surrogate
                    nqp::iseq_i(nqp::atpos_i(codes, 0),  92)        # \
                      && nqp::iseq_i(nqp::atpos_i(codes, 1), 117),  # u
                    nqp::stmts(                                # low surrogate
                      nqp::shift_i(codes),  # get rid of \
                      nqp::shift_i(codes),  # get rid of u
                      nqp::if(
                        nqp::isge_i((my int $low = fetch-codepoint), 0xDC00),
                        ($codepoint = nqp::add_i(              # got low surrogate
                          nqp::add_i(                          # transmogrify
                            nqp::mul_i(nqp::sub_i($codepoint, 0xD800), 0x400),
                            0x10000                            # with
                          ),                                   # low surrogate
                          nqp::sub_i($low, 0xDC00)
                        )),
                        (die "improper low surrogate \\u$low.base(16) for high surrogate \\u$codepoint.base(16) near $pos")
                      )
                    ),
                    (die "missing low surrogate for high surrogate \\u$codepoint.base(16) near $pos")
                  )
                ),
                nqp::push_i($output, $codepoint)
              ),
              nqp::if(                                         # other escapes?
                ($codepoint = nqp::atpos_i($escapees, $ordinal)),
                nqp::push_i($output, $codepoint),              # recognized escape
                (die "unknown escape code found '\\{           # huh?
                    nqp::chr($ordinal)
                }' found near $pos")
              )
            ),
            nqp::if(                                           # not an escape
              nqp::iseq_i($ordinal, 9) || nqp::iseq_i($ordinal, 10),  # \t \n
              (die "this kind of whitespace is not allowed in a string: '{
                  nqp::chr($ordinal).perl
              }' near $pos"),
              nqp::push_i($output, $ordinal)                   # ok codepoint
            )
          )
        );

        nqp::strfromcodes($output)
    }

    my sub parse-numeric(str $text, int $pos is rw) {
        my int $start = nqp::sub_i($pos,1);

        my int $end = nqp::findnotcclass(nqp::const::CCLASS_NUMERIC,
          $text, $pos, nqp::sub_i(nqp::chars($text),$pos));
        nqp::if(
          nqp::iseq_i(nqp::ordat($text, $end), 46),                      # .
          nqp::stmts(
            ($pos = nqp::add_i($end,1)),
            ($end = nqp::findnotcclass(nqp::const::CCLASS_NUMERIC,
              $text, $pos, nqp::sub_i(nqp::chars($text),$pos))
            )
          )
        );

        nqp::if(
          nqp::iseq_i((my int $ordinal = nqp::ordat($text, $end)), 101)  # e
           || nqp::iseq_i($ordinal, 69),                                 # E
          nqp::stmts(
            ($pos = nqp::add_i($end,1)),
            ($pos = nqp::add_i($pos,
              nqp::eqat($text, '-', $pos) || nqp::eqat($text, '+', $pos)
            )),
            ($end = nqp::findnotcclass(nqp::const::CCLASS_NUMERIC,
              $text, $pos, nqp::sub_i(nqp::chars($text),$pos))
            )
          )
        );

        my $result := nqp::substr($text, $start, nqp::sub_i($end,$start)).Numeric;
        nqp::if(
          nqp::istype($result, Failure),
          nqp::stmts(
            $result.Bool,  # handle Failure
            (die "at $pos: invalid number token $text.substr($start,$end - $start)")
          ),
          nqp::stmts(
            ($pos = $end),
            $result
          )
        )
    }

    my sub parse-obj(str $text, int $pos is rw) {
        my %result;
        my $hash := nqp::ifnull(
          nqp::getattr(%result,Map,'$!storage'),
          nqp::bindattr(%result,Map,'$!storage',nqp::hash)
        );

        nom-ws($text, $pos);
        my int $ordinal = nqp::ordat($text, $pos);
        nqp::if(
          nqp::iseq_i($ordinal, 125),  # }             {
          nqp::stmts(
            ($pos = nqp::add_i($pos,1)),
            %result
          ),
          nqp::stmts(
            my $descriptor := nqp::getattr(%result,Hash,'$!descriptor');
            nqp::stmts(  # this level is needed for some reason
              nqp::while(
                1,
                nqp::stmts(
                  nqp::if(
                    nqp::iseq_i($ordinal, 34),  # "
                    (my $key := parse-string($text, $pos = nqp::add_i($pos,1))),
                    (die nqp::if(
                      nqp::iseq_i($pos, nqp::chars($text)),
                      "at end of input: expected a quoted string for an object key",
                      "at $pos: json requires object keys to be strings"
                    ))
                  ),
                  nom-ws($text, $pos),
                  nqp::if(
                    nqp::iseq_i(nqp::ordat($text, $pos), 58),  # :
                    ($pos = nqp::add_i($pos, 1)),
                    (die "expected to see a ':' after an object key")
                  ),
                  nom-ws($text, $pos),
                  nqp::bindkey($hash, $key,
                    nqp::p6scalarwithvalue($descriptor, parse-thing($text, $pos))),
                  nom-ws($text, $pos),
                  ($ordinal = nqp::ordat($text, $pos)),
                  nqp::if(
                    nqp::iseq_i($ordinal, 125),  # }  {
                    nqp::stmts(
                      ($pos = nqp::add_i($pos,1)),
                      (return %result)
                    ),
                    nqp::unless(
                      nqp::iseq_i($ordinal, 44),  # ,
                      (die nqp::if(
                        nqp::iseq_i($pos, nqp::chars($text)),
                        "at end of input: unexpected end of object.",
                        "unexpected '{ nqp::substr($text, $pos, 1) }' in an object at $pos"
                      ))
                    )
                  ),
                  nom-ws($text, $pos = nqp::add_i($pos,1)),
                  ($ordinal = nqp::ordat($text, $pos)),
                )
              )
            )
          )
        )
    }


    my sub parse-array(str $text, int $pos is rw) {
        my @result;
        nqp::bindattr(@result, List, '$!reified',
          my $buffer := nqp::create(IterationBuffer));

        nom-ws($text, $pos);
        nqp::if(
          nqp::eqat($text, ']', $pos),
          nqp::stmts(
            ($pos = nqp::add_i($pos,1)),
            @result
          ),
          nqp::stmts(
            (my $descriptor := nqp::getattr(@result, Array, '$!descriptor')),
            nqp::while(
              1,
              nqp::stmts(
                (my $thing := parse-thing($text, $pos)),
                nom-ws($text, $pos),
                (my int $partitioner = nqp::ordat($text, $pos)),
                nqp::if(
                  nqp::iseq_i($partitioner,93),  # ]
                  nqp::stmts(
                    nqp::push($buffer,nqp::p6scalarwithvalue($descriptor,$thing)),
                    ($pos = nqp::add_i($pos,1)),
                    (return @result)
                  ),
                  nqp::if(
                    nqp::iseq_i($partitioner,44),  # ,
                    nqp::stmts(
                      nqp::push($buffer,nqp::p6scalarwithvalue($descriptor,$thing)),
                      ($pos = nqp::add_i($pos,1))
                    ),
                    (die "at $pos, unexpected partitioner '{
                        nqp::substr($text,$pos,1)
                    }' inside list of things in an array")
                  )
                )
              )
            )
          )
        )
    }

    my sub parse-thing(str $text, int $pos is rw) {
        nom-ws($text, $pos);

        my int $ordinal = nqp::ordat($text, $pos);
        if nqp::iseq_i($ordinal,34) {  # "
            parse-string($text, $pos = $pos + 1)
        }
        elsif nqp::iseq_i($ordinal,91) {  # [
            parse-array($text, $pos = $pos + 1)
        }
        elsif nqp::iseq_i($ordinal,123) {  # {
            parse-obj($text, $pos = $pos + 1)
        }
        elsif nqp::iscclass(nqp::const::CCLASS_NUMERIC, $text, $pos)
          || nqp::iseq_i($ordinal,45) {  # -
            parse-numeric($text, $pos = $pos + 1)
        }
        elsif nqp::iseq_i($ordinal,116) && nqp::eqat($text,'true',$pos) {
            $pos = $pos + 4;
            True
        }
        elsif nqp::iseq_i($ordinal,102) && nqp::eqat($text,'false',$pos) {
            $pos = $pos + 5;
            False
        }
        elsif nqp::iseq_i($ordinal,110) && nqp::eqat($text,'null',$pos) {
            $pos = $pos + 4;
            Any
        }
        else {
            die "at $pos: expected a json object, but got '{
              nqp::substr($text, $pos, 8).perl
            }'";
        }
    }

    method from-json(Str() $text) {
        CATCH { when X::AdHoc { die JSONException.new(:text($_)) } }

        my str $ntext   = $text;
        my int $length  = $text.chars;
        my int $pos     = 0;
        my     $result := parse-thing($text, $pos);

        try nom-ws($text, $pos);

        if $pos != nqp::chars($text) {
            die "additional text after the end of the document: { substr($text, $pos).raku }";
        }

        $result
    }
#?endif

#?if jvm
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

    my sub tear-off-combiners(\text, \pos) {
        text.substr(pos,1).NFD.skip.map( {
             $^ord > 0x10000
               ?? to-surrogate-pair($^ord)
               !! $^ord.chr
        } ).join
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
        my Mu  $treacherous;
        my Mu  $escape_counts := nqp::hash();

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
                die "this kind of whitespace is not allowed in a string: { nqp::substr($text, $pos - 1, 1).raku } at {$pos - 1}";
            }
        }

        $pos = $pos + 1;

        my str $raw = nqp::substr($text, $startpos, $endpos - $startpos);
        if $startcombiner {
            $raw = $startcombiner ~ $raw
        }
        if not $has_treacherous and not $has_hexcodes and $escape_counts {
            my @a;
            my @b;
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
                        my @caps = $/.caps>>.value>>.Str;
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

        $raw
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

        +(my $result := nqp::substr($text, $startpos - 1, $pos - $startpos + 1)) // die "at $pos: invalid number token $result.raku()";
    }

    my sub parse-obj(str $text, int $pos is rw) {
        my %result;

        my $key;
        my $value;

        nom-ws($text, $pos);

        if nqp::eqat($text, '}', $pos) {
            $pos = $pos + 1;
            return %result;
        }

        my $thing;
        loop {
            $thing := Any;

            if $key.DEFINITE {
                $thing := parse-thing($text, $pos)
            } else {
                nom-ws($text, $pos);

                if nqp::ordat($text, $pos) == 34 { # "
                    $pos    = $pos + 1;
                    $thing := parse-string($text, $pos)
                } else {
                    die "at end of string: expected a quoted string for an object key" if $pos == nqp::chars($text);
                    die "at $pos: json requires object keys to be strings";
                }
            }
            nom-ws($text, $pos);

            #my str $partitioner = nqp::substr($text, $pos, 1);

            if      nqp::eqat($text, ':', $pos) and   !($key.DEFINITE or      $value.DEFINITE) {
                $key := $thing;
            } elsif nqp::eqat($text, ',', $pos) and     $key.DEFINITE and not $value.DEFINITE {
                $value        := $thing;
                %result{$key}  = $value;
                $key          := Any;
                $value        := Any;
            } elsif nqp::eqat($text, '}', $pos) and     $key.DEFINITE and not $value.DEFINITE {
                $value        := $thing;
                %result{$key}  = $value;
                $pos           = $pos + 1;
                last;
            } else {
                die "at end of string: unexpected end of object." if $pos == nqp::chars($text);
                die "unexpected { nqp::substr($text, $pos, 1) } in an object at $pos";
            }

            $pos = $pos + 1;
        }

        %result
    }

    my sub parse-array(str $text, int $pos is rw) {
        my @result;

        nom-ws($text, $pos);

        if nqp::eqat($text, ']', $pos) {
            $pos = $pos + 1;
            return @result;
        }

        my     $thing;
        my str $partitioner;

        loop {
            $thing := parse-thing($text, $pos);
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

        @result
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
            my str $rest = nqp::substr($text, $pos - 1, 8).raku;
            die "at $pos: expected a json object, but got $initial (context: $rest)"
        }
    }

    method from-json(Str() $text) {
        CATCH { when X::AdHoc { die JSONException.new(:text($_)) } }

        my str $ntext   = $text;
        my int $length  = $text.chars;
        my int $pos     = 0;
        my     $result := parse-thing($text, $pos);

        try nom-ws($text, $pos);

        if $pos != nqp::chars($text) {
            die "additional text after the end of the document: { substr($text, $pos).raku }";
        }

        $result
    }

#?endif
}

# vim: expandtab shiftwidth=4
