# THEORY OF OPERATION
# -------------------
# The SPRINTF method will look for the given format in a hash.  If found,
# it contains a Callable that will process the parameters doing all of the
# necessary checks and conversions.
#
# If not found, it will parse the format with the Syntax grammar and the
# Actions class, which will create the source code for the format, EVAL it,
# store the resulting Callable in the hash and then run that Callable.
# 
# At about 100 times calling the sprintf with the same format, this code
# is faster than the sprintf handling of nqp (as that runs the grammar
# **every time** sprintf is called).  At about 10K times calling the same
# format, it is about 10x as fast as the nqp equivalent.
#
# TODO:
# - optimize RUNTIME parts to use nqp::ops only
# - generate code that uses native ops and variables where possible
# - port the whole approach back to nqp and build the QAST without having
#   to run EVAL.

class Rakudo::Internals::Sprintf {

    # the grammar for parsing format strings
    grammar Syntax {
        token TOP { ^ <statement>* $ }

        method panic($message, $payload) {
            my $ex := nqp::newexception();
            nqp::setmessage($ex, $message);
            nqp::setpayload($ex, $payload);
            nqp::throw($ex);
        }

        token statement {
            [
            | <?[%]> [ [ <directive> | <escape> ]
              || <.panic(
                "'"
                  ~ self.orig.substr(1)
                  ~ "' is not valid in sprintf format sequence '"
                  ~ self.orig
                  ~ "'",
                nqp::hash(
                  'BAD_DIRECTIVE',
                  nqp::hash(
                    'DIRECTIVE', self.orig.substr(1),
                    'SEQUENCE', self.orig
                  )
                )
              )> ]
            | <![%]> <literal>
            ]
        }

        proto token directive { <...> }
        token directive:sym<b> {
            '%' <idx>? <flags>* <size>? [ '.' <precision=.size> ]? $<sym>=<[bB]>
        }
        token directive:sym<c> {
            '%' <idx>? <flags>* <size>? <sym>
        }
        token directive:sym<d> {
            '%' <idx>? <flags>* <size>? [ '.' <precision=.size> ]? $<sym>=<[di]>
        }
        token directive:sym<e> {
            '%' <idx>? <flags>* <size>? [ '.' <precision=.size> ]? $<sym>=<[eE]>
        }
        token directive:sym<f> {
            '%' <idx>? <flags>* <size>? [ '.' <precision=.size> ]? $<sym>=<[fF]>
        }
        token directive:sym<g> {
            '%' <idx>? <flags>* <size>? [ '.' <precision=.size> ]? $<sym>=<[gG]>
        }
        token directive:sym<o> {
            '%' <idx>? <flags>* <size>? [ '.' <precision=.size> ]? <sym>
        }
        token directive:sym<s> {
            '%' <idx>? <flags>* <size>? [ '.' <precision=.size> ]? <sym>
        }
        token directive:sym<u> {
            '%' <idx>? <flags>* <size>? <sym>
        }
        token directive:sym<x> {
            '%' <idx>? <flags>* <size>? [ '.' <precision=.size> ]? $<sym>=<[xX]>
        }

        proto token escape { <...> }
        token escape:sym<%> { '%' <flags>* <size>? <sym> }

        token literal { <-[%]>+ }

        token idx {
            $<param_index>=[\d+] '$'
        }

        token flags { <[ +0#-]> }

        token size {
            \d* | $<star>='*'
        }
    }

    # class to be used with Grammar to turn format into array of pieces of code
    class Actions {
        my $knowhow := nqp::knowhow().new_type(:repr("P6bigint"));
        my $zero    := nqp::box_i(0, $knowhow);

        method statement($/){
            make ($<directive> || $<escape> || $<literal>).made;
        }

        method escape:sym<%>($/) { make '%' }

        method literal($/) { make $/.Str.perl }

        # helper sub to check if a flag is set
        sub has_hash($/)  { $<flags>.contains("#") }
        sub has_minus($/) { $<flags>.contains("-") }
        sub has_plus($/)  { $<flags>.contains("+") }
        sub has_zero($/)  { $<flags>.contains("0") }

        # helper sub for processing formats for integer values
        sub handle-integer-numeric($/, Int:D $base, Str:D $hash) {
            # set up any prefixes
            my str $prefix = has_plus($/) ?? "+" !! "";
            $prefix = $prefix ~ $hash if has_hash($/);

            # handle precision / zero filling
            my int $size = +$<size>;
            my str $value  = "\@args.AT-POS({$*DIRECTIVES_SEEN++}).Int.base($base)";
            if $<precision> -> $precision {
                $value = "pad-zeroes-str($precision,$value)";
            }
            elsif has_zero($/) {
                $value = "pad-zeroes-str({$size - $prefix.chars},$value)";
            }

            # make sure we add the prefix
            $value = "'$prefix' ~ $value" if $prefix;

            # handle left/right justification
            if $size {
                $value = has_minus($/)
                 ?? "str-left-justified($size,$value)"
                 !! "str-right-justified($size,$value)";
            }

            make (~$<sym>, $value);
        }

        # show numeric value in binary
        method directive:sym<b>($/) {
            handle-integer-numeric($/, 2, "0$<sym>")
        }

        # show character representation of codepoint value
        method directive:sym<c>($/) {
            my str $value = "\@args.AT-POS({$*DIRECTIVES_SEEN++}).chr";

            # handle justification
            if +$<size> -> $size {
                $value = has_minus($/)
                  ?? "str-left-justified($size,$value)"
                  !! "str-right-justified($size,$value)";
            }

            make (~$<sym>, $value)
        }

        # show decimal (integer) value
        method directive:sym<d>($/) {

            # handle precision / plus prefixing
            my str $value = "\@args.AT-POS({$*DIRECTIVES_SEEN++}).Int.Str";
            if $<precision> -> $precision {
                $value = has_plus($/)
                  ?? "prefix-plus(pad-zeroes-int($precision,$value))"
                  !! "pad-zeroes-int($precision,$value)";
            }
            elsif has_plus($/) {
                $value = "prefix-plus($value)";
            }

            # handle left/right justification / zero padding
            if +$<size> -> $size {
                if has_minus($/) {
                    $value = "str-left-justified($size,$value)";
                }
                elsif has_zero($/) {
                    $value = "pad-zeroes-int($size,$value)";
                }
                else {
                    $value = "str-right-justified($size,$value)";
                }
            }

            make (~$<sym>, $value)
        }

        # show numeric value in octal
        method directive:sym<o>($/) {
            handle-integer-numeric($/, 8, "0")
        }

        # show string
        method directive:sym<s>($/) {
            my str $value = "\@args.AT-POS({$*DIRECTIVES_SEEN++}).Str";
            
            # handle precision
            if $<precision> -> $precision {
                $value = "$value\.substr(0,$precision)"
            }

            # handle left/right justification
            if +$<size> -> $size {
                $value = has_minus($/)
                  ?? "str-left-justified($size,$value)"
                  !! "str-right-justified($size,$value)";
            }

            make (~$<sym>, $value)
        }

        # show unsigned decimal (integer) value
        method directive:sym<u>($/) {

            # handle unsigned check
            my str $value = "unsigned-int(\@args.AT-POS({$*DIRECTIVES_SEEN++}))";
            # handle zero padding / left / right justification
            if +$<size> -> $size {
                if has_minus($/) {
                    $value = "str-left-justified($size,$value)";
                }
                elsif has_zero($/) {
                    $value = "pad-zeroes-int($size,$value)";
                }
                else {
                    $value = "str-right-justified($size,$value)";
                }
            }

            make (~$<sym>, $value)
        }

        # show numeric value in octal
        method directive:sym<x>($/) {
            handle-integer-numeric($/, 16, "0$<sym>")
        }
    }

    # RUNTIME number of arguments checker
    sub check-args(@args,@directives--> Nil) {
        my int $args-used = @directives.elems;

        # number of args doesn't match
        if @args.elems != $args-used {
            Failure.new(
              X::Str::Sprintf::Directives::Count.new(
                args-have => @args.elems.Num,
                args-used => $args-used,
              )
            )
        }

        # check types
        else {
            return Failure.new(
              X::Str::Sprintf::Directives::BadType.new(
                directive => @directives.AT-POS($_),
                expected  => "Cool",
                type      => @args.AT-POS($_).^name,
                value     => @args.AT-POS($_),
              )
            ) unless Cool.ACCEPTS(@args.AT-POS($_))
              for ^$args-used;
        }
    }

    # RUNTIME number of arguments checker for single expected argument
    sub check-one-arg(@args, str $directive --> Nil) {

        # number of args matches, check type
        if @args.elems == 1 {
            Failure.new(
              X::Str::Sprintf::Directives::BadType.new(
                directive => $directive,
                expected  => "Cool",
                type      => @args.AT-POS(0).^name,
                value     => @args.AT-POS(0),
              )
            ) unless Cool.ACCEPTS(@args.AT-POS(0))
        }
        # number of args doesn't match
        else {
            Failure.new(
              X::Str::Sprintf::Directives::Count.new(
                :args-have(@args.elems.Num), :1args-used
              )
            )
        }
    }

    # RUNTIME number of arguments checker for NO expected argument
    sub check-no-arg(@args --> Nil) {
        if @args.elems -> $elems {
            Failure.new(
              X::Str::Sprintf::Directives::Count.new(
                :args-have($elems.Num), :0args-used
              )
            )
        }
    }

    # RUNTIME check if value is positive integer and stringify
    sub unsigned-int(Cool:D $arg --> Str:D) {
        my $value := $arg.Int;

        # number of args matches, check type
        $value < 0
          ?? Failure.new(
               X::Str::Sprintf::Directives::BadType.new(
                 directive => "u",
                 expected  => "UInt",
                 type      => $arg.^name,
                 value     => $value
               )
             )
          !! $value.Str
        }

    # RUNTIME prefix plus if value is not negative
    sub prefix-plus(Str:D $string --> Str:D) {
        $string.starts-with("-") ?? $string !! "+" ~ $string
    }

    # RUNTIME pad with zeroes as integer
    sub pad-zeroes-int(int $positions, Str:D $string --> Str:D) {
        $string.starts-with("-")
          ?? "-" ~ pad-zeroes-str($positions,$string.substr(1))
          !!       pad-zeroes-str($positions,$string)
    }

    # RUNTIME pad with zeroes as string
    sub pad-zeroes-str(int $positions, Str:D $string --> Str:D) {
        my int $chars = $string.chars;
        $chars < $positions
          ?? "0" x $positions - $chars ~ $string
          !! $string
    }

    # RUNTIME string, left justified
    sub str-left-justified(int $positions, Str:D $string --> Str:D) {
        my int $chars = $string.chars;
        $chars < $positions
          ?? $string ~ " " x $positions - $chars
          !! $string
    }
    # RUNTIME string, right justified
    sub str-right-justified(int $positions, Str:D $string --> Str:D) {
        my int $chars = $string.chars;
        $chars < $positions
          ?? " " x $positions - $chars ~ $string
          !! $string
    }

    # Create callable for given uncached format string
    sub create-format($format --> Callable:D) {
        my $*DIRECTIVES_SEEN = 0;
        if Syntax.parse($format, actions => Actions) -> $parsed {
            my @directives;
            my @parts = $parsed<statement>.map: {
                my $made := .made;

                # an actual directive
                if $made.elems > 1 {
                    @directives.push($made[0].perl);
                    $made[1]
                }

                # just a string
                else {
                    $made
                }
            }

            # at least one directive
            my $code = "-> \@args \{\n";
            if @directives {
                $code = @directives == 1
                  ?? "$code  check-one-arg(\@args,@directives[0]);\n"
                  !! "$code  check-args(\@args,(@directives.join(",")));\n";
                $code = @parts == 1
                  ?? "$code  @parts[0]\n}"
                  !! "$code  (\n    @parts.join(",\n    ")\n  ).join\n}";
            }

            # no directives, just a string
            else {
                $code = "$code  check-no-arg(\@args);\n  @parts[0]\n}";
            }
say $code;
            EVAL $code
        }
        else {
            die "huh?"
        }
    }

    # actual workhorse for sprintf()
    my $format-lock := Lock.new; # allow multiple threads to create formats
    my $FORMATS := nqp::hash;    # where we keep our formats
    method SPRINTF(Str:D $format, @args --> Str:D) {
        $format-lock.protect: {
            if nqp::atkey($FORMATS,$format) -> &process {
                process(@args);
            }
            else {
                nqp::deletekey(
                  $FORMATS,
                  nqp::iterkey_s(nqp::shift(nqp::iterator($FORMATS)))
                ) if nqp::elems($FORMATS) == 100;
                nqp::bindkey($FORMATS,$format,create-format($format))(@args)
            }
        }
    }
}

# the procedural frontend of sprintf functionality
proto sub zprintf($, |) {*}
multi sub zprintf(Str(Cool) $format, \value) {
    Rakudo::Internals::Sprintf.SPRINTF($format, nqp::list(value))
}
multi sub zprintf(Str(Cool) $format, @args) {
    Rakudo::Internals::Sprintf.SPRINTF($format, @args)
}
multi sub zprintf(Str(Cool) $format, *@args) {
    Rakudo::Internals::Sprintf.SPRINTF($format, @args)
}
