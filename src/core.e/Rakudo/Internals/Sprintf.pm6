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
            '%' <idx>? <flags>* <size>? <precision>? $<sym>=<[bB]>
        }
        token directive:sym<c> {
            '%' <idx>? <flags>* <size>? <sym>
        }
        token directive:sym<d> {
            '%' <idx>? <flags>* <size>? <precision>? $<sym>=<[dDi]>
        }
        token directive:sym<e> {
            '%' <idx>? <flags>* <size>? <precision>? $<sym>=<[eE]>
        }
        token directive:sym<f> {
            '%' <idx>? <flags>* <size>? <precision>? $<sym>=<[fF]>
        }
        token directive:sym<g> {
            '%' <idx>? <flags>* <size>? <precision>? $<sym>=<[gG]>
        }
        token directive:sym<o> {
            '%' <idx>? <flags>* <size>? <precision>? <sym>
        }
        token directive:sym<O> {
            '%' <idx>? <flags>* <size>? <precision>? <sym>
        }
        token directive:sym<s> {
            '%' <idx>? <flags>* <size>? <precision>? <sym>
        }
        token directive:sym<u> {
            '%' <idx>? <flags>* <size>? <sym>
        }
        token directive:sym<x> {
            '%' <idx>? <flags>* <size>? <precision>? $<sym>=<[xX]>
        }

        proto token escape { <...> }
        token escape:sym<%> { '%' <flags>* <size>? <sym> }

        token literal { <-[%]>+ }

        token idx { [\d+] '$' }

        token flags { <[\ +0#-]> }

        token size { \d* | $<star>='*' <idx>? }

        token precision { '.' <size>? }
    }

    # class to be used with Grammar to turn format into array of pieces of code
    class Actions {
        my $knowhow := nqp::knowhow().new_type(:repr("P6bigint"));
        my $zero    := nqp::box_i(0, $knowhow);

        method statement($/ --> Nil){
            make ($<directive> || $<escape> || $<literal>).made;
        }

        method escape:sym<%>($/ --> Nil) { make '%' }

        method literal($/ --> Nil) { make $/.Str.perl }

        # helper sub to check if a flag is set
        sub has_hash($/)  { "#" (elem) $<flags>.map: *.Str }
        sub has_minus($/) { "-" (elem) $<flags>.map: *.Str }
        sub has_plus($/)  { "+" (elem) $<flags>.map: *.Str }
        sub has_space($/) { " " (elem) $<flags>.map: *.Str }
        sub has_zero($/)  { "0" (elem) $<flags>.map: *.Str }

        # helper sub to determine the value for this directive
        sub parameter($/ --> Str:D) {
            my Int $index = $<idx> ?? $<idx>.chop.Int - 1 !! $*NEXT-PARAMETER;
            X::Str::Sprintf::Directives::Unsupported.new(
              directive => ~$<idx>,
              sequence  => ~$/,
            ).throw if $index < 0;

            # set default index for next parameter
            $*NEXT-PARAMETER = $index + 1;

            # record the directive
            @*DIRECTIVES[$index].push(~$_) with $<sym> // '*';

            "\@args.AT-POS($index)"
        }

        # helper sub for processing formats for integer values
        sub handle-integer-numeric($/,
           Int:D :$base!,    # the number base to assume for generating string
           Str   :$hash,     # the string to prefix if "#" is in format
          Bool   :$plus,     # whether to prefix "+" if positive
          Bool   :$space,    # whether to prefix " " if not starting with + or -
          Bool   :$lc        # whether to lowercase resulting string
        --> Nil) {

# Please note that the order in which parameters are fetched, is following the
# way that Perl 5 is doing.  From a left-to-right point of view, it feels that
# the $expr one should be the first.

            # set up size specification
            my str $size = $<size>
              ?? $<size><star>
                ?? parameter($<size>)
                !! ~$<size>
              !! '';

            # set up precision specification
            my str $precision;
            if $<precision><size> -> $size {
                $precision = $size<star>
                  ?? parameter($size)
                  !! (~$size) || '0';
            }
            elsif $size {
                if has_zero($/) {
                    $precision = $size;
                    $size = '';
                }
                else {
                    $precision = '1'
                }
            }

            # set up initial expression to work on
            my str $expr = parameter($/)
              ~ ($base == 10 ?? '.Int.Str' !! ".Int.base($base)");
            $expr = "$expr.lc" if $lc;

            # handle any prefixes
            my int $minus;
            if has_hash($/) {
                if $hash eq '0' {
                    $expr  = "prefix-zero($expr)";
                    $minus = 1;
                }
                else {
                    $expr  = "prefix-hash('$hash',$expr)";
                    $minus = $hash.chars;
                }
            }
            elsif $plus && has_plus($/) {
                $expr  = "prefix-plus($expr)";
                $minus = 1;
            }
            elsif $space && has_space($/) {
                $expr  = "prefix-space($expr)";
                $minus = 1;
            }

            # expand to precision indicated
            if $precision {
                $expr = $minus
                  ?? "pad-zeroes-int($precision-$minus,$expr)"
                  !! "pad-zeroes-int($precision,$expr)";
            }

            # handle justification only if we need to
            if $size {
                $expr = has_minus($/)
                   ?? "str-left-justified($size,$expr)"
                   !! "str-right-justified($size,$expr)";
            }

            make $expr;
        }

        # show numeric value in binary
        method directive:sym<b>($/ --> Nil) {
            handle-integer-numeric($/, :base(2), :hash("0$<sym>"));
        }

        # show character representation of codepoint value
        method directive:sym<c>($/ --> Nil) {

            # handle justification
            my str $value = parameter($/) ~ ".chr";
            if +$<size> -> $size {
                $value = has_minus($/)
                  ?? "str-left-justified($size,$value)"
                  !! "str-right-justified($size,$value)";
            }

            make $value;
        }

        # show decimal (integer) value
        method directive:sym<d>($/ --> Nil) {
            handle-integer-numeric($/, :base(10), :plus, :space);
        }

        # show floating point value, scientific notation
        method directive:sym<e>($/ --> Nil) {

            # handle precision / plus prefixing
            my int $precision = $<precision> ?? +$<precision> !! 6;
            my str $value = "scientify($precision," ~ parameter($/) ~ ")";

            # handle left/right justification / zero padding
            $value = "prefix-plus($value)" if has_plus($/);
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

            make $value;
        }

        # show floating point value
        method directive:sym<f>($/ --> Nil) {

            # handle precision / plus prefixing
            my int $precision = $<precision> ?? +$<precision> !! 6;
            my str $value = "pad-zeroes-precision($precision,"
              ~ parameter($/)  # needs to be in outer scope
              ~ ".Numeric.round(10**-$precision).Str)";

            $value = "prefix-plus($value)" if has_plus($/);

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

            make $value;
        }

        # show numeric value in octal using Perl 5 / Perl 6 semantics
        method directive:sym<o>($/ --> Nil) {
            handle-integer-numeric($/, :base(8), :hash<0>);
        }

        # show string
        method directive:sym<s>($/ --> Nil) {

            # handle precision
            my str $value = parameter($/) ~ ".Str";
            if $<precision> -> $precision {
                $value = "$value\.substr(0,$precision)"
            }

            # handle left/right justification
            if +$<size> -> $size {
                $value = has_minus($/)
                  ?? "str-left-justified($size,$value)"
                  !! "str-right-justified($size,$value)";
            }

            make $value
        }

        # show unsigned decimal (integer) value
        method directive:sym<u>($/ --> Nil) {

            # handle unsigned check
            my str $value = "unsigned-int(" ~ parameter($/) ~ ")";

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

            make $value
        }

        # show numeric value in hexadecimal
        method directive:sym<x>($/ --> Nil) {
            handle-integer-numeric(
              $/, :base(16), :hash("0$<sym>"), :lc($<sym> eq "x")
            )
        }
    }

    # RUNTIME number of arguments checker
    sub check-args(@args,@directives--> Nil) {
        my int $args-used = @directives.elems;

        # number of args doesn't match
        if @args.elems != $args-used {
            Failure.new(
              X::Str::Sprintf::Directives::Count.new(
                args-have => @args.elems.Int,
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
                :args-have(@args.elems.Int), :1args-used
              )
            )
        }
    }

    # RUNTIME number of arguments checker for NO expected argument
    sub check-no-arg(@args --> Nil) {
        if @args.elems -> $elems {
            Failure.new(
              X::Str::Sprintf::Directives::Count.new(
                :args-have($elems.Int), :0args-used
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

    # RUNTIME prefix space if string not starting with "+" or "-"
    sub prefix-space(Str:D $string --> Str:D) {
        $string.starts-with('-') || $string.starts-with('+')
          ?? $string
          !! $string.starts-with('0') && $string ne '0'
            ?? ' ' ~ $string.substr(1)
            !! ' ' ~ $string
    }

    # RUNTIME prefix 0 if string not starting with 0
    sub prefix-zero(Str:D $string --> Str:D) {
        $string.starts-with('0')
          ?? $string
          !! $string.starts-with('-')
            ?? $string.substr-eq('0',1)
              ?? $string
              !! '-0' ~ $string.substr(1)
            !! '0' ~ $string
    }

    # RUNTIME prefix plus if value is not negative
    sub prefix-plus(Str:D $string --> Str:D) {
        $string.starts-with("-")
          ?? $string
          !! "+" ~ $string
    }

    # RUNTIME prefix given hash properly, also if value negative
    sub prefix-hash(Str:D $hash, Str:D $string --> Str:D) {
        +$string
          ?? $string.starts-with("-")
            ?? '-' ~ $hash ~ $string.substr(1)
            !! $hash ~ $string
          !! $string
    }

    # RUNTIME pad with zeroes as integer
    sub pad-zeroes-int(int $positions, Str:D $string --> Str:D) {
        $positions > 0
          ?? $string.chars < $positions
            ?? $string.starts-with('-')
              ?? '-' ~ pad-zeroes-str($positions - 1,$string.substr(1))
              !!       pad-zeroes-str($positions,$string)
            !! $string
          !! $string.ends-with('0')
            ?? $string.chop
            !! $string
    }

    # RUNTIME pad with zeroes after decimal point
    sub pad-zeroes-precision(int $positions, Str:D $string --> Str:D) {
        with $string.index(".") {
            my int $digits = $string.chars - 1 - $_;
            $positions > $digits
              ?? $string ~ "0" x ($positions - $digits)
              !! $string
        }
        else {
            $positions
              ?? $string ~ "." ~ "0" x $positions
              !! $string
        }
    }

    # RUNTIME pad with zeroes as string
    sub pad-zeroes-str(int $positions, Str:D $string --> Str:D) {
        my int $chars = $string.chars;
        $chars < $positions
          ?? "0" x $positions - $chars ~ $string
          !! $string
    }

    # RUNTIME set up value for scientific notation
    proto sub scientify(|) {*}
    multi sub scientify(Int:D $positions, Str:D $value --> Str:D) {
        scientify($positions, $value.Numeric)
    }
    multi sub scientify(Int:D $positions, Cool:D $value --> Str:D) {
        my int $exponent = $value.abs.log(10).floor;
        my int $abs-expo = $exponent.abs;
        pad-zeroes-precision(
          $positions,
          ($value / 10 ** $exponent).round(10**-$positions).Str
        ) ~ ($exponent < 0 ?? "e-" !! "e+")
          ~ ($abs-expo < 10 ?? "0" ~ $abs-expo !! $abs-expo)
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
        my @*DIRECTIVES;          # the directives seen
        my $*NEXT-PARAMETER = 0;  # index of next parameter to be expected

        if Syntax.parse($format, actions => Actions) -> $parsed {
            my @parts = $parsed<statement>.map: *.made;

            # at least one directive
            my $code = "-> \@args \{\n";
            if @*DIRECTIVES {
                $code = @*DIRECTIVES == 1
                  ?? "$code  check-one-arg(\@args,'@*DIRECTIVES[0]');\n"
                  !! "$code  check-args(\@args,(@*DIRECTIVES.map( {
                         $_ ?? "'$_'" !! "''"
                     } ).join(",")));\n";
                $code = @parts == 1
                  ?? "$code  @parts[0]\n}"
                  !! "$code  (\n    @parts.join(",\n    ")\n  ).join\n}";
            }

            # no directives, just a string
            else {
                $code = "$code  check-no-arg(\@args);\n  @parts[0]\n}";
            }
note $code;
            EVAL($code) but role is-hidden-from-backtrace {
                method is-hidden-from-backtrace(--> True) { }
            }
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
proto sub sprintf($, |) {*}
multi sub sprintf(Str(Cool) $format, \value) {
    Rakudo::Internals::Sprintf.SPRINTF($format, nqp::list(value))
}
multi sub sprintf(Str(Cool) $format, @args) {
    Rakudo::Internals::Sprintf.SPRINTF($format, @args)
}
multi sub sprintf(Str(Cool) $format, *@args) {
    Rakudo::Internals::Sprintf.SPRINTF($format, @args)
}
