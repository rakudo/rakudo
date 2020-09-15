# THEORY OF OPERATION
# -------------------
# The new method will look for the given format in a hash.  If found,
# it contains a Callable that will process the parameters doing all of the
# necessary checks and conversions.
#
# If not found, it will parse the format with the Syntax grammar and the
# Actions class, which will create the RakuAST nodes for it and store the
# Callable in the hash and then run that Callable.
#
# At about 100 times calling the sprintf with the same format, this code
# is faster than the sprintf handling of nqp (as that runs the grammar
# **every time** sprintf is called).  At about 10K times calling the same
# format, it is about 10x as fast as the nqp equivalent.
#
# TODO:
# - optimize RUNTIME parts to use nqp::ops only (or perhaps not)
# - generate code that uses native ops and variables where possible

# problem cases that should be checked:
# - https://github.com/Raku/old-issue-tracker/issues/4537
#   say sprintf('%f %f %f %f', Mu, Any, Nil, NaN);
# - https://github.com/Raku/old-issue-tracker/issues/4892
#   say sprintf("%e",1000)    # should be 1.0... instead of 10....

# the grammar for parsing format strings
grammar Formatter::Syntax {
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

class Formatter {

    # class to be used with Grammar to turn format into array of pieces of code
    class Actions {
        my $knowhow := nqp::knowhow().new_type(:repr("P6bigint"));
        my $zero    := nqp::box_i(0, $knowhow);

        method statement($/ --> Nil){
            make ($<directive> || $<escape> || $<literal>).made;
        }

        method escape:sym<%>($/ --> Nil) {
            make RakuAST::StrLiteral.new('%') but '%'
        }

        method literal($/ --> Nil) {
            my $string := $/.Str;
            make RakuAST::StrLiteral.new($string) but $string.raku;
        }

        # helper sub to check if a flag is set
        sub has_hash($/)  { "#" (elem) $<flags>.map: *.Str }
        sub has_minus($/) { "-" (elem) $<flags>.map: *.Str }
        sub has_plus($/)  { "+" (elem) $<flags>.map: *.Str }
        sub has_space($/) { " " (elem) $<flags>.map: *.Str }
        sub has_zero($/)  { "0" (elem) $<flags>.map: *.Str }

        # helper sub to determine the value for this directive
        sub parameter($/) {
            my Int $index = $<idx> ?? $<idx>.chop.Int - 1 !! $*NEXT-PARAMETER;
            X::Str::Sprintf::Directives::Unsupported.new(
              directive => ~$<idx>,
              sequence  => ~$/,
            ).throw if $index < 0;

            # set default index for next parameter
            $*NEXT-PARAMETER = $index + 1;

            # record the directive
            @*DIRECTIVES[$index] = .Str with $<sym> // '*';

            # @args.AT-POS($index)
            RakuAST::ApplyPostfix.new(
              operand => RakuAST::Var::Lexical.new('@args'),
              postfix => RakuAST::Call::Method.new(
                name => RakuAST::Name.from-identifier('AT-POS'),
                args => RakuAST::ArgList.new(
                  RakuAST::IntLiteral.new($index)
                )
              )
            ) but "\@args.AT-POS($index)"
        }

        # helper sub for creating literal integer nodes
        sub literal-integer(Int:D $int) {
            RakuAST::IntLiteral.new($int) but $int.Str
        }

        # helper sub for name of justification sub
        sub justification-name($/) {
            has_minus($/) ?? "str-left-justified" !! "str-right-justified"
        }

        # helper sub to call a method on a given AST
        sub ast-call-method($ast, $name) {
            RakuAST::ApplyPostfix.new(
              operand => $ast,
              postfix => RakuAST::Call::Method.new(
                name => RakuAST::Name.from-identifier($name)
              )
            ) but $ast ~ ".$name"
        }

        # helper sub to get size specification
        sub size($/) { any-size($<size>) }

        # helper sub to get precision specification
        sub precision($/) { any-size($<precision><size>) }

        # helper sub to get any size-type info.  Note that this will "eat"
        # parameters if a * is specified, indicating runtime width info.
        sub any-size($/) {
            $/
              ?? $<star>
                ?? ast-call-method(parameter($/), 'Int')
                !! $/.Int > 1
                  ?? literal-integer($/.Int)
                  !! Nil
              !! Nil
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
# way that Perl is doing.  From a left-to-right point of view, it feels that
# the $ast one should be the first.

            # set up size / precision specification
            my $size      = size($/);
            my $precision = precision($/);

            if !$precision && $size {
                if has_zero($/) {
                    $precision = $size;
                    $size = Nil;
                }
#                else {
#                    $precision = literal-integer(1);
#                }
            }

            # parameter($/).Int
            my $ast = ast-call-method(parameter($/), 'Int');

            # $ast.(Str || .base($base))
            $ast = RakuAST::ApplyPostfix.new(
              operand => $ast,
              postfix => $base == 10
                ?? RakuAST::Call::Method.new(
                     name => RakuAST::Name.from-identifier('Str')
                   )
                !! RakuAST::Call::Method.new(
                     name => RakuAST::Name.from-identifier('base'),
                     args => RakuAST::ArgList.new(literal-integer($base))
                   )
            ) but $ast ~ ($base == 10 ?? '.Str' !! ".base($base)");

            # $ast.lc
            $ast = ast-call-method($ast, 'lc') if $lc;

            # handle any prefixes
            my int $minus;
            if has_hash($/) {
                if $hash eq '0' {
                    # prefix-zero($ast)
                    $ast = RakuAST::Call::Name.new(
                      name => RakuAST::Name.from-identifier('prefix-zero'),
                      args => RakuAST::ArgList.new(
                        RakuAST::Statement::Expression.new($ast)
                      )
                    ) but "prefix-zero($ast)";
                    $minus = 1;
                }
                else {
                    # prefix-hash('$hash',$ast)
                    $ast = RakuAST::Call::Name.new(
                      name => RakuAST::Name.from-identifier('prefix-hash'),
                      args => RakuAST::ArgList.new(
                        RakuAST::StrLiteral.new($hash),
                        RakuAST::Statement::Expression.new($ast)
                      )
                    ) but "prefix-hash('$hash',$ast)";
                    $minus = $hash.chars;
                }
            }
            elsif $plus && has_plus($/) {
                # prefix-plus($ast)
                $ast = RakuAST::Call::Name.new(
                  name => RakuAST::Name.from-identifier('prefix-plus'),
                  args => RakuAST::ArgList.new(
                    RakuAST::Statement::Expression.new($ast)
                  )
                ) but "prefix-plus($ast)";
                $minus = 1;
            }
            elsif $space && has_space($/) {
                # prefix-space($ast)
                $ast = RakuAST::Call::Name.new(
                  name => RakuAST::Name.from-identifier('prefix-space'),
                  args => RakuAST::ArgList.new(
                    RakuAST::Statement::Expression.new($ast)
                  )
                ) but "prefix-space($ast)";
                $minus = 1;
            }

            # expand to precision indicated
            if $precision {
                if $minus {
                    # pad-zeroes-int($precision - $minus, $ast)
                    $ast = RakuAST::Call::Name.new(
                      name => RakuAST::Name.from-identifier('pad-zeroes-int'),
                      args => RakuAST::ArgList.new(
                        RakuAST::ApplyInfix.new(
                          left  => $precision,
                          infix => RakuAST::Infix.new('-'),
                          right => literal-integer($minus)
                        ),
                        RakuAST::Statement::Expression($ast)
                      )
                    ) but "pad-zeroes-int($precision - $minus,$ast)";
                }
                else {
                    # pad-zeroes-int($precision, $ast)
                    $ast = RakuAST::Call::Name.new(
                      name => RakuAST::Name.from-identifier('pad-zeroes-int'),
                      args => RakuAST::ArgList.new(
                        $precision,
                        RakuAST::Statement::Expression.new($ast)
                      )
                    ) but "pad-zeroes-int($precision,$ast)";
                }
            }

            # handle justification only if we need to
            if $size {
                my $name = has_minus($/)
                  ?? "str-left-justified"
                  !! "str-right-justified";

                # str-(left|right)-justified($precision, $ast)
                $ast = RakuAST::Call::Name.new(
                  name => RakuAST::Name.from-identifier($name),
                  args => RakuAST::ArgList.new(
                    $size,
                    RakuAST::Statement::Expression.new($ast)
                  )
                ) but $name ~ "($size,$ast)";
            }

            make $ast;
        }

        # helper sub for float values handling plus/minus/zero padding
        sub plus-minus-zero($/, $size, $ast is copy) {

            if has_plus($/) {

                # prefix-plus($ast)
                $ast = RakuAST::Call::Name.new(
                  name => RakuAST::Name.from-identifier("prefix-plus"),
                  args => RakuAST::ArgList.new(
                    RakuAST::Statement::Expression.new($ast)
                  )
                ) but "prefix-plus($ast)";
            }

            if $size {
                my $name = has_minus($/)
                  ?? 'str-left-justified'
                  !! has_zero($/)
                    ?? "pad-zeroes-int"
                    !! "str-right-justified";

                # $name($size,$ast)
                $ast = RakuAST::Call::Name.new(
                  name => RakuAST::Name.from-identifier($name),
                  args => RakuAST::ArgList.new(
                    $size,
                    RakuAST::Statement::Expression.new($ast)
                  )
                ) but $name ~ "($size,$ast)";
            }

            $ast
        }

        # show numeric value in binary
        method directive:sym<b>($/ --> Nil) {
            handle-integer-numeric($/, :base(2), :hash("0$<sym>"));
        }

        # show character representation of codepoint value
        method directive:sym<c>($/ --> Nil) {

            my $size = size($/);
            my $ast  = parameter($/);

            # $ast.chr
            $ast = RakuAST::ApplyPostfix.new(
              operand => $ast,
              postfix => RakuAST::Call::Method.new(
                name => RakuAST::Name.from-identifier('chr')
              )
            ) but "$ast.chr";

            if $size {
                my $name = justification-name($/);

                # str-(left|right)-justified($size, $ast)
                $ast = RakuAST::Call::Name.new(
                  name => RakuAST::Name.from-identifier($name),
                  args => RakuAST::ArgList.new(
                    RakuAST::IntLiteral.new($size),
                    RakuAST::Statement::Expression.new($ast)
                  )
                ) but $name ~ "($size,$ast)";
            }

            make $ast;
        }

        # show decimal (integer) value
        method directive:sym<d>($/ --> Nil) {
            handle-integer-numeric($/, :base(10), :plus, :space);
        }

        # show floating point value, scientific notation
        method directive:sym<e>($/ --> Nil) {

            my $size      = size($/);
            my $precision = precision($/) // literal-integer(6);
            my $ast       = parameter($/);

            # scientify($precision,$ast)
            $ast = RakuAST::Call::Name.new(
              name => RakuAST::Name.from-identifier("scientify"),
              args => RakuAST::ArgList.new(
                $precision,
                RakuAST::Statement::Expression.new($ast)
              )
            ) but "scientify($precision,$ast)";

            make plus-minus-zero($/, $size, $ast);
        }

        # show floating point value
        method directive:sym<f>($/ --> Nil) {

            my $size      = size($/);
            my $precision = precision($/) // literal-integer(6);

            # parameter.Numeric
            my $ast = ast-call-method(parameter($/), 'Numeric');

            # $ast.round(10 ** -$the-precision)
            $ast = RakuAST::ApplyPostfix.new(
              operand => $ast,
              postfix => RakuAST::Call::Method.new(
                name => RakuAST::Name.from-identifier('round'),
                args => RakuAST::ArgList.new(
                  RakuAST::ApplyInfix.new(
                    left  => RakuAST::IntLiteral.new(10),
                    infix => RakuAST::Infix.new('**'),
                    right => RakuAST::ApplyPrefix.new(
                      prefix  => RakuAST::Prefix.new("-"),
                      operand => $precision
                    )
                  )
                )
              )
            ) but $ast ~ ".round(10 ** -$precision)";

            # $ast.Str
            $ast = ast-call-method($ast, 'Str');

            make plus-minus-zero($/, $size, $ast);
        }

        # show numeric value in octal using Perl / Raku semantics
        method directive:sym<o>($/ --> Nil) {
            handle-integer-numeric($/, :base(8), :hash<0>);
        }

        # show string
        method directive:sym<s>($/ --> Nil) {
            my $ast = parameter($/);

            # $ast.Str
            $ast = RakuAST::ApplyPostfix.new(
              operand => $ast,
              postfix => RakuAST::Call::Method.new(
                name => RakuAST::Name.from-identifier('Str')
              )
            ) but $ast ~ ".Str";

            # handle precision
            if $<precision><size> -> $the-precision {
                # $ast.substr(0,$the-precision)
                $ast = RakuAST::ApplyPostfix.new(
                  operand => $ast,
                  postfix => RakuAST::Call::Method.new(
                    name => RakuAST::Name.from-identifier('substr'),
                    args => RakuAST::ArgList.new(
                      RakuAST::IntLiteral.new(0),
                      RakuAST::IntLiteral.new($the-precision.Int)
                    )
                  )
                ) but $ast ~ ".substr(0,$the-precision)";

            }

            # handle left/right justification
            if $<size> && +$<size> -> $the-size {
                my $name = justification-name($/);

                # str-(left|right)-justified($size, $ast)
                $ast = RakuAST::Call::Name.new(
                  name => RakuAST::Name.from-identifier($name),
                  args => RakuAST::ArgList.new(
                    RakuAST::IntLiteral.new($the-size),
                    RakuAST::Statement::Expression.new($ast)
                  )
                ) but $name ~ "($the-size,$ast)";
            }

            make $ast
        }

        # show unsigned decimal (integer) value
        method directive:sym<u>($/ --> Nil) {

            # handle unsigned check
            my str $value = "unsigned-int(" ~ parameter($/) ~ ")";

            # handle zero padding / left / right justification
            if $<size> && +$<size> -> $the-size {
                if has_minus($/) {
                    $value = "str-left-justified($the-size,$value)";
                }
                elsif has_zero($/) {
                    $value = "pad-zeroes-int($the-size,$value)";
                }
                else {
                    $value = "str-right-justified($the-size,$value)";
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
#    proto sub scientify(|) {*}
#    multi sub scientify($positions, Str:D $value --> Str:D) {
#        scientify($positions, $value.Numeric)
#    }
#    multi sub scientify($positions, Cool:D $value --> Str:D) {
    sub scientify($positions, Cool:D $value is copy --> Str:D) {
        $value = $value.Numeric if $value ~~ Str;  # no multis in RakuAST yet

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
            my $ast;

            # at least one directive
            if @*DIRECTIVES -> @directives {
                if @directives == 1 {

                    # check-one-arg(@args, @directives[0])
                    $ast = RakuAST::Call::Name.new(
                      name => RakuAST::Name.from-identifier('check-one-arg'),
                      args => RakuAST::ArgList.new(
                        RakuAST::Var::Lexical.new('@args'),
                        RakuAST::StrLiteral.new(@directives[0])
                      )
                    ) but "check-one-arg(@args,'@directives[0]')";
                }
                else {

                    # infix:<,>(@directives.map( { "$_" } ))
                    $ast = RakuAST::ApplyListInfix.new(
                      infix    => RakuAST::Infix.new(','),
                      operands => [@directives.map( {
                        RakuAST::StrLiteral.new($_ || '')
                      } )]
                    ) but "(@directives.map( { $_ ?? "'$_'" !! "''" } ).join(","))";

                    # check-args(@args, $ast)
                    $ast = RakuAST::Call::Name.new(
                      name => RakuAST::Name.from-identifier('check-args'),
                      args => RakuAST::ArgList.new(
                        RakuAST::Var::Lexical.new('@args'),
                        RakuAST::Statement::Expression.new($ast)
                      )
                    ) but "check-args(@args,$ast)";
                }

                if @parts == 1 {

                    # $ast; @parts[0]
                    $ast = RakuAST::StatementList.new(
                     RakuAST::Statement::Expression.new($ast),
                      RakuAST::Statement::Expression.new(@parts[0])
                    ) but "$ast;\n  @parts[0]";
                }
                else {

                    # $ast; @parts.join
                    $ast = RakuAST::StatementList.new(
                      RakuAST::Statement::Expression.new($ast),
                      RakuAST::Statement::Expression.new(
                        RakuAST::ApplyPostfix.new(
                          operand => RakuAST::ApplyListInfix.new(
                            infix     => RakuAST::Infix.new(','),
                            operands  => @parts
                          ),
                          postfix => RakuAST::Call::Method.new(
                            name => RakuAST::Name.from-identifier('join')
                          )
                        )
                      )
                    ) but "$ast;\n  (\n    @parts.join(",\n    ")\n  ).join";
                }
            }

            # no directives, just a string
            else {

                # check-no-arg(@args); $format
                $ast = RakuAST::StatementList.new(
                  RakuAST::Statement::Expression.new(
                    RakuAST::Call::Name.new(
                      name => RakuAST::Name.from-identifier('check-no-arg'),
                      args => RakuAST::ArgList.new(
                        RakuAST::Var::Lexical.new('@args')
                      )
                    )
                  ),
                  RakuAST::Statement::Expression.new(
                    RakuAST::StrLiteral.new($format)
                  )
                ) but "check-no-arg(\@args);\n  '$format'";
            }


            # -> @args { $ast }
            $ast = RakuAST::PointyBlock.new(
              signature => RakuAST::Signature.new(
                parameters => (
                  RakuAST::Parameter.new(
                    target => RakuAST::ParameterTarget::Var.new('@args')
                  ),
                )
              ),
              body => RakuAST::Blockoid.new($ast)
            ) but "-> \@args \{\n  $ast\n\}";

note $ast.Str if %*ENV<RAKUDO_FORMATTER>;
            EVAL(%*ENV<RAKUDO_AST> ?? $ast !! $ast.Str)
        }
        else {
            die "huh?"
        }
    }

    # actual workhorse for sprintf()
    my $format-lock := Lock.new; # allow multiple threads to create formats
    my $FORMATS := nqp::hash;    # where we keep our formats
    method new(Str:D $format) {
        $format-lock.protect: {
            nqp::ifnull(
              nqp::atkey($FORMATS,$format),
              nqp::stmts(
                nqp::if(
                  nqp::iseq_i(nqp::elems($FORMATS),100),  # XXX  should be settable
                  nqp::deletekey(
                    $FORMATS,
                    nqp::iterkey_s(nqp::shift(nqp::iterator($FORMATS)))
                  )
                ),
                nqp::bindkey($FORMATS,$format,create-format($format))
              )
            )
        }
    }
}

# the procedural frontend of sprintf functionality
#proto sub sprintf($, |) {*}
#multi sub sprintf(Str(Cool) $format, \value) {
#    Formatter.new($format)(nqp::list(value))
#}
#multi sub sprintf(Str(Cool) $format, @args) {
#    Formatter.new($format)(@args)
#}
#multi sub sprintf(Str(Cool) $format, *@args) {
sub sprintf(Str(Cool) $format, *@args) {   # RakuAST doesn't do multis yet
    Formatter.new($format)(@args)
}
