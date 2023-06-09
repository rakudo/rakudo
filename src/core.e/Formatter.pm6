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
# TODO:
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
        | <?[%]> [ <directive>
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
    token directive:sym<%> { '%%' }

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

        # helper sub to check if a flag is set
        sub has_hash($/)  { "#" (elem) $<flags>.map: *.Str }
        sub has_minus($/) { "-" (elem) $<flags>.map: *.Str }
        sub has_plus($/)  { "+" (elem) $<flags>.map: *.Str }
        sub has_space($/) { " " (elem) $<flags>.map: *.Str }
        sub has_zero($/)  { "0" (elem) $<flags>.map: *.Str }

        # helper sub for creating literal integer nodes
        sub literal-integer(Int:D $int --> RakuAST::IntLiteral:D) {
            RakuAST::IntLiteral.new($int)
        }

        # helper sub for creating literal string nodes
        sub literal-string(Str:D $string --> RakuAST::StrLiteral:D) {
            RakuAST::StrLiteral.new($string)
        }

        # helper sub to call a method on a given AST
        proto sub ast-call-method(|) {*}
        multi sub ast-call-method($ast, $name --> RakuAST::ApplyPostfix:D) {
            RakuAST::ApplyPostfix.new(
              operand => $ast,
              postfix => RakuAST::Call::Method.new(
                name => RakuAST::Name.from-identifier($name)
              )
            )
        }
        multi sub ast-call-method(
          $ast, $name, $one
        --> RakuAST::ApplyPostfix:D) {
            RakuAST::ApplyPostfix.new(
              operand => $ast,
              postfix => RakuAST::Call::Method.new(
                name => RakuAST::Name.from-identifier($name),
                args => RakuAST::ArgList.new($one)
              )
            )
        }
        multi sub ast-call-method(
          $ast, $name, $one, $two
        --> RakuAST::ApplyPostfix:D) {
            RakuAST::ApplyPostfix.new(
              operand => $ast,
              postfix => RakuAST::Call::Method.new(
                name => RakuAST::Name.from-identifier($name),
                args => RakuAST::ArgList.new($one, $two)
              )
            )
        }

        # helper sub to call a sub with the given parameters
        proto sub ast-call-sub(|) {*}
        multi sub ast-call-sub($name, $one --> RakuAST::Call::Name:D) {
            RakuAST::Call::Name.new(
              name => RakuAST::Name.from-identifier($name),
              args => RakuAST::ArgList.new($one)
            )
        }
        multi sub ast-call-sub($name, $one, $two --> RakuAST::Call::Name:D) {
            RakuAST::Call::Name.new(
              name => RakuAST::Name.from-identifier($name),
              args => RakuAST::ArgList.new($one, $two)
            )
        }
        multi sub ast-call-sub(
          $name, $one, $two, $three
        --> RakuAST::Call::Name:D) {
            RakuAST::Call::Name.new(
              name => RakuAST::Name.from-identifier($name),
              args => RakuAST::ArgList.new($one, $two, $three)
            )
        }

        # helper sub to call an infix operator
        sub ast-infix($left, $infix, $right --> RakuAST::ApplyInfix:D) {
            RakuAST::ApplyInfix.new(
              left  => $left,
              infix => RakuAST::Infix.new($infix),
              right => $right
            )
        }

        # helper sub to call a prefix operator
        sub ast-prefix($prefix, $operand --> RakuAST::ApplyPrefix:D) {
            RakuAST::ApplyPrefix.new(
              prefix  => RakuAST::Prefix.new($prefix),
              operand => $operand
            )
        }

        # helper sub to create a ternary
        sub ast-ternary($condition, $then, $else --> RakuAST::Ternary) {
            RakuAST::Ternary.new(:$condition, :$then, :$else)
        }

        # helper sub to get size specification
        sub size($/ --> RakuAST::Node:D) { any-size($<size>) }

        # helper sub to get precision specification
        sub precision($/ --> RakuAST::Node:D) { any-size($<precision><size>) }

        # helper sub to get any size-type info.  Note that this will "eat"
        # parameters if a * is specified, indicating runtime width info.
        sub any-size($/ --> RakuAST::Node:D) {
            $/
              ?? $<star>
                ?? ast-call-method(parameter($/), 'Int')
                !! $/.Int > 1
                  ?? literal-integer($/.Int)
                  !! Nil
              !! Nil
        }

        # helper sub to determine the value for this directive
        sub parameter($/ --> RakuAST::Node:D) {
            my Int $index = $<idx> ?? $<idx>.chop.Int - 1 !! $*NEXT-PARAMETER;
            X::Str::Sprintf::Directives::Unsupported.new(
              directive => ~$<idx>,
              sequence  => ~$/,
            ).throw if $index < 0;

            # set default index for next parameter
            $*NEXT-PARAMETER = $index + 1;

            # record the directive
            @*DIRECTIVES[$index] = .Str with $<sym> // '*';

            # Set up fetcher, .head() is 20% faster so use that if we can
            $index
              # @args.AT-POS($index)
              ?? ast-call-method(
                   RakuAST::Var::Lexical.new('@args'),
                   'AT-POS',
                   literal-integer($index)
                 )
              # @args.head
              !! ast-call-method(
                   RakuAST::Var::Lexical.new('@args'),
                   'head',
                 )
        }

        # helper sub for processing formats for integer values
        sub handle-integer-numeric($/,
           Int:D :$base!,    # the number base to assume for generating string
           Str   :$hash,     # the string to prefix if "#" is in format
          Bool   :$plus,     # whether to prefix "+" if positive
          Bool   :$space,    # whether to prefix " " if not starting with + or -
          Bool   :$lc        # whether to lowercase resulting string
        ) {

# Please note that the order in which parameters are fetched, is following the
# way that Perl is doing.  From a left-to-right point of view, it feels that
# the $ast one should be the first.

            # set up size / precision specification
            my $size      := size($/);
            my $precision := precision($/);
            my $parameter := parameter($/);

            if !$precision && $size {
                if has_zero($/) && !has_minus($/) {
                    $precision := $size;
                    $size      := Nil;
                }
#                else {
#                    $precision = literal-integer(1);
#                }
            }

            # parameter($/).Int
            my $ast := ast-call-sub('intify', $parameter);

            # $ast.(Str || .base($base))
            $ast := $base == 10
              ?? ast-call-method($ast, 'Str')
              !! ast-call-method($ast, 'base', literal-integer($base));

            # $ast.lc
            $ast := ast-call-method($ast, 'lc') if $lc;

            # handle any prefixes
            my int $minus;
            if $hash && has_hash($/) {
                if $hash eq '0' {   # only for octal
                    # prefix-zero($ast)
                    $ast  := ast-call-sub('prefix-zero', $ast);
                    $minus = 1;
                }
                else {
                    # parameter ?? prefix-hash('$hash',$ast) !! $ast
                    $ast := ast-ternary(
                      $parameter,
                      ast-call-sub(
                        'prefix-hash', literal-string($hash), $ast
                      ),
                      $ast
                    );
                    $minus = $hash.chars;
                }
            }

            my $prefix;
            if $plus && has_plus($/) {
                $prefix = 'prefix-plus';
                $minus  = 1;
            }
            elsif $space && has_space($/) {
                $prefix = 'prefix-space';
                $minus  = 1;
            }

            # expand to precision indicated
            if $precision {
                my $width := $prefix
                  # $precision - 1
                  ?? ast-infix($precision, "-", literal-integer(1))
                  !! $precision;

                # pad-zeroes-int(
                #   $parameter ?? ($precision - $minus) !! $precision, $ast
                # )
                $ast := ast-call-sub(
                  'pad-zeroes-int',
                  ast-ternary(
                    $parameter,
                    $minus
                      ?? ast-infix($width, "-", literal-integer($minus))
                      !! $width,
                    $width
                  ),
                  $ast
                );
            }

            # $prefix($ast)
            $ast := ast-call-sub($prefix, $ast) if $prefix;

            # handle justification only if we need to
            if $size {
                # str-(left|right)-justified($precision, $ast)
                $ast := ast-call-sub(
                  has_minus($/)
                    ?? "str-left-justified"
                    !! "str-right-justified",
                  $size,
                  $ast
                );
            }

            # Set up special handling of 0 if 0 precision
            if !$precision && $<precision><size> {
                # parameter ?? $ast !! ""
                $ast := ast-ternary(
                  $parameter,
                  $ast,
                  RakuAST::StrLiteral.new(""),
                );
            }

            $ast
        }

        # helper sub for float values handling plus/minus/zero padding
        sub plus-minus-zero($/, $size, $ast is copy) {

            if has_plus($/) {
                # prefix-plus($ast)
                $ast = ast-call-sub('prefix-plus', $ast);
            }

            if $size {
                # justification($size, $ast)
                $ast = ast-call-sub(
                  has_minus($/)
                    ?? 'str-left-justified'
                    !! has_zero($/)
                      ?? "pad-zeroes-int"
                      !! "str-right-justified",
                  $size,
                  $ast
                );
            }

            $ast
        }

        method statement($/ --> Nil){
            make ($<directive> || $<literal>).made;
        }

        method literal($/ --> Nil) {
            make literal-string($/.Str);
        }

        # show numeric value in binary
        method directive:sym<b>($/ --> Nil) {
            make handle-integer-numeric(
              $/, :base(2), :plus, :space, :hash("0$<sym>")
            );
        }

        # show character representation of codepoint value
        method directive:sym<c>($/ --> Nil) {
            # chrify(parameter)
            my $ast := ast-call-sub('chrify', parameter($/));

            if size($/) -> $size {
                # str-(left|right)-justified($size, $ast)
                $ast := ast-call-sub(
                  has_minus($/)
                    ?? "str-left-justified"
                    !! has_zero($/)
                      ?? "pad-zeroes-str"
                      !! "str-right-justified",
                  $size,
                  $ast
                );
            }

            make $ast;
        }

        # show decimal (integer) value
        method directive:sym<d>($/ --> Nil) {
            make handle-integer-numeric($/, :base(10), :plus, :space);
        }

        # show floating point value, scientific notation
        method directive:sym<e>($/ --> Nil) {
            my $size      := size($/);
            my $precision := precision($/) // literal-integer(6);

            # scientify($precision,$ast)
            my $ast := ast-call-sub(
              'scientify', literal-string($<sym>.Str), $precision, parameter($/)
            );

            make plus-minus-zero($/, $size, $ast);
        }

        # show floating point value
        method directive:sym<f>($/ --> Nil) {
            my $size      := size($/);
            my $precision := precision($/) // literal-integer(6);

            # parameter.Numeric
            my $ast := ast-call-method(parameter($/), 'Numeric');

            # $ast.round(10 ** -$precision)
            $ast := ast-call-method(
              $ast,
              'round',
              ast-infix(literal-integer(10), '**', ast-prefix('-', $precision))
            );

            # $ast.Str
            $ast := ast-call-method($ast, 'Str');

            make plus-minus-zero($/, $size, $ast);
        }

        # f or e depending on value
        method directive:sym<g>($/ --> Nil) {
            self."directive:sym<f>"($/);  # for now
        }

        # show numeric value in octal using Perl / Raku semantics
        method directive:sym<o>($/ --> Nil) {
            make handle-integer-numeric($/, :base(8), :hash<0>);
        }

        # show string
        method directive:sym<s>($/ --> Nil) {
            my $size      := size($/);
            my $precision := precision($/);

            # $ast.Str
            my $ast := ast-call-method(parameter($/), 'Str');

            if $precision {
                # $ast.substr(0,$-precision)
                $ast := ast-call-method(
                  $ast, 'substr', literal-integer(0), $precision
                );
            }

            if $size {
                # str-(left|right)-justified($size, $ast)
                $ast := ast-call-sub(
                  has_minus($/)
                    ?? "str-left-justified"
                    !! "str-right-justified",
                  $size,
                  $ast
                );
            }

            make $ast;
        }

        # show unsigned decimal (integer) value
        method directive:sym<u>($/ --> Nil) {
            my $size := size($/);
            my $ast  := ast-call-sub(
              "unsigned-int", literal-string($*FORMAT), parameter($/)
            );

            # handle zero padding / left / right justification
            if $size {
                $ast := ast-call-sub(
                  has_minus($/)
                    ?? 'str-left-justified'
                    !! has_zero($/)
                      ?? 'pad-zeroes-int'
                      !! 'str-right-justified',
                  $size, $ast
                );
            }

            make $ast;
        }

        # show numeric value in hexadecimal
        method directive:sym<x>($/ --> Nil) {
            make handle-integer-numeric(
              $/, :base(16), :hash("0$<sym>"), :lc($<sym> eq "x")
            )
        }

        method directive:sym<%>($/ --> Nil) {
            make literal-string('%');
        }
    }

    # RUNTIME: wrong number of args thrower
    sub throw-count(
      str $format,
      int $args-have,
      int $args-used
    ) is hidden-from-backtrace {
        X::Str::Sprintf::Directives::Count.new(
          :$format, :$args-have, :$args-used
        ).throw;
    }

    # RUNTIME: wrong type of arg thrower
    sub throw-type(
      str $format, str $directive, str $expected, Mu $value
    ) is hidden-from-backtrace {
        X::Str::Sprintf::Directives::BadType.new(
          :$format, :$directive, :$expected, :type($value.^name), :$value
        ).throw
    }

    # RUNTIME number of arguments checker
    sub check-args(str $format, @args, @directives--> Nil) {
        my $args       := nqp::getattr(@args,List,'$!reified');
        my $directives := nqp::getattr(@directives,List,'$!reified');

        # number of args doesn't match
        if nqp::not_i(nqp::isconcrete($args))
          || nqp::isne_i(nqp::elems($args),nqp::elems($directives)) {
            throw-count(
              $format,
              nqp::elems($args),
              nqp::elems($directives)
            )
        }

        # check types
        else {
            my int $i = -1;
            nqp::while(
              nqp::islt_i(($i = nqp::add_i($i,1)),nqp::elems($args)),
              nqp::unless(
                Cool.ACCEPTS(nqp::atpos($args,$i)),
                throw-type($format,
                  nqp::atpos($directives,$i), "Cool", nqp::atpos($args,$i)
                )
              )
            );
        }
    }

    # RUNTIME number of arguments checker for single expected argument
    sub check-one-arg(str $format, @args, Str:D $directive --> Nil) {
        my $args := nqp::getattr(@args,List,'$!reified');

        # number of args matches, check type
        if nqp::isconcrete($args) && nqp::iseq_i(nqp::elems($args),1) {
            throw-type($format,
              $directive, "Cool", nqp::atpos($args,0)
            ) unless Cool.ACCEPTS(nqp::atpos($args,0))
        }
        # number of args doesn't match
        else {
            throw-count(
              $format,
              $directive,
              nqp::isconcrete($args) ?? nqp::elems($args) !! 0,
              1
            )
        }
    }

    # RUNTIME number of arguments checker for NO expected argument
    sub check-no-arg(str $format, @args --> Nil) {
        my $args := nqp::getattr(@args,List,'$!reified');
        throw-count($format, '', nqp::elems($args), 0)
          if nqp::isconcrete($args) && nqp::elems($args);
    }

    # RUNTIME check if value is positive integer and stringify
    proto sub unsigned-int(|) {*}
    multi sub unsigned-int(Str:D $format, $arg) {
        unsigned-int($format, $arg.Numeric.Int)
    }
    multi sub unsigned-int(Str:D $format, Int:D $arg) {
        nqp::islt_I($arg,0)
          ?? throw-type($format, "u", "UInt", $arg)
          !! $arg.Str
    }

    # RUNTIME prefix space if string not starting with "+" or "-"
    sub prefix-space(str $string) {
        nqp::eqat($string,'-',0) || nqp::eqat($string,'+',0)
          ?? $string
          !! nqp::concat(' ',$string)
    }

    # RUNTIME prefix 0 if string not starting with 0
    sub prefix-zero(str $string) {
        nqp::eqat($string,'0',0)
          ?? $string
          !! nqp::eqat($string,'-',0)
            ?? nqp::eqat($string,'0',1)
              ?? $string
              !! nqp::concat('-0',nqp::substr($string,1))
            !! nqp::concat('0',$string)
    }

    # RUNTIME prefix plus if value is not negative
    sub prefix-plus(str $string) {
        nqp::eqat($string,'-',0)
          ?? $string
          !! nqp::concat("+",$string)
    }

    # RUNTIME prefix given hash properly, also if value negative
    sub prefix-hash(str $hash, str $string) {
        nqp::eqat($string,'-',0)
          ?? nqp::concat('-',nqp::concat($hash,nqp::substr($string,1)))
          !! nqp::concat($hash,$string)
    }

    # RUNTIME pad with zeroes as integer
    sub pad-zeroes-int(int $positions, str $string) {
        nqp::isgt_i($positions,0)
          ?? nqp::islt_i(nqp::chars($string),$positions)
            ?? nqp::eqat($string,'-',0)
              ?? nqp::concat('-',pad-zeroes-str(
                   nqp::sub_i($positions,1),nqp::substr($string,1)
                 ))
              !! pad-zeroes-str($positions,$string)
            !! $string
          !! $string
    }

    # RUNTIME pad with zeroes after decimal point
    sub pad-zeroes-precision(int $positions, str $string) {
        my int $index = nqp::index($string,'.');
        if nqp::isge_i($index,0) {
            my int $digits =  # $string.chars - 1 - $index;
              nqp::sub_i(nqp::sub_i(nqp::chars($string),1),$index);

            nqp::isgt_i($positions,$digits)
              ?? nqp::concat(
                   $string,
                   nqp::x('0',nqp::sub_i($positions,$digits))
                 )
              !! $string
        }
        else {
            $positions
              ?? nqp::concat('.',nqp::x('0',$positions))
              !! $string
        }
    }

    # RUNTIME pad with zeroes as string
    sub pad-zeroes-str(int $positions, str $string) {
        my int $chars = nqp::chars($string);
        nqp::islt_i(nqp::chars($string),$positions)
          ?? nqp::concat(
               nqp::x('0',nqp::sub_i($positions,nqp::chars($string))),
               $string
             )
          !! $string
    }

    # RUNTIME create .chr of given value
    proto sub intify(|) {*}
    multi sub intify(Int:D $value) { $value             }
    multi sub intify(      $value) { $value.Numeric.Int }

    # RUNTIME create .chr of given value
    proto sub chrify(|) {*}
    multi sub chrify(Int:D $value) { $value.chr         }
    multi sub chrify(      $value) { $value.Numeric.chr }

    # RUNTIME set up value for scientific notation
    proto sub scientify(|) {*}
    multi sub scientify($letter, $positions, $value --> Str:D) {
        scientify($letter, $positions, $value.Numeric)
    }
    multi sub scientify($letter, $positions, Numeric:D $value --> Str:D) {
        if $value {
            my $exponent := $value ?? $value.abs.log(10).floor !! 0;
            my $abs-expo := $exponent.abs;
            pad-zeroes-precision(
              $positions,
              ($value / 10 ** $exponent).round(10**-$positions).Str
            ) ~ $letter
              ~ ($exponent < 0 ?? "-" !! "+")
              ~ ($abs-expo < 10 ?? "0" ~ $abs-expo !! $abs-expo)
        }
        else {
            "0." ~ nqp::x("0",$positions) ~ $letter ~ "+00"
        }
    }

    # RUNTIME string, left justified
    sub str-left-justified(int $positions, str $string) {
        nqp::islt_i($positions,0)
          ?? str-left-justified(-$positions, $string)  # -* with negative width
          !! nqp::islt_i(nqp::chars($string),$positions)
            ?? nqp::concat(
                 $string,
                 nqp::x(' ',nqp::sub_i($positions,nqp::chars($string)))
               )
            !! $string
    }
    # RUNTIME string, right justified
    sub str-right-justified(int $positions, str $string) {
        nqp::islt_i($positions,0)
          ?? str-left-justified(-$positions, $string)  # * with negative width
          !! nqp::islt_i(nqp::chars($string),$positions)
            ?? nqp::concat(
                 nqp::x(' ',nqp::sub_i($positions,nqp::chars($string))),
                 $string
               )
            !! $string
    }

    # Return AST for given format
    method AST(Str(Cool) $format) {
        my $*FORMAT        := $format;  # for error message generation
        my @*DIRECTIVES;                # the directives seen
        my $*NEXT-PARAMETER = 0;        # index of next parameter to be expected

        if Syntax.parse($format, actions => Actions) -> $parsed {
            my @parts = $parsed<statement>.map: *.made;
            my $ast;

            # at least one directive
            if @*DIRECTIVES -> @directives {
                if @directives == 1 {

                    # check-one-arg(@args, @directives[0])
                    $ast := RakuAST::Call::Name.new(
                      name => RakuAST::Name.from-identifier('check-one-arg'),
                      args => RakuAST::ArgList.new(
                        RakuAST::StrLiteral.new($format),
                        RakuAST::Var::Lexical.new('@args'),
                        RakuAST::StrLiteral.new(@directives[0])
                      )
                    );
                }
                else {

                    # infix:<,>(@directives.map( { "$_" } ))
                    $ast := RakuAST::ApplyListInfix.new(
                      infix    => RakuAST::Infix.new(','),
                      operands => [@directives.map( {
                        RakuAST::StrLiteral.new($_ || '')
                      } )]
                    );

                    # check-args($format, @args, $ast)
                    $ast := RakuAST::Call::Name.new(
                      name => RakuAST::Name.from-identifier('check-args'),
                      args => RakuAST::ArgList.new(
                        RakuAST::StrLiteral.new($format),
                        RakuAST::Var::Lexical.new('@args'),
                        RakuAST::Statement::Expression.new(:expression($ast))
                      )
                    );
                }

                if @parts == 1 {

                    # $ast; @parts[0]
                    $ast := RakuAST::StatementList.new(
                     RakuAST::Statement::Expression.new(:expression($ast)),
                      RakuAST::Statement::Expression.new(:expression(@parts[0]))
                    );
                }
                else {

                    # $ast; @parts.join
                    $ast := RakuAST::StatementList.new(
                      RakuAST::Statement::Expression.new(:expression($ast)),
                      RakuAST::Statement::Expression.new(:expression(
                        RakuAST::ApplyPostfix.new(
                          operand => RakuAST::ApplyListInfix.new(
                            infix     => RakuAST::Infix.new(','),
                            operands  => @parts
                          ),
                          postfix => RakuAST::Call::Method.new(
                            name => RakuAST::Name.from-identifier('join')
                          )
                        )
                      ))
                    );
                }
            }

            # no directives, just a string
            else {
                # check-no-arg($format, @args); $format.subst('%%','%',:g)
                $ast := RakuAST::StatementList.new(
                  RakuAST::Statement::Expression.new(:expression(
                    RakuAST::Call::Name.new(
                      name => RakuAST::Name.from-identifier('check-no-arg'),
                      args => RakuAST::ArgList.new(
                        RakuAST::StrLiteral.new($format),
                        RakuAST::Var::Lexical.new('@args')
                      )
                    )
                  )),
                  RakuAST::Statement::Expression.new(:expression(
                    # If there are no directives, it generally is just
                    # the format string, with one exception: if there
                    # are '%%' escapes.  Instead of picking @parts apart
                    # then, just escape the '%%' here string-wise.
                    RakuAST::StrLiteral.new($format.subst('%%','%',:g))
                  ))
                );
            }

            # -> @args { $ast }
            $ast := RakuAST::PointyBlock.new(
              signature => RakuAST::Signature.new(
                parameters => (
                  RakuAST::Parameter.new(
                    target => RakuAST::ParameterTarget::Var.new('@args')
                  ),
                )
              ),
              body => RakuAST::Blockoid.new($ast)
            );
            $ast
        }
        else {
            die "huh?"
        }
    }

    # Return Callable for given format
    method CODE(Str(Cool) $format --> Callable:D) { EVAL self.AST($format) }

    # actual workhorse for sprintf()
    my $FORMATS := nqp::hash;  # where we keep our formats
    method new(Str:D $format) {
        nqp::ifnull(
          nqp::atkey($FORMATS,$format),
          self!fetch-new-format($format)
        )
    }

    # Threadsafe cache updater, don't care about multiple threads trying
    # to do the same format, but this way we don't have a lock if the
    # same format is called *many* times over
    method !fetch-new-format(Str:D $format) {
        my $new := nqp::clone($FORMATS);

        # remove the first key we encounter if max reached
        nqp::deletekey($new,nqp::iterkey_s(nqp::shift(nqp::iterator($new))))
          if nqp::isge_i(nqp::elems($new),100);  # XXX  should be settable

        nqp::bindkey($new,$format,my $code := self.CODE($format));
        $FORMATS := $new;
        $code
    }
}

# the procedural frontend of sprintf functionality
proto sub zprintf($, |) {*}
multi sub zprintf(Str(Cool) $format, \value) {
    Formatter.new($format)(nqp::list(value))
}
multi sub zprintf(Str(Cool) $format, @args) {
    Formatter.new($format)(@args)
}
multi sub zprintf(Str(Cool) $format, *@args) {
    Formatter.new($format)(@args)
}
multi sub zprintf(&code, *@args) {
    code(@args)
}

augment class Cool {
    method zprintf(*@args) { zprintf(self, @args) }
}

# vim: expandtab shiftwidth=4
