# THEORY OF OPERATION
# -------------------
# The "new" method will look for the given format in a hash.  If found,
# it contains a Callable that will process the parameters doing all of the
# necessary checks and conversions.
#
# If not found, it will parse the format with the Formatter::Syntax grammar
# and the Formatter::Actions class, which will create the RakuAST nodes for
# it and store the Callable in the hash.
#
# TODO:
# - generate code that uses native ops and variables where possible

#- Formatter -------------------------------------------------------------------
# Class containing actual format -> AST mapping logic

our class Formatter {

#-------------------------------------------------------------------------------
# Subroutines referenced at runtime by the generated ASTs, to reduce the
# actual size of the specific parts of sprintf processing.

    # Pad with zeroes as integer, keeping any - as first character intact
    # and taking it into account for the padding
    our sub pad-zeroes-int(int $positions, str $string --> str) {
        nqp::isle_i($positions,0)
          || nqp::isge_i(nqp::chars($string),$positions)
          ?? $string                                # nothing to do
          !! nqp::eqat($string,'-',0)
            ?? nqp::concat(                         # preserve -
                 "-",
                 pad-zeroes-str(
                   nqp::sub_i($positions,1),nqp::substr($string,1)
                 )
               )
            !! pad-zeroes-str($positions,$string)   # just pad out
    }

    # Pad with zeroes as integer, keeping any - as first character intact
    # and *not* taking any minus into account for padding
    our sub pad-zeroes-int-strict(int $positions, str $string --> str) {
        nqp::isle_i($positions,0)
          || nqp::isgt_i(nqp::chars($string),$positions)
          ?? $string                                # nothing to do
          !! nqp::eqat($string,'-',0)
            ?? nqp::concat(                         # preserve -
                 "-",
                 pad-zeroes-str($positions,nqp::substr($string,1))
               )
            !! pad-zeroes-str($positions,$string)   # just pad out
    }

    # Pad with zeroes as integer, specifying the number of *digits* to
    # generate (excluding any sign symbol)
    our sub signer-pad-zeroes-int(
      str $signer,  # sign symbol: "", " " or "+"
      int $digits,  # number of digits that should be generated at least
      str $string   # the stringified version of the value so far
    --> str) {
        my int $minus = nqp::eqat($string,'-',0);
        nqp::isle_i($digits,0)
          || nqp::isge_i(nqp::chars($string),$digits)
          ?? $minus                         # too wide
            ?? $string                       # nothing to do, already has sign
            !! nqp::concat($signer,$string)  # just prepend any signer
          !! $minus                         # still room
            ?? nqp::concat(                  # negative
                 "-",
                 pad-zeroes-str($digits,nqp::substr($string,1))
               )
            !! nqp::concat($signer,pad-zeroes-str($digits,$string))
    }

    # Pad with zeroes as integer, specifying the number of *characters* to
    # generate (including any sign symbol)
    our sub pad-signer-zeroes-int(
      int $digits,  # number of digits that should be generated at least
      str $signer,  # sign symbol: "", " " or "+"
      str $string   # the stringified version of the value so far
    --> str) {
        my int $minus = nqp::eqat($string,'-',0);

        nqp::isle_i($digits,0)
          || nqp::isge_i(nqp::chars($string),$digits)
          ?? $minus                              # too wide
            ?? $string                            # nothing todo, has sign
            !! nqp::concat($signer,$string)       # just prepend any signer
          !! $minus                              # still room
            ?? nqp::concat(                       # account for existing sign
                 "-",
                 pad-zeroes-str(nqp::sub_i($digits,1), nqp::substr($string,1))
               )
            !! $signer                            # has a signer
              ?? nqp::concat(                      # take signer into account
                   $signer,
                   pad-zeroes-str(nqp::sub_i($digits,1),$string)
                 )
              !! pad-zeroes-str($digits, $string)  # just pad
    }

    # pad with zeroes after decimal point
    our sub pad-zeroes-precision(int $positions, str $string --> str) {
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
              ?? nqp::concat($string,nqp::concat('.',nqp::x('0',$positions)))
              !! $string
        }
    }

    # pad with zeroes as string
    our sub pad-zeroes-str(int $positions, str $string --> str) {
        nqp::islt_i(nqp::chars($string),$positions)
          ?? nqp::concat(
               nqp::x('0',nqp::sub_i($positions,nqp::chars($string))),
               $string
             )
          !! $string
    }

    # prefix given hash properly, also if value negative or starting with
    # a + or - or space
    our sub prefix-hash(str $hash, str $string --> str) {
        nqp::eqat($string,'-',0)
          ?? nqp::concat(
               nqp::substr($string,0,1),
               nqp::concat($hash,nqp::substr($string,1))
             )
          !! nqp::concat($hash,$string)
    }

    # prefix sign symbol if value is not negative or zero
    our sub prefix-signer(str $signer, str $string --> str) {
        nqp::eqat($string,'-',0)
          ?? $string
          !! nqp::concat($signer,$string)
    }

    # prefix plus if value is not negative
    our sub prefix-plus(str $string --> str) {
        nqp::eqat($string,'-',0)
          ?? $string
          !! nqp::concat("+",$string)
    }

    # Prefix signer character if string not starting with "-",
    # and replace leading zero with signer, or just prefix it
    our sub prefix-space(str $string --> str) {
        nqp::eqat($string,'-',0)
          ?? $string
          !! nqp::concat(' ',$string)
    }

    # set up value for scientific notation
    our proto sub scientify(|) {*}
    multi sub scientify($letter, $positions, $value) {
        scientify($letter, $positions, $value.Numeric)
    }
    multi sub scientify($letter, $positions, Numeric:D $value) {
        if $value {
            my int $exponent = $value ?? $value.abs.log(10).floor !! 0;
            my str $string =
              ($value / 10 ** $exponent).round(10 ** -$positions).Str;
            my int $end = nqp::chars($string) - 1;

            if nqp::ord($string,$end) == 48 {  # "0"
                $string = nqp::substr($string,0,$end);
                ++$exponent;
            }

            my int $abs-expo = nqp::abs_i($exponent);
            pad-zeroes-precision(
              $positions,
              $string,
            ) ~ $letter
              ~ ($exponent < 0 ?? "-" !! "+")
              ~ ($abs-expo < 10 ?? "0" ~ $abs-expo !! $abs-expo)
        }
        else {
            "0." ~ nqp::x("0",$positions) ~ $letter ~ "+00"
        }
    }

    # provide left justification of string
    our sub str-left-justified(int $positions, str $string --> str) {
        nqp::islt_i(nqp::chars($string),nqp::abs_i($positions))
          ?? nqp::concat(
               $string,nqp::x(
                 ' ',nqp::sub_i(nqp::abs_i($positions),nqp::chars($string))
               )
             )
          !! $string
    }

    # provide right justification of string
    our sub str-right-justified(int $positions, str $string --> str) {
        nqp::islt_i($positions,0)
          ?? str-left-justified($positions, $string)
          !! nqp::islt_i(nqp::chars($string),$positions)
            ?? nqp::concat(
                 nqp::x(' ',nqp::sub_i($positions,nqp::chars($string))),
                 $string
               )
            !! $string
    }

#-------------------------------------------------------------------------------
# Actions class to be used with Grammar to turn format into array of piecesi
# of code and have the results joined into a single string.

    my class Actions {

#-------------------------------------------------------------------------------
# Helper subroutines that generate RakuAST::Nodes.  These all have the "ast"
# prefix and should only be called at format parse time.  These methods are
# agnostic of the grammar / actions.

        # helper sub to see whether an AST is a literal Int
        sub is-literal-int($ast --> int) {
            nqp::istype($ast,RakuAST::IntLiteral)
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
              name => RakuAST::Name.from-identifier-parts('Formatter', $name),
              args => RakuAST::ArgList.new($one)
            )
        }
        multi sub ast-call-sub($name, $one, $two --> RakuAST::Call::Name:D) {
            RakuAST::Call::Name.new(
              name => RakuAST::Name.from-identifier-parts('Formatter', $name),
              args => RakuAST::ArgList.new($one, $two)
            )
        }
        multi sub ast-call-sub(
          $name, $one, $two, $three
        --> RakuAST::Call::Name:D) {
            RakuAST::Call::Name.new(
              name => RakuAST::Name.from-identifier-parts('Formatter', $name),
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

        # helper sub for creating literal integer nodes
        sub ast-integer(Int:D $int --> RakuAST::IntLiteral:D) {
            RakuAST::IntLiteral.new($int)
        }

        # helper sub to call a prefix operator
        sub ast-prefix($prefix, $operand --> RakuAST::ApplyPrefix:D) {
            RakuAST::ApplyPrefix.new(
              prefix  => RakuAST::Prefix.new($prefix),
              operand => $operand
            )
        }

        # helper sub for creating literal string nodes
        sub ast-string(Str:D $string --> RakuAST::StrLiteral:D) {
            RakuAST::StrLiteral.new($string)
        }

        # helper sub to create a ternary
        sub ast-ternary($condition, $then, $else --> RakuAST::Ternary) {
            RakuAST::Ternary.new(:$condition, :$then, :$else)
        }

        # helper sub to create ast from ast and integer value to be subtracted,
        # constant fold as much as possible
        sub ast-sub-integer($ast, Int:D $value) {
            $value
              ?? is-literal-int($ast)
                ?? ast-integer($ast.value - $value)          # constant fold
                !! ast-infix($ast, "-", ast-integer($value)) # runtime fold
              !! $ast
        }

        # helper sub to create an NQP op
        sub ast-nqp($op, *@args) { RakuAST::Nqp.new($op, |@args) }

#-------------------------------------------------------------------------------
# Helper subs that obtain information from Match objects.  These always take
# $/ as the first positional parameter.

        # helper sub to check if a flag is set
        sub has-hash($/)  { "#" (elem) $<flags>.map: *.Str }
        sub has-minus($/) { "-" (elem) $<flags>.map: *.Str }
        sub has-plus($/)  { "+" (elem) $<flags>.map: *.Str }
        sub has-space($/) { " " (elem) $<flags>.map: *.Str }
        sub has-zero($/)  { "0" (elem) $<flags>.map: *.Str }

        # helper sub to get size specification, size 0 is ignored
        sub size($/ --> RakuAST::Node:D) {
            is-literal-int(my $size := any-size($<size>))
              ?? $size.value > 0
                ?? $size
                !! Nil
              !! $size
        }

        # helper sub to get precision specification
        sub precision($/ --> RakuAST::Node:D) {
            any-size($<precision><size>)
        }

        # helper sub to get any size-type info.  Note that this will "eat"
        # parameters if a * is specified, indicating runtime width info.
        sub any-size($/ --> RakuAST::Node:D) {
            $/
              ?? $<star>
                ?? parameter($/, :coerce<Int>)
                !! ast-integer($/.Int)
              !! Nil
        }

        # helper sub to determine the value for this directive
        sub parameter($/, :$coerce --> RakuAST::Node:D) {
            my Int $index = $<idx> ?? $<idx>.chop.Int !! $*NEXT-PARAMETER;
            X::Str::Sprintf::Directives::Unsupported.new(
              directive => ~$<idx>,
              sequence  => ~$/,
            ).throw if $index < 1;

            # set default index for next parameter
            $*NEXT-PARAMETER = $index + 1;

            # record the directive, * indicates a position indicator (e.g. 4$)
            @*DIRECTIVES[$index] = .Str with $<sym> // '*';
            @*COERCIONS[$index]  = $coerce if $coerce;

            my $letter = "a";
            $letter++ while --$index;
            RakuAST::Var::Lexical.new('$' ~ $letter)
        }

        # helper sub for float values handling plus/minus/zero padding
        sub plus-minus-zero($/, $size, $ast is copy) {

            if has-plus($/) {
                # prefix-plus($ast)
                $ast = ast-call-sub('prefix-plus', $ast);
            }

            if $size {
                # justification($size, $ast)
                $ast = ast-call-sub(
                  has-minus($/)
                    ?? 'str-left-justified'
                    !! has-zero($/)
                      ?? "pad-zeroes-int"
                      !! "str-right-justified",
                  $size,
                  $ast
                );
            }

            $ast
        }

        # Helper sub to obtain size / precision / parameter ASTs.
        # We first need to get any size/precision information because
        # they can be parameter based and should be specified *before*
        # the actual argument
        proto sub spa(|) {*}
        multi sub spa($/ --> List:D) {
            (size($/), precision($/), parameter($/))
        }
        multi sub spa($/, Int:D $default --> List:D) {
            (size($/), precision($/) // ast-integer($default), parameter($/))
        }

        # helper sub for processing formats for integer values
        sub handle-integer-numeric($/,
           Int :$base,    # the number base to assume for generating string
           Str :$hash,    # the string to prefix if "#" is in format
           Str :$coerce,  # method name to initially coerce with, default Int
          Bool :$plus,    # whether to prefix "+" if positive
          Bool :$space,   # whether to prefix " " if not starting with + or -
          Bool :$lc       # whether to lowercase resulting string
        ) {
            my $size      := size($/);
            my $precision := precision($/);
            my $parameter := parameter($/, :coerce($coerce // "Int"));

            # AST for handling when the value is zero
            my $zero := "0";

            # AST for handling when the value is NOT zero
            my $not-zero := $base && $base != 10
              ?? ast-call-method($parameter, 'base', ast-integer($base))
              !! ast-call-method($parameter, 'Str');

            # For lowercase versions number bases > 10
            $not-zero := ast-call-method($not-zero, 'lc') if $lc;

            # Set up width of any prefix (a # in the format)
            my int $prefix-width = $hash && has-hash($/)
              ?? $hash.chars
              !! 0;

            # The signer symbol is either a "+" or a " " which will
            # be prefixed if the value is not negative and there's
            # no # in the format
            my str $signer = "";
            unless $prefix-width {
                # Handling + and space
                if $plus && has-plus($/) {
                    $signer = "+";
                }
                elsif $space && has-space($/) {
                    $signer = " ";
                }
            }

            # A precision indicates the number of digits to be shown
            # filled out with 0's for the given precision.  It does
            # **not** take into account any prefix width or plus/space,
            # and will gladly revert to not showing any digit at all if
            # the precision is 0.
            if $precision {
                $zero := is-literal-int($precision)
                  ?? $signer ~ "0" x $precision.value
                  !! $signer
                    ?? ast-nqp("concat",
                         ast-string($signer),
                         ast-nqp("x","0",$precision)
                       )
                    !! ast-nqp("x","0",$precision);

                $not-zero := $prefix-width
                  ?? ast-call-sub('pad-zeroes-int-strict',
                       $precision, $not-zero
                     )
                  !! ast-call-sub('signer-pad-zeroes-int',
                       ast-string($signer), $precision, $not-zero
                     );
            }

            # Use of a size with a zero is *almost* the same as
            # precision, but has different handling for zero and
            # also takes into account any prefix, + or space in
            # its size padding logic.
            elsif $size && has-zero($/) && !has-minus($/)  {

                $zero := is-literal-int($size)
                  ?? (my int $width = $size.value)
                    ?? ($signer ~ "0" x ($width - $signer.chars))
                    !! $zero
                  !! $signer
                    ?? ast-nqp("if",
                         $size,
                         ast-nqp("concat",
                           ast-string($signer),
                           ast-nqp("x","0",ast-sub-integer($size,1))
                         ),
                         ast-string($signer)
                       )
                    !! ast-nqp("x","0",$size);

                $not-zero := $prefix-width
                  ?? ast-call-sub('pad-zeroes-int',
                       is-literal-int($size)
                         ?? ast-integer($size.value - $prefix-width)
                         !! ast-sub-integer($size, $prefix-width),
                       $not-zero
                     )
                  !! ast-call-sub('pad-signer-zeroes-int',
                       $size, ast-string($signer), $not-zero
                     );

                # No further sizing needed
                $size := Nil;
            }

            # A signer and no zeroed size and no precision
            elsif $signer {
                $zero     := $signer ~ "0";
                $not-zero := ast-call-sub('prefix-signer',
                  ast-string($signer), $not-zero
                );
            }

            # Handle any hash prefix.
            if $prefix-width {
                # prefix-hash($hash,$ast)
                $not-zero := ast-call-sub('prefix-hash',
                  ast-string($hash), $not-zero
                );
            }

            # Handle justification
            if $size {
                my $justifier := has-minus($/)
                  ?? "str-left-justified"
                  !! "str-right-justified";

                if is-literal-int($size) {
                    $zero := nqp::istype($zero,Str)
                      ?? ast-string(::("&$justifier")($size.value, $zero))
                      !! ast-call-sub($justifier, $size, $zero);

                    ast-ternary(
                      $parameter,
                      ast-call-sub($justifier, $size, $not-zero),
                      $zero
                    )
                }
                else {
                    $zero := ast-call-sub($justifier,
                      $size,
                      nqp::istype($zero,Str) ?? ast-string($zero) !! $zero
                    );
                    $not-zero := ast-call-sub(
                      $justifier,
                      $size,
                      ast-ternary($parameter, $not-zero, $zero)
                    );
                }
            }

            else {
                ast-ternary(
                  $parameter,
                  $not-zero,
                  nqp::istype($zero,Str) ?? ast-string($zero) !! $zero
                )
            }
        }

#-------------------------------------------------------------------------------
# These are the actual action methods that will be called when the associated
# token in the grammar matches.

        # collect all the statements made
        method statement($/ --> Nil){
            make ($<directive> || $<literal>).made;
        }

        # any non-format related string
        method literal($/ --> Nil) {
            make ast-string($/.Str);
        }

        # show numeric value in binary
        method directive:sym<b>($/ --> Nil) {
            make handle-integer-numeric(
              $/, :base(2), :hash("0$<sym>")
            );
        }

        # show character representation of codepoint value
        method directive:sym<c>($/ --> Nil) {
            my ($size, $precision, $parameter) := spa($/);

            # $a.chr
            my $ast := ast-call-method($parameter, 'chr');

            if $size {
                # str-(left|right)-justified($size, $ast)
                $ast := ast-call-sub(
                  has-minus($/)
                    ?? "str-left-justified"
                    !! has-zero($/)
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
            my ($size, $precision, $parameter) := spa($/, 6);

            # scientify($precision,'e',$a)
            my $ast := ast-call-sub(
              'scientify', ast-string($<sym>.Str), $precision, $parameter
            );

            make plus-minus-zero($/, $size, $ast);
        }

        # show floating point value
        method directive:sym<f>($/ --> Nil) {
            my ($size, $precision, $parameter) := spa($/, 6);

            # $a.Numeric
            my $ast := ast-call-method($parameter, 'Numeric');

            # $ast.round(10 ** -$precision)
            $ast := ast-call-method(
              $ast,
              'round',
              ast-infix(ast-integer(10), '**', ast-prefix('-', $precision))
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
            my ($size, $precision, $parameter) := spa($/);

            # make sure we have a (potentially truncated) string
            my $ast := $precision
                 # $a.substr(0,$precision)
              ?? ast-call-method(
                   $parameter, 'substr', ast-integer(0), $precision
                 )
                 # $a.Str
              !! ast-call-method($parameter, 'Str');

            # perform any justification
            $ast := ast-call-sub(
              'str-' ~ (has-minus($/) ?? 'left' !! 'right') ~ '-justified',
              $size,
              $ast
            ) if $size;

            make $ast;
        }

        # show unsigned decimal (integer) value
        method directive:sym<u>($/ --> Nil) {
            make handle-integer-numeric($/, :coerce<UInt>);
        }

        # show numeric value in hexadecimal
        method directive:sym<x>($/ --> Nil) {
            make handle-integer-numeric(
              $/, :base(16), :hash("0$<sym>"), :lc($<sym> eq "x")
            )
        }

        # an escaped %
        method directive:sym<%>($/ --> Nil) {
            make ast-string('%');
        }
    }

#-------------------------------------------------------------------------------
# The actual AST generation logic

    method AST(Str(Cool) $format) {

        # If we don't have a DIRECTIVES array yet, create one and call
        # ourselves again, now *with* the DIRECTIVES array being available.
        # This allows an external caller (such as Format.new) to set up
        # their own DIRECTIVES array and so be able to find out what the
        # directives were.
        if nqp::istype(@*DIRECTIVES,Failure) {
            my @*DIRECTIVES := my str @;  # the directives seen
            my @*COERCIONS  := my str @;  # any coercions on parameters
            return &?ROUTINE(self, $format);
        }

        # we're 1-based internally
        @*DIRECTIVES.unshift("");
        @*COERCIONS.unshift("");

        # Index of next parameter to be expected.  Note that we do this
        # 1-based rather than 0-based, for easier matching with position
        # specifications, which *are* 1-based.
        my $*NEXT-PARAMETER = 1;

        if Formatter::Syntax.parse($format, actions => Actions) -> $parsed {
            my @operands = $parsed<statement>.map: *.made;

            # 0-based from now on
            @*DIRECTIVES.shift;
            @*COERCIONS.shift;

            # at least one directive
            if @*DIRECTIVES -> @directives {

                # set up the statements
                my $stmts := RakuAST::StatementList.new(
                  RakuAST::Statement::Expression.new(
                    expression => @operands == 1
                      ?? @operands.head  # already stringified
                      !! RakuAST::ApplyPostfix.new(
                           operand => RakuAST::ApplyListInfix.new(
                             infix    => RakuAST::Infix.new(','),
                             operands => @operands
                           ),
                          postfix => RakuAST::Call::Method.new(
                            name => RakuAST::Name.from-identifier('join')
                          )
                        )
                  )
                );

                # set up the parameters list
                my $letter = "a";
                my @parameters = (^@directives).map: {

                    # set up any coercion logic
                    my $type;
                    if @*COERCIONS[$_] -> $coercion {
                        $type := RakuAST::Type::Coercion.new(
                          base-type => RakuAST::Type::Simple.new(
                            RakuAST::Name.from-identifier($coercion)
                          )
                        )
                    }

                    # set up the parameter
                    RakuAST::Parameter.new(
                      :$type,
                      target => RakuAST::ParameterTarget::Var.new(
                        :name('$' ~ $letter++)
                      )
                    )
                }

                # -> $a, $b, ... { $ast }
                RakuAST::PointyBlock.new(
                  signature => RakuAST::Signature.new(
                    parameters => @parameters.List
                  ),
                  body      => RakuAST::Blockoid.new($stmts)
                );
            }

            # no directives, just a string
            else {
                RakuAST::PointyBlock.new(
                  body => RakuAST::Blockoid.new(
                    RakuAST::StatementList.new(
                      RakuAST::Statement::Expression.new(:expression(
                        # If there are no directives, it generally is just
                        # the format string, with one exception: if there
                        # are '%%' escapes.  Instead of picking @parts apart
                        # then, just escape the '%%' here string-wise.
                        RakuAST::StrLiteral.new($format.subst('%%','%',:g))
                      ))
                    )
                  )
                )
            }
        }
        else {
            die "huh?"
        }
    }

    # Return Callable for given format
    method CODE(Str(Cool) $format --> Callable:D) {
        self.AST($format).EVAL
    }

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

# vim: expandtab shiftwidth=4
