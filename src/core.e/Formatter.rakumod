# THEORY OF OPERATION
# -------------------
# The new method will look for the given format in a hash.  If found,
# it contains a Callable that will process the parameters doing all of the
# necessary checks and conversions.
#
# If not found, it will parse the format with the Formatter:: Syntax grammar
# and the Formatter::Actions class, which will create the RakuAST nodes for
# it and store the Callable in the hash.
#
# TODO:
# - generate code that uses native ops and variables where possible

# problem cases that should be checked:
# - https://github.com/Raku/old-issue-tracker/issues/4537
#   say sprintf('%f %f %f %f', Mu, Any, Nil, NaN);
# - https://github.com/Raku/old-issue-tracker/issues/4892
#   say sprintf("%e",1000)    # should be 1.0... instead of 10....

our class Formatter {

#-------------------------------------------------------------------------------
# Subroutines referenced at runtime by the generated ASTs, to reduce the
# actual size of the specific parts of sprintf processing.

    # pad with zeroes as integer
    our sub pad-zeroes-int(int $positions, Str:D $string) {
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

    # pad with zeroes after decimal point
    our sub pad-zeroes-precision(int $positions, Str:D $string) {
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

    # pad with zeroes as string
    our sub pad-zeroes-str(int $positions, Str:D $string) {
        nqp::islt_i(nqp::chars($string),$positions)
          ?? nqp::concat(
               nqp::x('0',nqp::sub_i($positions,nqp::chars($string))),
               $string
             )
          !! $string
    }

    # prefix given hash properly, also if value negative
    our sub prefix-hash(str $hash, Str:D $string) {
        nqp::eqat($string,'-',0)
          ?? nqp::concat('-',nqp::concat($hash,nqp::substr($string,1)))
          !! nqp::concat($hash,$string)
    }

    # prefix plus if value is not negative
    our sub prefix-plus(Str:D $string) {
        nqp::eqat($string,'-',0)
          ?? $string
          !! nqp::concat("+",$string)
    }

    # prefix space if string not starting with "+" or "-"
    our sub prefix-space(Str:D $string) {
        nqp::eqat($string,'-',0) || nqp::eqat($string,'+',0)
          ?? $string
          !! nqp::concat(' ',$string)
    }

    # prefix 0 if string not starting with 0
    our sub prefix-zero(Str:D $string) {
        nqp::eqat($string,'0',0)
          ?? $string
          !! nqp::eqat($string,'-',0)
            ?? nqp::eqat($string,'0',1)
              ?? $string
              !! nqp::concat('-0',nqp::substr($string,1))
            !! nqp::concat('0',$string)
    }

    # set up value for scientific notation
    our proto sub scientify(|) {*}
    our multi sub scientify($letter, $positions, $value --> Str:D) {
        scientify($letter, $positions, $value.Numeric)
    }
    our multi sub scientify($letter, $positions, Numeric:D $value --> Str:D) {
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

    # provide left justification of string
    our sub str-left-justified(int $positions, Str:D $string) {
        nqp::islt_i(nqp::chars($string),nqp::abs_i($positions))
          ?? nqp::concat(
               $string,nqp::x(
                 ' ',nqp::sub_i(nqp::abs_i($positions),nqp::chars($string))
               )
             )
          !! $string
    }

    # provide right justification of string
    our sub str-right-justified(int $positions, Str:D $string) {
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

#-------------------------------------------------------------------------------
# Helper subs that obtain information from Match objects.  These always take
# $/ as the first positional parameter.

        # helper sub to check if a flag is set
        sub has-hash($/)  { "#" (elem) $<flags>.map: *.Str }
        sub has-minus($/) { "-" (elem) $<flags>.map: *.Str }
        sub has-plus($/)  { "+" (elem) $<flags>.map: *.Str }
        sub has-space($/) { " " (elem) $<flags>.map: *.Str }
        sub has-zero($/)  { "0" (elem) $<flags>.map: *.Str }


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
                !! (my $size := $/.Int) > 1
                  ?? ast-integer($size)
                  !! Nil
              !! Nil
        }

        # helper sub to determine the value for this directive
        sub parameter($/ --> RakuAST::Node:D) {
            my Int $index = $<idx> ?? $<idx>.chop.Int !! $*NEXT-PARAMETER;
            X::Str::Sprintf::Directives::Unsupported.new(
              directive => ~$<idx>,
              sequence  => ~$/,
            ).throw if $index < 1;

            # set default index for next parameter
            $*NEXT-PARAMETER = $index + 1;

            # record the directive, * indicates a position indicator (e.g. 4$)
            @*DIRECTIVES[$index] = .Str with $<sym> // '*';

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
            my ($size is copy, $precision is copy, $parameter) := spa($/);

            if !$precision && $size {
                if has-zero($/) && !has-minus($/) {
                    $precision := $size;
                    $size      := Nil;
                }
#                else {
#                    $precision = literal-integer(1);
#                }
            }

            # $a.Int
            my $ast := ast-call-method($parameter, $coerce // 'Int');

            # $ast.(Str || .base($base))
            $ast := $base && $base != 10
              ?? ast-call-method($ast, 'base', ast-integer($base))
              !! ast-call-method($ast, 'Str');

            # $ast.lc
            $ast := ast-call-method($ast, 'lc') if $lc;

            # handle any prefixes
            my int $minus;
            if $hash && has-hash($/) {
                if $hash eq '0' {   # only for octal
                    # prefix-zero($ast)
                    $ast  := ast-call-sub('prefix-zero', $ast);
                    $minus = 1;
                }
                else {
                    # parameter ?? prefix-hash('$hash',$ast) !! $ast
                    $ast := ast-ternary(
                      $parameter,
                      ast-call-sub('prefix-hash', ast-string($hash), $ast),
                      $ast
                    );
                    $minus = $hash.chars;
                }
            }

            my $prefix;
            if $plus && has-plus($/) {
                $prefix = 'prefix-plus';
                $minus  = 1;
            }
            elsif $space && has-space($/) {
                $prefix = 'prefix-space';
                $minus  = 1;
            }

            # expand to precision indicated
            if $precision {
                my $width := $prefix
                  # $precision - 1
                  ?? ast-infix($precision, "-", ast-integer(1))
                  !! $precision;

                # pad-zeroes-int(
                #   $parameter ?? ($precision - $minus) !! $precision, $ast
                # )
                $ast := ast-call-sub(
                  'pad-zeroes-int',
                  ast-ternary(
                    $parameter,
                    $minus
                      ?? ast-infix($width, "-", ast-integer($minus))
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
                  has-minus($/)
                    ?? "str-left-justified"
                    !! "str-right-justified",
                  $size,
                  $ast
                );
            }

            # Set up special handling of 0 if 0 precision
            if !$precision && $<precision><size> {
                # parameter ?? $ast !! ""
                $ast := ast-ternary($parameter, $ast, ast-string(""));
            }

            $ast
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
              $/, :base(2), :plus, :space, :hash("0$<sym>")
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
            make handle-integer-numeric($/, :plus, :space, :coerce<UInt>);
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
            return &?ROUTINE(self, $format);
        }
        @*DIRECTIVES.unshift("");       # we're 1-based internally

        # Index of next parameter to be expected.  Note that we do this
        # 1-based rather than 0-based, for easier matching with position
        # specifications, which *are* 1-based.
        my $*NEXT-PARAMETER = 1;

        if Formatter::Syntax.parse($format, actions => Actions) -> $parsed {
            my @operands = $parsed<statement>.map: *.made;

            # at least one directive
            @*DIRECTIVES.shift;  # 0-based from now on
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
                    RakuAST::Parameter.new(
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
