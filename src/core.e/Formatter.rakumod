# THEORY OF OPERATION
# -------------------
# The "new" method will look for the given format in a hash.  If found,
# it contains a Callable that will process the parameters doing all of the
# necessary checks and conversions.
#
# If not found, it will parse the format with the Formatter::Syntax grammar
# and the Formatter::Actions class, which will create the RakuAST nodes for
# it and store the Callable in the hash.

#- Formatter -------------------------------------------------------------------
# Class containing actual format -> AST mapping logic

our class Formatter {

    # Integer representation of characters needed in rounding logic.
    # Not sure yet whether constants would be better
    my int $zero    = nqp::ord("0");
    my int $five    = $zero + 5;
    my int $ten     = $zero + 10;
    my int $minus   = nqp::ord("-");
    my     $period := nqp::list_i(nqp::ord("."));

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
        nqp::eqat($string,'-',0) || nqp::iseq_s($string,'NaN')
          ?? $string
          !! nqp::concat($signer,$string)
    }

    # prefix plus if value is not negative
    our sub prefix-plus(str $string --> str) {
        nqp::eqat($string,'-',0)
          ?? $string
          !! nqp::concat("+",$string)
    }

    # prefix 0 if value does not start with a 0
    our sub prefix-zero(str $string --> str) {
        nqp::eqat($string,'0',0)
          ?? $string
          !! nqp::eqat($string,'-',0)
            ?? nqp::eqat($string,'0',1)
              ?? $string
              !! nqp::concat("-0",nqp::substr($string,1))
            !! nqp::concat("0",$string)
    }

    # Set up value for scientific notation: at this point it
    # is guaranteed that the value is *not* Inf, -Inf or NaN.
    # This version will *always* render with a decimal point,
    # even if precision is 0
    our sub scientify-point(str $letter, int $positions, $value --> str) {
        my str $string = scientify($letter, $positions, $value);
        my int $index  = nqp::index($string,$letter);

        # Not zero positions or generated with "e" format
        $positions
          ?? $string
          !! $index > -1
            ?? nqp::concat(
                 nqp::substr($string,0,$index),
                 nqp::concat(".",nqp::substr($string,$index))
               )
            !! nqp::concat($string,".")
    }

    # Set up value for scientific notation: at this point it
    # is guaranteed that the value is *not* Inf, -Inf or NaN.
    # The value 0 can only occur here with dynamic width or
    # precision.
    our sub scientify(str $letter, int $positions, $value --> str) {

        # Something complicated to do
        if $value {
            my $abs := $value.abs;
            my constant $divider = 10.log;

            # Initial rendering
            my int $exp     = $abs ?? ($abs.log / $divider).floor !! 0;
            my int $abs-exp = nqp::abs_i($exp);
            my     $power  := 10 ** $abs-exp;
            my str $string  = (($exp < 0
              ?? $abs * $power
              !! $abs / $power
            ) * 10 ** $positions).round.Str;

            # Fix up decimal point
            $string = $positions
              ?? nqp::concat(
                   nqp::substr($string,0,1),
                   nqp::concat(
                     ".",
                     nqp::substr($string,1,$positions)
                   )
                 )
              !! nqp::substr($string,0,1);

            # Fix up for consumption
            nqp::concat(
              ($value < 0 ?? "-" !! ""),
              nqp::concat(
                $string,
                nqp::concat(
                  $letter,
                  nqp::concat(
                    ($exp < 0 ?? "-" !! "+"),
                    ($abs-exp < 10 ?? "0" ~ $abs-exp !! ~$abs-exp)
                  )
                )
              )
            )
        }

        # Simple 0 handling
        else {
            nqp::concat(
              $positions
                ?? nqp::concat('0.',nqp::x('0',$positions))
                !! '0',
              nqp::concat(
                $letter,
                '+00'
              )
            )
        }
    }

    # Helper sub to round a number in a string with a possible decimal
    # point.  Returns a int32 codes array. Sets an upgraded flag if the
    # value actually was bumped an order of magnitude (999 -> 1000).
    our sub string-round-codes(
      str $string,
      int $point,
      int $significant,
          $upgraded is rw
    ) {
        nqp::strtocodes(
          $string,nqp::const::NORMALIZE_NFC,my int32 @codes
        );
        nqp::splice(@codes,nqp::list_i,$point,1)  # remove any period
          if $point > -1;
        nqp::unshift_i(@codes,$zero);  # make sure a 999 will allow a 1000

        # Perform rounding
        my int $index = nqp::elems(@codes);
        nqp::while(
          nqp::isge_i(--$index,$significant),  # > because of additional 0
          nqp::if(
            nqp::isge_i(nqp::atpos_i(@codes,$index),$five),
            nqp::stmts(
              nqp::bindpos_i(@codes,$index,$zero),
              (my int $before = $index - 1),
              nqp::bindpos_i(
                @codes,$before,nqp::atpos_i(@codes,$before) + 1
              )
            )
          )
        );

        # Finish rounding
        if nqp::iseq_i(nqp::atpos_i(@codes,$index),$ten) {
            nqp::bindpos_i(@codes,$index--,$zero);   # reset current
            nqp::bindpos_i(                          # increment previous
              @codes,$index,nqp::atpos_i(@codes,$index) + 1
            );
        }

        # Upped an order of magnitude (999 -> 1000)
        if nqp::isne_i(nqp::atpos_i(@codes,0),$zero) {
            $upgraded = 1;
        }

        # No upgrade, remove dummy zero
        else {
            nqp::shift_i(@codes);
        }

        @codes
    }

    # Helper sub to round the *string* representing a mantissa in
    # scientific notation to the given significant number of digits
    our sub mantissa-round(
      str $string,       # string to handle
      int $exponential,  # location of the E/e in the string
      int $significant   # number of significant digits required
     --> str) {

        # Convert to codes and round the value
        my @codes := string-round-codes(
          nqp::substr($string,0,$exponential), 1, $significant, my $upgraded
        );

        # Moved value up an order of magnitude
        my str $post = nqp::substr($string,$exponential);
        if $upgraded {
            my int $exponent =
              nqp::atpos(nqp::radix(10,nqp::substr($post,1),0,0x02),0);
            ++$exponent;
            $exponent = nqp::abs_i($exponent);

            $post = nqp::concat(
              nqp::substr($post,0,1),
              $exponent <= 10 ?? nqp::concat("0",~$exponent) !! ~$exponent
            );

            # Going up an order of magnitude *always* results in a X.0,
            # so limit to just the first digit
            nqp::setelems(@codes,1);
        }

        # Same order of magnitude, set to number of significant digits
        # and slip in the period
        else {
            nqp::setelems(@codes,$significant);

            my $index = nqp::elems(@codes);
            nqp::while(
              nqp::iseq_i(nqp::atpos_i(@codes,--$index),$zero),
              nqp::null
            );
            nqp::splice(@codes,$period,1,0);
        }

        # create the final result
        nqp::concat(nqp::strfromcodes(@codes),$post)
    }

    # Set up value for either a sort of floating point or scientific
    # notation: this version will *always* render with a period if it
    # is *not* reverting to %e format. At this point it is guaranteed
    # that the value is *not* Inf, -Inf or NaN.  The value 0 can only
    # occur here with dynamic width or precision.
    our sub bestfit-point(str $letter, int $positions, $value --> str) {
        my str $string = bestfit($letter, $positions, $value);

        # Not zero positions or reverted to "e" format
        $value
          ?? $positions || nqp::indexic($string,"e",0) > -1
            ?? $string
            !! nqp::concat($string,".")
          !! "0."
    }

    # Set up value for either a sort of floating point or scientific
    # notation: at this point it is guaranteed that the value is
    # *not* Inf, -Inf or NaN.  The value 0 can only occur here
    # with dynamic width or precision.
    our sub bestfit(
      str $letter      is copy,
      int $significant is copy,
          $value
    ) {

        # Something complicated to do
        if $value {

            # Adjust to letter to generate with
            $letter = $letter eq 'G' ?? 'E' !! 'e';

            # One would specify a 0 significance to indicate that the
            # decimal point should be shown for whole values (with the
            # # also occurring in the format).  Otherwise, we just use
            ++$significant unless $significant;

            # Initial rendering
            my $abs       := $value.abs;
            my str $string = $abs.Str;

            # Already get an exponential representation
            my int $exponential = nqp::index($string,'e');
            if $exponential > -1 {

                # Make sure the string as the right letter
                $string = nqp::replace($string,$exponential,1,$letter)
                  if $letter eq 'E';

                # String was generated with the right number of significant
                # digits or less: less means that there were no more
                # significant digits to generate anyway.  So we're done.
                if $exponential - 1 <= $significant {
                    $string
                }

                # Need to round the mantissa to the requested number of
                # significant digits
                else {
                    $string = mantissa-round(
                      $string, $exponential, $significant
                    );

                    # Make sure the negative values were handled correctly
                    $value < 0
                      ?? nqp::concat("-",$string)
                      !! $string
                }
            }

            # Not an exponential representation yet
            else {
                my int $point    = nqp::index($string,".");
                my int $has-zero = nqp::eqat($string,'0',0);
                my int $valids   =
                  nqp::chars($string) - $has-zero - nqp::isgt_i($point,-1);

                # Initial rendering has fewer or the same significant
                # digits: so we're really done
                if $valids <= $significant {

                    # Make sure the negative values were handled correctly
                    $value < 0
                      ?? nqp::concat("-",$string)
                      !! $string
                }

                # Initial rendering has more than requested significant
                # digits, so we will need to round
                else {

                    # Helper sub for adding E±NN
                    my sub add-exponent(str $string, int $exponent) {
                        nqp::concat(
                          $string,
                          nqp::concat(
                            $letter,
                            nqp::concat(
                              nqp::islt_i($exponent,0) ?? "-" !! "+",
                              nqp::concat(
                                nqp::abs_i($exponent) < 10 ?? "0" !! "",
                                nqp::abs_i($exponent)
                              )
                            )
                          )
                        )
                    }

                    # Convert to codes, round the value and set signficance
                    my @codes := string-round-codes(
                      $string, $point, $significant, my int $upgraded
                    );

                    # If upgraded to another order of magnitude, it's
                    # always just 1e+NN, so do that
                    if $upgraded {
                        add-exponent(
                          $value < 0 ?? "-1" !! "1",
                          $point > -1
                            ?? $point - $has-zero
                            !! nqp::chars($string)
                        )
                    }

                    # not upgraded
                    else {
                        my int $power;

                        # Same order of magnitude, with a period
                        if $point > -1 {

                            # period indicates just a fraction
                            if $abs < 1 {

                                # Count leading zeroes
                                my int $index;
                                nqp::while(
                                  nqp::iseq_i(
                                    nqp::atpos_i(@codes,++$index),$zero
                                  ),
                                  nqp::null
                                );

                                # Can show without reverting to e notation
                                if $index <= $significant {
                                    # include leading 0 for later limiting
                                    ++$significant;
                                }

                                # Need to use E notation
                                else {
                                    nqp::splice(@codes,nqp::list,0,$index);
                                    $power = $index;
                                }
                            }
                            # period indicates value with a fractional part
                            else {
                                $power = $point - 1;
                            }
                        }

                        # No period
                        else {
                            $power = nqp::sub_i(nqp::chars($string),1);
                        }

                        # Limit to significant digits
                        nqp::setelems(@codes,$significant);

                        # Eliminate trailing 0's
                        nqp::while(
                          nqp::elems(@codes) && nqp::iseq_i(
                            (my int $digit = nqp::pop_i(@codes)),
                            $zero
                          ),
                          nqp::null
                        );
                        nqp::push_i(@codes,$digit);

                        # Put back period unless it would be at the end
                        nqp::splice(@codes,$period,1,0)
                          if nqp::isgt_i(nqp::elems(@codes),1);

                        # Prefix sign if necessary and done
                        nqp::unshift_i(@codes,$minus) if $value < 0;

                        # Add scientific notation if so indicated
                        $power
                          ?? add-exponent(nqp::strfromcodes(@codes),$power)
                          !! nqp::strfromcodes(@codes)
                    }
                }
            }
        }

        # 0 returns "0", as it has no number of significant digits
        # and the "g/G" format only shows significant digits
        else {
            "0"
        }
    }

    # Provide left justification of string for given signer and
    # number of positions
    our sub str-signer-left-justified(
      str $signer, int $positions, str $string
    --> str) {
        str-left-justified(
          $positions,
          nqp::eqat($string,'-',0)
            ?? $string
            !! nqp::concat($signer,$string)
        )
    }

    # Provide right justification of string for given signer and
    # number of positions
    our sub str-signer-right-justified(
      str $signer, int $positions, str $string
    --> str) {
        str-right-justified(
          $positions,
          nqp::eqat($string,'-',0)
            ?? $string
            !! nqp::concat($signer,$string)
        )
    }

    # Provide left justification of string for given number of positions
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

    # Return 1 of NaN or Inf, else 0
    our sub nan-or-inf($value --> int) {
        nqp::istype($value,Num) && nqp::isnanorinf($value)
    }

    # Provide conversion of numeric values to string, only rendering a
    # decimal point if number of digits > 0
    our sub stringify-multiplier-digits(
      $value, Int:D $multiplier, int $digits
    --> str) {
        $digits
          ?? stringify-multiplier-digits-point($value, $multiplier, $digits)
          !! $value.round.Str
    }

    # Provide conversion of numeric values to string, always rendering
    # a decimal point of the %f formatting
    our sub stringify-multiplier-digits-point(
      $value, Int:D $multiplier, int $digits
    --> str) {
        my str $string = $value
          ?? ($value * $multiplier).round.Str
          !! nqp::concat("0",nqp::substr($multiplier.Str,1));

        if nqp::chars($string) - $digits -> int $cutoff {
            $cutoff < 0
              ?? nqp::concat(
                   "0.",
                   nqp::concat($string,nqp::x("0", nqp::abs_i($cutoff)))
                 )
              !! nqp::concat(
                   nqp::substr($string,0,$cutoff),
                   nqp::concat('.',nqp::substr($string,$cutoff))
                 )
        }
        else {
            nqp::concat("0.",$string);
        }
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
        multi sub ast-call-sub(
          $name, $one, $two, $three, $four,
        --> RakuAST::Call::Name:D) {
            RakuAST::Call::Name.new(
              name => RakuAST::Name.from-identifier-parts('Formatter', $name),
              args => RakuAST::ArgList.new($one, $two, $three, $four)
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

        # helper sub to create AST for justifying if necessary
        sub ast-justify($/, $size, $ast) {
            $size
              ?? ast-call-sub(
                   has-minus($/)
                     ?? "str-left-justified"
                     !! has-zero($/)
                       ?? "pad-zeroes-str"
                       !! "str-right-justified",
                   $size,
                   $ast
                 )
              !! $ast
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
        sub plus-minus-zero($/, $size, $ast) {
            ast-justify( $/,
              $size,
              has-plus($/)
                ?? ast-call-sub('prefix-plus', $ast)
                !! $ast
            )
        }

        # Helper sub to return string for any signer, or the empty string
        sub signer($/) {
            has-plus($/) ?? "+" !! has-space($/) ?? " " !! ""
        }

        # Helper sub to return any applicable justifier without signer
        sub justifier($/) {
            has-minus($/)
              ?? 'str-left-justified'
              !! 'str-right-justified'
        }

        # Helper sub to return any applicable justifier *with* signer
        sub signer-justifier($/) {
            has-minus($/)
              ?? 'str-signer-left-justified'
              !! 'str-signer-right-justified'
        }

        # Helper sub to return a string for 0 for the given %e or %g
        # formats.  Returns empty string if no static string
        # could be returned.
        sub zero-exponential($/, str $letter, $size, $precision) {
            zero-float($/, $size, $precision, $letter ~ "+00");
        }

        # Helper sub to return a string for 0 for the given %f
        # format.  Returns empty string if no static string
        # could be returned.
        sub zero-float($/, $size, $precision, str $extra = "" --> str) {
            if is-literal-int($precision) {
                my int $digits = $precision.value;
                my str $string = $digits || has-hash($/)
                  ?? "0." ~ "0" x $digits ~ $extra
                  !! "0" ~ $extra;
                my str $signer = signer($/);

                if $size {
                    if is-literal-int($size) {
                        my int $width = $size.value;
                        has-zero($/)
                          ?? pad-signer-zeroes-int($width, $signer, $string)
                          !! ::("&" ~ justifier($/))(
                               $width,
                               prefix-signer($signer, $string)
                             )
                    }
                    else {
                        ""
                    }
                }
                else {
                    prefix-signer($signer, $string)
                }
            }
            else {
                ""
            }
        }

        # helper sub for processing formats for integer values
        sub handle-integer-numeric($/,
           Int :$base,    # the number base to assume for generating string
           Str :$hash,    # the string to prefix if "#" is in format
           Str :$coerce,  # method name to initially coerce with, default Int
          Bool :$lc,      # whether to lowercase resulting string
          Bool :$plus,    # allow prefix "+" if positive, not starting with -
          Bool :$space,   # allow prefix " " if not starting with -
          Bool :$octal    # perform extra octal handling for #
        ) {
            my $size      = size($/);
            my $precision = precision($/);
            my $parameter = parameter($/, :coerce($coerce // "Int"));

            # AST for handling when the value is zero
            my $zero := "0";

            # AST for handling when the value is NOT zero
            my $not-zero := $base && $base != 10
              ?? ast-call-method($parameter, 'base', ast-integer($base))
              !! ast-call-method($parameter, 'Str');

            # Perform weird octal handling for # flag
            $not-zero := ast-call-sub('prefix-zero', $not-zero)
              if $octal && has-hash($/);

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
                my str $justifier = justifier($/);

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

        # Generic handling of floating point logic, returns a ready
        # to use AST
        sub handle-float-numeric($/,
          $size, $precision, $parameter, str $zero, $not-zero is copy,
          :$NAN-OR-INF
        ) {
            my str $signer = signer($/);

            # helper sub to set up justification if there is no "0" flag
            my sub justify($/, $ast) {
                $size
                  ?? $signer
                    ?? ast-call-sub(signer-justifier($/),
                         ast-string($signer), $size, $ast
                       )
                    !! ast-call-sub(justifier($/), $size, $ast)
                  !! $signer
                    ?? ast-call-sub('prefix-signer', ast-string($signer), $ast)
                    !! $ast
            }

            # Set up NaN / ±Inf handling
            my $nan-or-inf := ast-call-method(
              $parameter, $NAN-OR-INF ?? 'uc' !! 'Str'
            );

            # Filling out with zeroes
            if $size && has-zero($/) {
                $not-zero := $signer
                  ?? ast-call-sub('signer-pad-zeroes-int',
                       ast-string($signer), ast-sub-integer($size, 1), $not-zero
                     )
                  !! ast-call-sub('pad-zeroes-int', $size, $not-zero);

                $not-zero := ast-ternary(
                  ast-call-sub('nan-or-inf', $parameter),
                  justify($/, $nan-or-inf),
                  $not-zero
                );

            }

            # Just justification
            else {
                $not-zero := justify($/,
                  ast-ternary(
                    ast-call-sub('nan-or-inf', $parameter),
                    $nan-or-inf,
                    $not-zero
                  )
                );
            }

            # Add zero handling if possible
            $zero
              ?? ast-ternary($parameter, $not-zero, ast-string($zero))
              !! $not-zero
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
            make handle-integer-numeric($/,
              :base(2), :hash("0$<sym>")
            );
        }

        # show character representation of codepoint value
        method directive:sym<c>($/ --> Nil) {
            my $size      := size($/);
            my $precision := precision($/);
            my $parameter := parameter($/, :coerce<Int>);

            make ast-justify($/,
              $size,
              ast-call-method($parameter, 'chr')
            )
        }

        # show decimal (integer) value
        method directive:sym<d>($/ --> Nil) {
            make handle-integer-numeric($/,
              :base(10), :plus, :space
            );
        }

        # show floating point value, scientific notation
        method directive:sym<e>($/ --> Nil) {
            my $size      := size($/);
            my $precision := precision($/) // ast-integer(6);
            my $parameter := parameter($/, :coerce<Numeric>);
            my $letter    := $<sym>.Str;

            make handle-float-numeric($/,
              $size, $precision, $parameter,
              zero-exponential($/, $letter, $size, $precision),  # zero
              ast-call-sub(has-hash($/)                          # not-zero
                  && (!is-literal-int($precision) || $precision.value == 0)
                  ?? 'scientify-point'
                  !! 'scientify',
                ast-string($letter),
                $precision,
                $parameter
              )
            )
        }

        # show floating point value
        method directive:sym<f>($/ --> Nil) {
            my $size      := size($/);
            my $precision := precision($/) // ast-integer(6);
            my $parameter := parameter($/, :coerce<Numeric>);

            make handle-float-numeric($/,
              $size, $precision, $parameter,
              zero-float($/, $size, $precision),  # zero
              ast-call-sub(                       # not-zero
                has-hash($/)
                  ?? 'stringify-multiplier-digits-point'
                  !! 'stringify-multiplier-digits',
                $parameter,
                is-literal-int($precision)
                  ?? ast-integer(10 ** $precision.value)
                  !! ast-infix(ast-integer(10), '**', $precision),
                $precision
              ),
              :NAN-OR-INF($<sym> eq 'F')
            );
        }

        # f or e depending on value
        method directive:sym<g>($/ --> Nil) {
            my $size      := size($/);
            my $precision := precision($/) // ast-integer(6);
            my $parameter := parameter($/, :coerce<Numeric>);
            my $letter    := $<sym>.Str;
            my int $point  = has-hash($/)
              && (!is-literal-int($precision) || $precision.value == 0);

            make handle-float-numeric($/,
              $size, $precision, $parameter,
              $point ?? "0." !! "0",   # zero
              ast-call-sub(            # not-zero
                $point ?? 'bestfit-point' !! 'bestfit',
                ast-string($letter),
                $precision,
                $parameter
              ),
              :NAN-OR-INF($letter eq 'G')
            )
        }

        # show numeric value in octal using Perl / Raku semantics
        method directive:sym<o>($/ --> Nil) {
            make handle-integer-numeric($/, :base(8), :octal);
        }

        # show string
        method directive:sym<s>($/ --> Nil) {
            my $size      := size($/);
            my $precision := precision($/);
            my $parameter := parameter($/, :coerce<Str>);

            make ast-justify($/,
              $size,
              $precision
              ?? ast-call-method(
                   $parameter, 'substr', ast-integer(0), $precision
                 )
              !! $parameter
            )
        }

        # show unsigned decimal (integer) value
        method directive:sym<u>($/ --> Nil) {
            make handle-integer-numeric($/, :coerce<UInt>);
        }

        # show numeric value in hexadecimal
        method directive:sym<x>($/ --> Nil) {
            make handle-integer-numeric($/,
              :base(16), :hash("0$<sym>"), :lc($<sym> eq "x")
            )
        }

        # an escaped %
        method directive:sym<%>($/ --> Nil) {
            make ast-string('%');
        }

        # huh?
        method panic($/ --> Nil) {
            X::Str::Sprintf::Directives::Unsupported.new(
              directive => ~$<sym>, sequence  => ~$/
            ).throw
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
                      !! RakuAST::Nqp.new(
                           "join",
                           RakuAST::StrLiteral.new(""),
                           RakuAST::Nqp.new(
                             "list_s",
                             @operands
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

    # Cache where we keep our formats/directives. Key is the format string,
    # and the value is an IterationBuffer with the code and directivces.
    my $FORMATS := nqp::hash;

    # Convert a format string into a Callable
    method new(Str:D $format) { nqp::atpos(self.fetch($format),0) }

    # Actual workhorse for sprintf()
    method fetch(Str:D $format) is implementation-detail {
        nqp::ifnull(
          nqp::atkey($FORMATS,$format),
          self!fetch-new-format($format)
        )
    }

    # Threadsafe cache updater, don't care about multiple threads trying
    # to do the same format, but this way we don't have a lock if the
    # same format is called *many* times over.  Returns an IterationBuffer
    # with the code and directives.
    method !fetch-new-format(Str:D $format) {
        my @*DIRECTIVES := my str @directives;  # the directives seen
        my @*COERCIONS  := my str @;            # any coercions on parameters

        # Set up initial state of new cache
        my $new := nqp::clone($FORMATS);
        nqp::deletekey($new,nqp::iterkey_s(nqp::shift(nqp::iterator($new))))
          if nqp::isge_i(nqp::elems($new),100);  # XXX should be settable

        # Obtain the info for this format
        my $info := nqp::create(IterationBuffer);
        nqp::push($info,self.CODE($format));
        nqp::push($info,@directives);

        # Add new format info to cache and install the updated cache
        # Note that the bindkey *MUST* be done before installing to
        # prevent the shared hash being altered, which could cause a
        # producing wrong results and/or a segfault.
        nqp::bindkey($new,$format,$info);
        $FORMATS := $new;

        $info
    }
}

# vim: expandtab shiftwidth=4
