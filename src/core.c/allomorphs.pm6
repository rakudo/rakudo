my class Allomorph is Str {
    multi method Bool(::?CLASS:D:) { self.Numeric.Bool }

    multi method ACCEPTS(Allomorph:D: Any:D \a) is default {
        nqp::istype(a, Numeric)
          ?? self.Numeric.ACCEPTS(a)
          !! nqp::istype(a, Str)
            ?? self.Str.ACCEPTS(a)
            !! self.Str.ACCEPTS(a) && self.Numeric.ACCEPTS(a)
    }

    method succ(Allomorph:D:) { self.Numeric.succ }
    method pred(Allomorph:D:) { self.Numeric.pred }

    method comb(Allomorph:D: |c) {
        nqp::getattr_s(self,Str,'$!value').comb(|c)
    }

    method split(Allomorph:D: |c) {
        nqp::getattr_s(self,Str,'$!value').split(|c)
    }

    method subst(Allomorph:D: |c) {
        nqp::getattr_s(self,Str,'$!value').subst(|c)
    }
    method subst-mutate(Allomorph:D \SELF: |c) {
        (SELF = nqp::getattr_s(self,Str,'$!value')).subst-mutate(|c)
    }

    method samecase(Allomorph:D: |c) {
        nqp::getattr_s(self,Str,'$!value').samecase(|c)
    }
    method samemark(Allomorph:D: |c) {
        nqp::getattr_s(self,Str,'$!value').samemark(|c)
    }
    method samespace(Allomorph:D: |c) {
        nqp::getattr_s(self,Str,'$!value').samespace(|c)
    }

    method chop(Allomorph:D: |c) {
        nqp::getattr_s(self,Str,'$!value').chop(|c)
    }
    method chomp(Allomorph:D:) {
        nqp::getattr_s(self,Str,'$!value').chomp
    }
    method trim(Allomorph:D:) {
        nqp::getattr_s(self,Str,'$!value').trim
    }
    method trim-leading(Allomorph:D:) {
        nqp::getattr_s(self,Str,'$!value').trim-leading
    }
    method trim-trailing(Allomorph:D:) {
        nqp::getattr_s(self,Str,'$!value').trim-trailing
    }
    method lc(Allomorph:D:) { 
        nqp::getattr_s(self,Str,'$!value').lc
    }
    method uc(Allomorph:D:) { 
        nqp::getattr_s(self,Str,'$!value').uc
    }
    method tc(Allomorph:D:) { 
        nqp::getattr_s(self,Str,'$!value').tc
    }
    method tclc(Allomorph:D:) { 
        nqp::getattr_s(self,Str,'$!value').tclc
    }
    method fc(Allomorph:D:) { 
        nqp::getattr_s(self,Str,'$!value').fc
    }
    method flip(Allomorph:D:) { 
        nqp::getattr_s(self,Str,'$!value').flip
    }

    method substr(Allomorph:D: |c) {
        nqp::getattr_s(self,Str,'$!value').substr(|c)
    }
    method substr-rw(Allomorph:D \SELF:
      $start = 0,
      $want  = Whatever
    ) is rw {
        SELF.substr-rw($start, $want, Str)
    }

    method Str(Allomorph:D:) {
        nqp::getattr_s(self,Str,'$!value')
    }

    multi method WHICH(Allomorph:D:) {
        nqp::box_s(
          nqp::join('|',nqp::list_s(
            self.^name,
            self.Numeric.WHICH,
            self.Str.WHICH
          )),
          ValueObjAt
        )
    }

    multi method raku(Allomorph:D:) {
        nqp::join("",nqp::list_s(
          self.^name,'.new(',self.Numeric.raku,', ',self.Str.raku,')'
        ))
    }
}

# the uses of add_I in this class are a trick to make bigints work right
my class IntStr is Allomorph is Int {
    method new(Int:D $i, Str:D $s) {
        my \SELF = nqp::add_I($i, 0, self);
        nqp::bindattr_s(SELF, Str, '$!value', $s);
        SELF
    }

    multi method Numeric(IntStr:U:) { self.Mu::Numeric }
    multi method Numeric(IntStr:D:) { nqp::add_I(self,0,Int) }

    multi method Real(IntStr:U:) { self.Mu::Real }
    multi method Real(IntStr:D:) { nqp::add_I(self,0,Int) }

    multi method Int(IntStr:D:) { nqp::add_I(self,0,Int) }
}

my class NumStr is Allomorph is Num {
    method new(Num:D $n, Str:D $s) {
        my \new = nqp::create(self);
        nqp::bindattr_n(new,Num,'$!value',$n);
        nqp::bindattr_s(new,Str,'$!value',$s);
        new
    }

    multi method Numeric(NumStr:U: --> 0e0) { self.Mu::Numeric }
    multi method Numeric(NumStr:D:) { nqp::getattr_n(self,Num,'$!value') }

    multi method Real(NumStr:U: --> 0e0) { self.Mu::Real }
    multi method Real(NumStr:D:) { nqp::getattr_n(self,Num,'$!value') }

    multi method Int(NumStr:D:)     { nqp::getattr_n(self,Num,'$!value').Int }
}

my class RatStr is Allomorph is Rat {
    method new(Rat:D $r, Str:D $s) {
        my \new = nqp::create(self);  # no need to normalize, so don't call .new
        nqp::bindattr(new,Rat,'$!numerator',
          nqp::getattr($r,Rat,'$!numerator'));
        nqp::bindattr(new,Rat,'$!denominator',
          nqp::getattr($r,Rat,'$!denominator'));
        nqp::bindattr_s(new,Str,'$!value',$s);
        new
    }

    method Capture(RatStr:D:) { self.Mu::Capture }

    multi method Numeric(RatStr:U: --> 0.0) { self.Mu::Numeric }
    multi method Numeric(RatStr:D:) { self.Rat }

    multi method Real(RatStr:U: --> 0.0) { self.Mu::Real }
    multi method Real(RatStr:D:) { self.Rat }

    multi method Int(RatStr:D:) { self.Rat.Int }

    method Rat(RatStr:D:) {
        my \new := nqp::create(Rat);  # no need to normalize, so don't call .new
        nqp::bindattr(new,Rat,'$!numerator',
          nqp::getattr(self,Rat,'$!numerator'));
        nqp::bindattr(new,Rat,'$!denominator',
          nqp::getattr(self,Rat,'$!denominator'));
        new
    }
}

my class ComplexStr is Allomorph is Complex {
    method new(Complex:D $c, Str $s) {
        my \new = nqp::create(self);
        nqp::bindattr_n(new,Complex,'$!re',
          nqp::getattr_n($c,Complex,'$!re'));
        nqp::bindattr_n(new,Complex,'$!im',
          nqp::getattr_n($c,Complex,'$!im'));
        nqp::bindattr_s(new,Str,'$!value',$s);
        new
    }

    method Capture(ComplexStr:D:) { self.Mu::Capture }

    multi method Numeric(ComplexStr:U:) { self.Mu::Numeric; 0i }
    multi method Numeric(ComplexStr:D:) { self.Complex }

    method !has-imaginary($target) is hidden-from-backtrace {
        X::Numeric::Real.new(
          target => $target,
          source => self,
          reason => "imaginary part not zero"
        ).throw
    }

    multi method Real(ComplexStr:U: --> 0e0) { self.Mu::Real }
    multi method Real(ComplexStr:D:) {
      nqp::getattr_n(self,Complex,'$!im')
        ?? self!has-imaginary(Real)
        !! nqp::getattr_n(self,Complex,'$!re')
    }

    multi method Int(ComplexStr:D:) {
      nqp::getattr_n(self,Complex,'$!im')
        ?? self!has-imaginary(Int)
        !! nqp::getattr_n(self,Complex,'$!re')
    }

    method Complex(ComplexStr:D:) {
        my \new = nqp::create(Complex);
        nqp::bindattr_n(new,Complex,'$!re',
          nqp::getattr_n(self,Complex,'$!re'));
        nqp::bindattr_n(new,Complex,'$!im',
          nqp::getattr_n(self,Complex,'$!im'));
        new
    }
}

multi sub infix:<cmp>(Allomorph:D $a, Allomorph:D $b) is default {
    nqp::eqaddr((my $cmp := $a.Numeric cmp $b.Numeric),Order::Same)
      ?? $a.Str cmp $b.Str
      !! $cmp
}

multi sub infix:<eqv>(Allomorph:D $a, Allomorph:D $b --> Bool:D) is default {
    nqp::eqaddr($a.WHAT,$b.WHAT)
      ?? $a.Numeric eqv $b.Numeric && $a.Str eqv $b.Str
      !! False
}

multi sub infix:<===>(IntStr:D $a, IntStr:D $b) {
    $a.Int === $b.Int && $a.Str === $b.Str
}
multi sub infix:<===>(RatStr:D $a, RatStr:D $b) {
    $a.Rat === $b.Rat && $a.Str === $b.Str
}
multi sub infix:<===>(NumStr:D $a, NumStr:D $b) {
    $a.Num === $b.Num && $a.Str === $b.Str
}
multi sub infix:<===>(ComplexStr:D $a, ComplexStr:D $b) {
    $a.Complex === $b.Complex && $a.Str === $b.Str
}

multi sub val(*@maybevals) {
    @maybevals.list.map({ val($_) }).eager;
}

multi sub val(Mu \mu) {
    warn "{ mu.raku } uselessly passed to val()";
    mu
}

multi sub val(Slip:D \maybevals) { val(|maybevals).Slip }
multi sub val(List:D \maybevals) { val(|maybevals)      }

multi sub val(Pair:D \ww-thing) is raw {
    # this is a Pair object possible in «» constructs; just pass it through. We
    # capture this specially from the below sub to avoid emitting a warning
    # whenever an affected «» construct is being processed.
    ww-thing
}

multi sub val(\one-thing) is raw {
    warn "Value of type { one-thing.^name } uselessly passed to val()";
    one-thing
}

multi sub val(Str:D $MAYBEVAL, Bool :$val-or-fail, Bool :$fail-or-mu) {
    # TODO:
    # * Additional numeric styles:
    #   + fractions in [] radix notation:  :100[10,'.',53]
    # * Performance tuning
    # * Fix remaining XXXX

    my str $str = nqp::unbox_s($MAYBEVAL);
    my int $eos = nqp::chars($str);
    return IntStr.new(0,"") unless $eos;  # handle ""

    # S02:3276-3277: Ignore leading and trailing whitespace
    my int $pos = nqp::findnotcclass(nqp::const::CCLASS_WHITESPACE,
                                              $str, 0, $eos);
    my int $end = nqp::sub_i($eos, 1);

    $end = nqp::sub_i($end, 1)
        while nqp::isge_i($end, $pos)
           && nqp::iscclass(nqp::const::CCLASS_WHITESPACE, $str, $end);

    # Fail all the way out when parse failures occur. Return the original
    # string, or a failure if we're Str.Numeric
    my &parse_fail := -> \msg {
        $val-or-fail
          ?? $fail-or-mu
            ?? return Mu
            !! fail X::Str::Numeric.new(:source($MAYBEVAL),:reason(msg),:$pos)
          !! return $MAYBEVAL
    }

    # Str.Numeric should handle blank string before val()
    parse_fail "Empty string not properly caught before val()" if nqp::islt_i($end, $pos);

    # Reset end-of-string after trimming
    $eos = nqp::add_i($end, 1);

    # return an appropriate type when we've found a number. Allomorphic unless
    # Str.Numeric is calling
    my &parse_win := -> \newval {
        $val-or-fail
          ?? return newval
          !! nqp::istype(newval, Num)
            ?? return NumStr.new(newval, $MAYBEVAL)
            !! nqp::istype(newval, Rat)
              ?? return RatStr.new(newval, $MAYBEVAL)
              !! nqp::istype(newval, Complex)
                ?? return ComplexStr.new(newval, $MAYBEVAL)
                !! nqp::istype(newval, Int)
                  ?? return IntStr.new(newval, $MAYBEVAL)
                  !! die "Unknown type {newval.^name} found in val() processing"
    }

    my sub parse-simple-number() {
        # Handle NaN here, to make later parsing simpler
        if nqp::eqat($str,'NaN',$pos) {
            $pos = nqp::add_i($pos, 3);
            return nqp::p6box_n(nqp::nan());
        }

        # Handle any leading +/-/− sign
        my int $ch  = nqp::ord($str, $pos);
        my int $neg = nqp::iseq_i($ch, 45) || nqp::iseq_i($ch, 8722); # '-', '−'
        if $neg || nqp::iseq_i($ch, 43) {  # '-', '−', '+'
            $pos = nqp::add_i($pos, 1);
            $ch  = nqp::islt_i($pos, $eos) && nqp::ord($str, $pos);
        }

        # nqp::radix_I parse results, and helper values
        my Mu  $parse;
        my str $prefix;
        my int $radix;
        my int $p;

        my sub parse-int-frac-exp() {
            my $start-pos = $pos - nqp::istrue($neg);
            # Integer part, if any
            my Int $int := 0;
            if nqp::isne_i($ch, 46) {  # '.'
                parse_fail "Cannot convert radix of $radix (max 36)"
                    if nqp::isgt_i($radix, 36);
                $parse := nqp::radix_I($radix, $str, $pos, $neg, Int);
                $p      = nqp::atpos($parse, 2);
                parse_fail "base-$radix number must begin with valid digits or '.'"
                    if nqp::iseq_i($p, -1);
                $pos    = $p;

                $int   := nqp::atpos($parse, 0);
                nqp::isge_i($pos, $eos)
                  ??  return $int
                  !!  ($ch = nqp::ord($str, $pos));
            }

            # Fraction, if any
            my Int $frac := 0;
            my Int $base := 0;
            if nqp::iseq_i($ch, 46) {  # '.'
                $pos    = nqp::add_i($pos, 1);
                $parse := nqp::radix_I($radix, $str, $pos,
                                       nqp::add_i($neg, 4), Int);
                $p      = nqp::atpos($parse, 2);
                parse_fail 'radix point must be followed by one or more valid digits'
                    if nqp::iseq_i($p, -1);
                $pos    = $p;

                $frac  := nqp::atpos($parse, 0);
                $base  := nqp::atpos($parse, 1);
                $ch     = nqp::islt_i($pos, $eos) && nqp::ord($str, $pos);
            }

            # Exponent, if 'E' or 'e' are present (forces return type Num)
            if nqp::iseq_i($ch, 69) || nqp::iseq_i($ch, 101) {  # 'E', 'e'
                parse_fail "'E' or 'e' style exponent only allowed on decimal (base-10) numbers, not base-$radix"
                    unless nqp::iseq_i($radix, 10);

                $pos = nqp::add_i($pos, 1);
                # handle the sign
                # XXX TODO: teach radix_I to handle '−' (U+2212) minus?
                my int $ch  = nqp::islt_i($pos, $eos) && nqp::ord($str, $pos);
                my int $neg-e = nqp::if(
                    nqp::iseq_i($ch, 43), # '+'
                    nqp::stmts(($pos = nqp::add_i($pos, 1)), 0),
                    nqp::if( # '-', '−'
                        nqp::iseq_i($ch, 45) || nqp::iseq_i($ch, 8722),
                        nqp::stmts(($pos = nqp::add_i($pos, 1)), 1),
                        0,
                    )
                );

                $parse := nqp::radix_I(10, $str, $pos, $neg-e, Int);
                $p      = nqp::atpos($parse, 2);
                parse_fail "'E' or 'e' must be followed by decimal (base-10) integer"
                    if nqp::iseq_i($p, -1);
                $pos    = $p;

                # now that we're satisfied the number is in valid-ish format, use nqp's numifier
                # to extract the actual num from the string.
                return nqp::numify(nqp::unbox_s(nqp::substr($str, $start-pos, $pos - $start-pos)));
            }

            # Multiplier with exponent, if single '*' is present
            # (but skip if current token is '**', as otherwise we
            # get recursive multiplier parsing stupidity)
            if nqp::iseq_i($ch, 42)
            && nqp::isne_s(substr($str, $pos, 2), '**') {  # '*'
                $pos           = nqp::add_i($pos, 1);
                my $mult_base := parse-simple-number();

                parse_fail "'*' multiplier base must be an integer"
                    unless nqp::istype($mult_base, Int);
                parse_fail "'*' multiplier base must be followed by '**' and exponent"
                    unless nqp::eqat($str,'**',$pos);

                $pos           = nqp::add_i($pos, 2);
                my $mult_exp  := parse-simple-number();

                parse_fail "'**' multiplier exponent must be an integer"
                    unless nqp::istype($mult_exp, Int);

                my $mult := $mult_base ** $mult_exp;
                $int     := $int  * $mult;
                $frac    := $frac * $mult;
            }

            # Return an Int if there was no radix point, otherwise, return a Rat
            nqp::unless($base, $int, Rat.new($int * $base + $frac, $base));
        }

        # Look for radix specifiers
        if nqp::iseq_i($ch, 58) {  # ':'
            # A string of the form :16<FE_ED.F0_0D> or :60[12,34,56]
            $pos    = nqp::add_i($pos, 1);
            $parse := nqp::radix_I(10, $str, $pos, 0, Int);
            $p      = nqp::atpos($parse, 2);
            parse_fail "radix (in decimal) expected after ':'"
                if nqp::iseq_i($p, -1);
            $pos    = $p;

            $radix  = nqp::atpos($parse, 0);
            $ch     = nqp::islt_i($pos, $eos) && nqp::ord($str, $pos);
            if nqp::iseq_i($ch, 60) {  # '<'
                $pos = nqp::add_i($pos, 1);

                my $result := parse-int-frac-exp();

                parse_fail "malformed ':$radix<>' style radix number, expecting '>' after the body"
                    unless nqp::islt_i($pos, $eos)
                        && nqp::iseq_i(nqp::ord($str, $pos), 62);  # '>'

                $pos = nqp::add_i($pos, 1);
                return $result;
            }
            elsif nqp::iseq_i($ch, 171) {  # '«'
                $pos = nqp::add_i($pos, 1);

                my $result := parse-int-frac-exp();

                parse_fail "malformed ':$radix«»' style radix number, expecting '»' after the body"
                    unless nqp::islt_i($pos, $eos)
                        && nqp::iseq_i(nqp::ord($str, $pos), 187);  # '»'

                $pos = nqp::add_i($pos, 1);
                return $result;
            }
            elsif nqp::iseq_i($ch, 91) {  # '['
                $pos = nqp::add_i($pos, 1);
                my Int $result := 0;
                my Int $digit  := 0;
                while nqp::islt_i($pos, $eos)
                   && nqp::isne_i(nqp::ord($str, $pos), 93) {  # ']'
                    $parse := nqp::radix_I(10, $str, $pos, 0, Int);
                    $p      = nqp::atpos($parse, 2);
                    parse_fail "malformed ':$radix[]' style radix number, expecting comma separated decimal values after opening '['"
                        if nqp::iseq_i($p, -1);
                    $pos    = $p;

                    $digit := nqp::atpos($parse, 0);
                    parse_fail "digit is larger than {$radix - 1} in ':$radix[]' style radix number"
                        if nqp::isge_i($digit, $radix);

                    $result := $result * $radix + $digit;
                    $pos     = nqp::add_i($pos, 1)
                        if nqp::islt_i($pos, $eos)
                        && nqp::iseq_i(nqp::ord($str, $pos), 44);  # ','
                }
                parse_fail "malformed ':$radix[]' style radix number, expecting ']' after the body"
                    unless nqp::islt_i($pos, $eos)
                        && nqp::iseq_i(nqp::ord($str, $pos), 93);  # ']'
                $pos = nqp::add_i($pos, 1);

                # XXXX: Handle fractions!
                # XXXX: Handle exponents!
                return $neg ?? -$result !! $result;
            }
            else {
                parse_fail "malformed ':$radix' style radix number, expecting '<' or '[' after the base";
            }
        }
        elsif nqp::iseq_i($ch, 48)  # '0'
          and $radix = nqp::index('  b     o d     x',
                                  nqp::substr($str, nqp::add_i($pos, 1), 1))
          and nqp::isge_i($radix, 2) {
            # A string starting with 0x, 0d, 0o, or 0b,
            # followed by one optional '_'
            $pos   = nqp::add_i($pos, 2);
            $pos   = nqp::add_i($pos, 1)
                if nqp::islt_i($pos, $eos)
                && nqp::iseq_i(nqp::ord($str, $pos), 95);  # '_'

            parse-int-frac-exp();
        }
        elsif nqp::eqat($str,'Inf',$pos) {
            # 'Inf'
            $pos = nqp::add_i($pos, 3);
            $neg ?? -Inf !! Inf;
        }
        else {
            # Last chance: a simple decimal number
            $radix = 10;
            parse-int-frac-exp();
        }
    }

    my sub parse-real() {
        # Parse a simple number or a Rat numerator
        my $result := parse-simple-number();
        return $result if nqp::iseq_i($pos, $eos);

        # Check for '/' indicating Rat denominator
        if nqp::iseq_i(nqp::ord($str, $pos), 47) {  # '/'
            $pos = nqp::add_i($pos, 1);
            parse_fail "denominator expected after '/'"
                unless nqp::islt_i($pos, $eos);

            my $denom := parse-simple-number();

            $result := nqp::istype($result, Int) && nqp::istype($denom, Int)
                    ?? Rat.new($result, $denom)
                    !! $result / $denom;
        }

        $result;
    }

    # Parse a real number, magnitude of a pure imaginary number,
    # or real part of a complex number
    my $result := parse-real();
    parse_win $result if nqp::iseq_i($pos, $eos);

    # Check for 'i' or '\\i' indicating first parsed number was
    # the magnitude of a pure imaginary number
    if nqp::iseq_i(nqp::ord($str, $pos), 105) {  # 'i'
        parse_fail "Imaginary component of 'NaN' or 'Inf' must be followed by \\i"
            if nqp::isnanorinf($result.Num);
        $pos = nqp::add_i($pos, 1);
        $result := Complex.new(0, $result);
    }
    elsif nqp::eqat($str,'\\i',$pos) {
        $pos = nqp::add_i($pos, 2);
        $result := Complex.new(0, $result);
    }
    # Check for '+' or '-' indicating first parsed number was
    # the real part of a complex number
    elsif nqp::iseq_i(nqp::ord($str, $pos), 45)     # '-'
       || nqp::iseq_i(nqp::ord($str, $pos), 43)     # '+'
       || nqp::iseq_i(nqp::ord($str, $pos), 8722) { # '−'
        # Don't move $pos -- we want parse-real() to see the sign
        my $im := parse-real();
        parse_fail "imaginary part of complex number must be followed by 'i' or '\\i'"
            unless nqp::islt_i($pos, $eos);

        if nqp::iseq_i(nqp::ord($str, $pos), 105) {  # 'i'
            parse_fail "Imaginary component of 'NaN' or 'Inf' must be followed by \\i"
                if nqp::isnanorinf($im.Num);
            $pos = nqp::add_i($pos, 1);
        }
        elsif nqp::eqat($str,'\\i',$pos) {
            $pos = nqp::add_i($pos, 2);
        }
        else {
            parse_fail "imaginary part of complex number must be followed by 'i' or '\\i'"
        }

        $result := Complex.new($result, $im);
    }

    # Check for trailing garbage
    parse_fail "trailing characters after number"
        if nqp::islt_i($pos, $eos);

    parse_win $result;
}

# vim: expandtab shiftwidth=4
