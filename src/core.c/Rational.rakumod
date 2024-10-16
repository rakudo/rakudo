# stub of this role is also present in Numeric.rakumod; be sure to update
# definition there as well, if changing this one
my role Rational[::NuT = Int, ::DeT = ::("NuT")] does Real {
    has NuT $.numerator;
    has DeT $.denominator;

    multi method WHICH(Rational:D: --> ValueObjAt:D) {
        nqp::box_s(
          nqp::concat(
            nqp::if(
              nqp::eqaddr(self.WHAT,Rational),
              'Rational|',
              nqp::concat(nqp::unbox_s(self.^name), '|')
            ),
            nqp::concat(
              nqp::tostr_I($!numerator),
              nqp::concat('/', nqp::tostr_I($!denominator))
            )
          ),
          ValueObjAt
        )
    }

    method new(NuT:D \nu = 0, DeT:D \de = 1) {
        nqp::if(
          de,
          nqp::stmts(                                   # normal rational
            (my \object := nqp::create(self)),
            (my \gcd := nqp::gcd_I(nqp::decont(nu), nqp::decont(de), Int)),
            (my \numerator   := nqp::div_I(nqp::decont(nu), gcd, NuT)),
            (my \denominator := nqp::div_I(nqp::decont(de), gcd, DeT)),
            nqp::if(
              nqp::islt_I(denominator,0),               # need to switch sign?
              nqp::stmts(                                # yup, so switch
                nqp::bindattr(
                  object,::?CLASS,'$!numerator',nqp::neg_I(numerator,Int)
                ),
                nqp::p6bindattrinvres(
                  object,::?CLASS,'$!denominator',nqp::neg_I(denominator,Int)
                )
              ),
              nqp::stmts(                                # no, so just store
                nqp::bindattr(
                  object,::?CLASS,'$!numerator',numerator
                ),
                nqp::p6bindattrinvres(
                  object,::?CLASS,'$!denominator',denominator
                )
              )
            )
          ),
          self.zero-denominator(
            nqp::box_i(
              nqp::isgt_I(nqp::decont(nu),0) || nqp::neg_i(nqp::istrue(nu)),
              nu.WHAT
            ),
            de
          )
        )
    }

    # Special creator that keeps a backtrace to be produced whenever a
    # Rational with a zero denominator is used
    method zero-denominator(Int:D $nu, Int:D $de) is implementation-detail {
        my role Here {
            has $!failure is built(:bind);
            method throw() { $!failure.throw }
            method raku() { callsame.subst('+{Rational::Here}') }
        }

        # Create handled failure to prevent noise at destruction
        (my $failure := Failure.new(
          X::Numeric::DivideByZero.new(numerator => $nu)
        )).defined;

        nqp::p6bindattrinvres(
          nqp::p6bindattrinvres(nqp::create(self),$?CLASS,'$!numerator',$nu),
          $?CLASS,'$!denominator',$de
        ) but Here($failure)
    }

    multi method raku(Rational:D: --> Str:D) {
        "Rational.new($!numerator, $!denominator)"
    }

    method nude() { $!numerator, $!denominator }

    method Num(Rational:D: --> Num:D) {
        $!denominator || $!numerator >= 0
          ?? nqp::p6box_n(nqp::div_In($!numerator,$!denominator))
          !! -Inf
    }

    method !divide-by-zero(Str:D $what) {
        X::Numeric::DivideByZero.new(
          :details("when calling .$what on Rational")
        ).Failure
    }

    method floor(Rational:D: --> Int:D) {
      $!denominator
        ?? $!denominator == 1
          ?? $!numerator
          !! $!numerator div $!denominator
        !! self!divide-by-zero('floor')
    }

    method ceiling(Rational:D: --> Int:D) {
      $!denominator
        ?? $!denominator == 1
          ?? $!numerator
          !! ($!numerator div $!denominator + 1)
        !! self!divide-by-zero('ceiling')
    }

    method Int(Rational:D: --> Int:D) {
        $!denominator
          ?? self.truncate
          !! self!divide-by-zero('Int')
    }

    multi method Bool(::?CLASS:D:) { nqp::hllbool(nqp::istrue($!numerator)) }

    method Range(::?CLASS:U:) { Range.new(-Inf, Inf) }

    method isNaN (--> Bool:D) {
        nqp::hllbool(nqp::isfalse($!denominator) && nqp::isfalse($!numerator))
    }

    method is-prime(--> Bool:D) {
        $!denominator == 1 && $!numerator.is-prime
    }

    multi method Str(::?CLASS:D: --> Str:D) {
        if $!denominator {
            my \abs   := self.abs;                              # N / D
            my \whole := abs.floor;
            (my \fract := abs - whole)
              # fight floating point noise issues https://github.com/Raku/old-issue-tracker/issues/4524
              ?? nqp::eqaddr(self.WHAT,Rat) && fract.Num == 1e0  # 42.666?
                ?? self!UNITS(nqp::add_I(whole,1,Int))           # next Int
                !! self!STRINGIFY(self!UNITS(whole), fract,      # 42.666
                     nqp::eqaddr(self.WHAT,Rat)
        # Stringify Rats to at least 6 significant digits. There does not
        # appear to be any written spec for this but there are tests in
        # roast that specifically test for 6 digits.
                       ?? nqp::islt_I($!denominator,100_000)
                         ?? 6
                         !! (nqp::chars(nqp::tostr_I($!denominator)) + 1)
        # TODO v6.d FatRats are tested in roast to have a minimum
        # precision pf 6 decimal places - mostly due to there being no
        # formal spec and the desire to test SOMETHING. With this
        # speed increase, 16 digits would work fine; but it isn't spec.
        #              !! $!denominator < 1_000_000_000_000_000
        #                ?? 16
                       !! $!denominator < 100_000
                         ?? 6
                         !! (nqp::chars(nqp::tostr_I($!denominator))
                              + nqp::chars(nqp::tostr_I(whole))
                              + 5
                            )
                   )
              !! self!UNITS(whole)                               # no fract val
        }
        else {                                                   # N / 0
            self.throw
        }
    }

    method !UNITS(Int:D $whole --> Str:D) {
        nqp::islt_I($!numerator,0)                  # next Int
          ?? nqp::concat("-",nqp::tostr_I($whole))    # < 0
          !! nqp::tostr_I($whole)                     # >= 0
    }

    method !STRINGIFY(str $units, $fract, int $digits) {
        my str $s = nqp::tostr_I(
          ($fract * nqp::pow_I(10,$digits,Num,Int)).round
        );
        $s = nqp::concat(nqp::x('0',$digits - nqp::chars($s)),$s)
          if nqp::chars($s) < $digits;

        my int $i = nqp::chars($s);
        nqp::while(
          nqp::eqat($s,'0',$i - 1) && --$i > 0,
          nqp::null
        );

        $i
          ?? nqp::concat($units,nqp::concat('.',nqp::substr($s,0,$i)))
          !! $units
    }

    sub BASE_OUT_OF_RANGE(int $got) {
        X::OutOfRange.new(
          :what('base argument to base'),:$got,:range<2..36>
        ).Failure
    }
    sub DIGITS_OUT_OF_RANGE(int $got) {
        X::OutOfRange.new(
          :what('digits argument to base'),:$got,:range<2..36>
        ).Failure
    }

    proto method base(|) {*}
    # Convert to given base with sensible number of digits depending on value
    multi method base(Rational:D: Int:D $base --> Str:D) {
        if 2 <= $base <= 36 {
            # Limit log calculation to 10**307 or less.
            # log coerces to Num. When larger than 10**307, it overflows and
            # returns Inf.
            my constant $lim = 10**307;
            if $!denominator < $lim {
                self!base(
                  $base,
                  $!denominator < $base**6
                    ?? 6
                    !! $!denominator.log($base).ceiling + 1,
                    0
                )
            }
            else {
                # If the internal log method is modified to handle larger
                # numbers, this branch can be modified/removed.
                my $d = $!denominator;
                my $exp = 0;
                ++$exp while ($d div= $base) > $lim;
                self!base($base, $exp + $d.log($base).ceiling + 2, 0)
            }
        }
        else {
            BASE_OUT_OF_RANGE($base)
        }
    }
    # Convert to given base until no fraction left: **CAUTION** this will
    # loop indefinitely for simple values such as 1/3
    multi method base(Rational:D: Int:D $base, Whatever --> Str:D) {
        2 <= $base <= 36
          ?? self!base($base, 0, 0)
          !! BASE_OUT_OF_RANGE($base)
    }

    # Convert to given base for given number of digits.  This will display
    # trailing 0's if number of digits exceeds accuracy of value, unless
    # inhibited with the :no-trailing-zeroes named argument.
    multi method base(Rational:D:
      Int:D $base,
      Int() $digits,
      Bool:D :$no-trailing-zeroes = False
    --> Str:D) {
        2 <= $base <= 36
          ?? $digits > 0
            ?? self!base($base, $digits, nqp::not_i($no-trailing-zeroes))
            !! $digits == 0
              ?? self.round.base($base)
              !! DIGITS_OUT_OF_RANGE($digits)
          !! BASE_OUT_OF_RANGE($base)
    }

    # Lookup table for converting from numerical value to string digit
    my str $num2digit = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";

    # Actual .base conversion workhorse.  Takes base, precision (0 for
    # conversion until no fractional part left) and a flag indicating
    # whether the preserve trailing zeroes
    method !base(
      Int:D $base,             # radix to output value in
      int $digits,             # number of digits to generate, 0 = indefinite
      int $trailing-zeroes,    # do not remove trailing zeroes
    --> Str:D) {
        my $result := nqp::list_s;

        # set up initial values
        my $abs;
        if nqp::islt_I($!numerator,0) {
            nqp::push_s($result,'-');
            $abs := -self;
        }
        else {
            $abs := self;
        }
        my $whole := $abs.floor;
        my $fract := $abs - $whole;

        # fight floating point noise issues https://github.com/Raku/old-issue-tracker/issues/4524
        if $fract.Num == 1e0 {
            $whole := $whole + 1;
            $fract := 0;
        }

        # have something after the decimal point
        if $fract {

            # we have a specific precision in mind
            if $digits {
                my str $s = ($fract * $base**$digits).round.base($base);
                my int $force-decimal;
                if nqp::chars($s) > $digits {
                    $whole := $whole + 1;
                    $s = nqp::substr($s,1);
                    $force-decimal = 1;
                }
                elsif nqp::chars($s) < $digits {
                    $s = nqp::concat(nqp::x('0',$digits - nqp::chars($s)),$s);
                }

                my int $i = nqp::chars($s);
                if $trailing-zeroes {                # we want trailing zeroes
                    nqp::while(
                      nqp::eqat($s,'0',--$i) && $i >= $digits,
                      nqp::null
                    );
                    ++$i; # correct for premature decrement
                }
                else {                               # no trailing zeroes
                    nqp::while(
                      nqp::eqat($s,'0',$i - 1) && --$i > 0,
                      nqp::null
                    );
                }

                nqp::push_s($result,$whole.base($base));
                if $i || $force-decimal {
                    nqp::push_s($result,'.');
                    nqp::push_s($result,nqp::substr($s,0,$i));
                }
            }

            # no precision, go on until nothing left, possibly forever
            else {
                nqp::push_s($result,$whole.base($base));
                nqp::push_s($result,'.');
                while $fract {
                    $fract    := $fract * $base;
                    my $digit := $fract.floor;
                    nqp::push_s($result,nqp::substr($num2digit,$digit,1));
                    $fract := $fract - $digit;
                }
            }
        }

        # nothing after decimal point
        else {
            nqp::push_s($result,$whole.base($base));
            if $digits && $trailing-zeroes {
                nqp::push_s($result,'.');
                nqp::push_s($result,nqp::x('0',$digits))
            }
        }

        nqp::join('',$result)
    }

    method base-repeating($base = 10) {
        return ~self, '' if self.narrow ~~ Int;
        my @quotients;
        my @remainders;
        my %remainders;
        push @quotients, [div] my ($nu, $de) = abs(self).nude;
        loop {
            push @remainders, $nu %= $de;
            last if %remainders{$nu}++ or $nu == 0;
            $nu *= $base;
            push @quotients, $nu div $de;
        }
        @quotients .= map(*.base($base));
        my @cycle = $nu
          ?? splice @quotients, @remainders.first($nu,:k) + 1
          !! ();
        splice @quotients, 1, 0, '.';
        '-' x (self < 0) ~ @quotients.join, @cycle.join;
    }

    method succ {
        self.new($!numerator + $!denominator, $!denominator);
    }

    method pred {
        self.new($!numerator - $!denominator, $!denominator);
    }

    method norm() { self }

    method narrow(::?CLASS:D:) {
        $!denominator == 1
            ?? $!numerator
            !! self;
    }

    multi method round(::?CLASS:D: --> Int:D) {
        $!denominator
          ?? nqp::div_I(
               nqp::add_I(nqp::mul_I($!numerator, 2, Int), $!denominator, Int),
               nqp::mul_I($!denominator, 2, Int),
               Int
             )
          !! self!divide-by-zero('round')
    }
}

# vim: expandtab shiftwidth=4
