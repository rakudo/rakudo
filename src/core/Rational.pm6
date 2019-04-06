# stub of this role is also present in Numeric.pm6; be sure to update
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
        my \object := nqp::create(self);
        nqp::if(
          de,
          nqp::stmts(                                   # normal rational
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
          nqp::stmts(                                   # Inf / NaN
            nqp::bindattr(object,::?CLASS,'$!numerator',
              nqp::box_i(
                nqp::isgt_I(nqp::decont(nu),0) || nqp::neg_i(nqp::istrue(nu)),
                nu.WHAT
              )
            ),
            nqp::p6bindattrinvres(object,::?CLASS,'$!denominator',
              nqp::decont(de)
            )
          )
        )
    }

    method nude() { $!numerator, $!denominator }

    method Num(--> Num:D) {
        nqp::p6box_n(nqp::div_In($!numerator,$!denominator))
    }

    method floor(Rational:D: --> Int:D) {
      $!denominator
        ?? $!denominator == 1
          ?? $!numerator
          !! $!numerator div $!denominator
        !! Failure.new(
             X::Numeric::DivideByZero.new(
               :details('when calling .floor on Rational')
             )
           )
    }

    method ceiling(Rational:D: --> Int:D) {
      $!denominator
        ?? $!denominator == 1
          ?? $!numerator
          !! ($!numerator div $!denominator + 1)
        !! Failure.new(
             X::Numeric::DivideByZero.new(
               :details('when calling .ceiling on Rational')
             )
           )
    }

    method Int(--> Int:D) {
        $!denominator
          ?? self.truncate
          !! Failure.new(
               X::Numeric::DivideByZero.new(
                 :details('when coercing Rational to Int')
               )
             )
    }

    multi method Bool(::?CLASS:D:) { nqp::hllbool(nqp::istrue($!numerator)) }

    method Bridge() { self.Num }

    method Range(::?CLASS:U:) { Range.new(-Inf, Inf) }

    method isNaN (--> Bool:D) {
        nqp::hllbool(nqp::isfalse($!denominator) && nqp::isfalse($!numerator))
    }

    method is-prime(--> Bool:D) {
        nqp::if($!denominator == 1,$!numerator.is-prime)
    }

    multi method Str(::?CLASS:D: --> Str:D) {
        nqp::if(
          $!denominator,
          nqp::stmts(
            (my $abs   := self.abs),
            (my $whole := $abs.floor),
            (my $fract := $abs - $whole),
            nqp::if(
              $fract,
              self!SLOW-STR($whole,$fract),
              nqp::if(
                nqp::islt_I($!numerator,0),
                nqp::concat("-",nqp::tostr_I($whole)),
                nqp::tostr_I($whole)
              )
            )
          ),
          X::Numeric::DivideByZero.new(
            :details('when coercing Rational to Str')
          ).throw
        )
    }

    method !SLOW-STR(\whole, \fract) {

        # fight floating point noise issues RT#126016
        fract.Num == 1e0 && nqp::eqaddr(self.WHAT,Rat)
          ?? nqp::islt_I($!numerator,0)
            ?? nqp::concat("-",nqp::tostr_I(whole + 1))
            !! nqp::tostr_I(whole + 1)
          !! self!STRINGIFY(
               whole,
               fract,
               nqp::eqaddr(self.WHAT,Rat)
        # Stringify Rats to at least 6 significant digits. There does not
        # appear to be any written spec for this but there are tests in
        # roast that specifically test for 6 digits.
                 ?? $!denominator < 100_000
                   ?? 6
                   !! (nqp::chars($!denominator.Str) + 1)
        # TODO v6.d FatRats are tested in roast to have a minimum
        # precision pf 6 decimal places - mostly due to there being no
        # formal spec and the desire to test SOMETHING. With this
        # speed increase, 16 digits would work fine; but it isn't spec.
        #        !! $!denominator < 1_000_000_000_000_000
        #          ?? 16
                 !! $!denominator < 100_000
                   ?? 6
                   !! (nqp::chars($!denominator.Str)
                        + nqp::chars(whole.Str)
                        + 1
                      )
             )
    }

    method !STRINGIFY(\whole, \fract, Int:D $precision) {
        my $f := (fract * nqp::pow_I(10, $precision, Num, Int)).round;
        my $fc = nqp::chars($f.Str);
        $f := $f div 10 while $f %% 10; # Remove trailing zeros
        (nqp::isle_I($!numerator,0) ?? "-" !! "")
          ~ whole
          ~ '.'
          ~ '0' x ($precision - $fc)
          ~ $f
    }

    method base($base, Any $digits? is copy) {
        # XXX TODO: this $base check can be delegated to Int.base once Num/0 gives Inf/NaN,
        # instead of throwing (which happens in the .log() call before we reach Int.base
        2 <= $base <= 36 or fail X::OutOfRange.new(
            what => "base argument to base", :got($base), :range<2..36>);

        my $prec;
        if $digits ~~ Whatever {
            $digits = Nil;
            $prec = 2**63;
        }
        elsif $digits.defined {
            $digits = $digits.Int;
            if $digits > 0 {
                $prec = $digits;
            }
            elsif $digits == 0 {
                return self.round.base($base)
            }
            else {
                fail X::OutOfRange.new(
                    :what('digits argument to base'), :got($digits),
                    :range<0..^Inf>,
                )
            }
        }
        else {
            # Limit log calculation to 10**307 or less.
            # log coerces to Num. When larger than 10**307, it overflows and
            # returns Inf.
            my $lim = 10**307;
            if $!denominator < $lim {
                $prec = ($!denominator < $base**6 ?? 6 !! $!denominator.log($base).ceiling + 1);
            }
            else {
                # If the internal log method is modified to handle larger numbers,
                # this branch can be modified/removed.
                my $d = $!denominator;
                my $exp = 0;
                ++$exp while ($d div= $base) > $lim;
                $prec = $exp + $d.log($base).ceiling + 2;
            }
        }

        my $sign  = nqp::if( nqp::islt_I($!numerator, 0), '-', '' );
        my $whole = self.abs.floor;
        my $fract = self.abs - $whole;

        # fight floating point noise issues RT#126016
        if $fract.Num == 1e0 { $whole++; $fract = 0 }

        my $result = $sign ~ $whole.base($base);
        my @conversion := <0 1 2 3 4 5 6 7 8 9
                           A B C D E F G H I J
                           K L M N O P Q R S T
                           U V W X Y Z>;

        my @fract-digits;
        while @fract-digits < $prec and ($digits // $fract) {
            $fract *= $base;
            my $digit = $fract.floor;
            push @fract-digits, $digit;
            $fract -= $digit;
        }

        # Round the final number, based on the remaining fractional part
        if 2*$fract >= 1 {
            for @fract-digits - 1 ... 0 -> $n {
                last if ++@fract-digits[$n] < $base;
                @fract-digits[$n] = 0;
                $result = $sign ~ ($whole+1).base($base) if $n == 0;
            }
        }

        $result ~
        (@fract-digits ??
         $base <= 10   ?? '.' ~ @fract-digits.join !!
         '.' ~ @conversion[@fract-digits].join     !! '')
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
          !! Failure.new(
               X::Numeric::DivideByZero.new(
                 :details('when calling .round on Rational')
               )
             )
    }
}

# vim: ft=perl6 expandtab sw=4
