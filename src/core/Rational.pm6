# stub of this role is also present in Numeric.pm6; be sure to update
# definition there as well, if changing this one
my role Rational[::NuT = Int, ::DeT = ::("NuT")] does Real {
    has NuT $.numerator   = 0;
    has DeT $.denominator = 1;

    multi method WHICH(Rational:D:) {
        self.REDUCE-ME;
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

    method new(NuT \nu = 0, DeT \de = 1) {
        my $new := nqp::create(self);

        # 0 denominator take it verbatim to support Inf/-Inf/NaN
        if de == 0 {
            nqp::bindattr($new,::?CLASS,'$!numerator',  nqp::decont(nu));
            nqp::bindattr($new,::?CLASS,'$!denominator',nqp::decont(de));
        }

        # normalize
        else {
            my $gcd        := nu gcd de;
            my $numerator   = nu div $gcd;
            my $denominator = de div $gcd;
            if $denominator < 0 {
                $numerator   = -$numerator;
                $denominator = -$denominator;
            }
            nqp::bindattr($new,::?CLASS,'$!numerator',  nqp::decont($numerator));
            nqp::bindattr($new,::?CLASS,'$!denominator',nqp::decont($denominator));
        }

        $new
    }

    method nude() { self.REDUCE-ME; $!numerator, $!denominator }

    method Num() {
        nqp::p6box_n(nqp::div_In(
          nqp::decont($!numerator),
          nqp::decont($!denominator)))
    }

    method floor(Rational:D:) {
        $!denominator == 1
            ?? $!numerator
            !! $!numerator div $!denominator
    }

    method ceiling(Rational:D:) {
        self.REDUCE-ME;
        $!denominator == 1
            ?? $!numerator
            !! ($!numerator div $!denominator + 1)
    }

    method Int() {
        $!denominator
            ?? self.truncate
            !! fail X::Numeric::DivideByZero.new:
                   :details('when coercing Rational to Int')
    }

    multi method Bool(::?CLASS:D:) { nqp::p6bool($!numerator) }

    method Bridge() { self.Num }

    method Range(::?CLASS:U:) { Range.new(-Inf, Inf) }

    method isNaN (--> Bool:D) {
        nqp::p6bool(nqp::isfalse($!denominator) && nqp::isfalse($!numerator))
    }

    method is-prime(--> Bool:D) {
        self.REDUCE-ME;
        nqp::if($!denominator == 1,$!numerator.is-prime)
    }

    multi method Str(::?CLASS:D:) {
        my $whole  = self.abs.floor;
        my $fract  = self.abs - $whole;

        # fight floating point noise issues RT#126016
        if $fract.Num == 1e0 && nqp::eqaddr(self.WHAT,Rat) {
            $whole += 1;
            $fract = 0;
        }

        my $result = nqp::if(
            nqp::islt_I($!numerator, 0), '-', ''
        ) ~ $whole;

        if $fract {
            my $precision;
            # Stringify Rats to at least 6 significant digits. There does not
            # appear to be any written spec for this but there are tests in
            # roast that specifically test for 6 digits.
            if nqp::eqaddr(self.WHAT,Rat) {
                if $!denominator < 100000 {
                    $precision = 6;
                    $fract *= 1000000;
                }
                else {
                    $precision = nqp::chars($!denominator.Str) + 1;
                    $fract *= nqp::pow_I(10, nqp::decont($precision), Num, Int);
                }
            }
            else {
                # TODO v6.d FatRats are tested in roast to have a minimum
                # precision pf 6 decimal places - mostly due to there being no
                # formal spec and the desire to test SOMETHING. With this
                # speed increase, 16 digits would work fine; but it isn't spec.  
                #if $!denominator < 1000000000000000 {
                #    $precision = 16;
                #    $fract *= 10000000000000000;
                #}
                if $!denominator < 100000 {
                    $precision = 6;
                    $fract *= 1000000;
                }
                else {
                    $precision = nqp::chars($!denominator.Str) + nqp::chars($whole.Str) + 1;
                    $fract *= nqp::pow_I(10, nqp::decont($precision), Num, Int);
                }
            }
            my $f  = $fract.round;
            my $fc = nqp::chars($f.Str);
            $f div= 10 while $f %% 10; # Remove trailing zeros
            $result ~= '.' ~ '0' x ($precision - $fc) ~ $f;
        }
        $result
    }

    method base($base, Any $digits? is copy) {
        # XXX TODO: this $base check can be delegated to Int.base once Num/0 gives Inf/NaN,
        # instead of throwing (which happens in the .log() call before we reach Int.base
        2 <= $base <= 36 or Failure.new(X::OutOfRange.new(
            what => "base argument to base", :got($base), :range<2..36>)
        );

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
            $prec = ($!denominator < $base**6 ?? 6 !! $!denominator.log($base).ceiling + 1);
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
            for @fract-digits-1 ... 0 -> $n {
                last if ++@fract-digits[$n] < $base;
                @fract-digits[$n] = 0;
                $result = $sign ~ ($whole+1).base($base) if $n == 0;
            }
        }

        @fract-digits
            ?? $result ~ '.' ~ @conversion[@fract-digits].join
            !! $result;
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

    method norm() { self.REDUCE-ME; self }

    method narrow(::?CLASS:D:) {
        self.REDUCE-ME;
        $!denominator == 1
            ?? $!numerator
            !! self;
    }

    method REDUCE-ME(--> Nil) {
        if $!denominator > 1 {
            my $gcd = $!denominator gcd $!numerator;
            if $gcd > 1 {
                nqp::bindattr(self,self.WHAT,'$!numerator',  $!numerator   div $gcd);
                nqp::bindattr(self,self.WHAT,'$!denominator',$!denominator div $gcd);
            }
        }
    }
}

# vim: ft=perl6 expandtab sw=4
