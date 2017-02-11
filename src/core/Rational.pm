my role Rational[::NuT, ::DeT] does Real {
    has NuT $.numerator   = 0;
    has DeT $.denominator = 1;

    multi method WHICH(Rational:D:) {
        nqp::box_s(
            nqp::concat(
                nqp::concat(nqp::unbox_s(self.^name), '|'),
                nqp::concat(
                    nqp::tostr_I($!numerator),
                    nqp::concat('/', nqp::tostr_I($!denominator))
                )
            ),
            ObjAt
        );
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
        nqp::istype($!numerator,Int)
          ?? nqp::p6box_n(nqp::div_In(
               nqp::decont($!numerator),
               nqp::decont($!denominator)
             ))
          !! $!numerator
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

    method Int() { self.truncate }
    method Bridge() { self.Num }
    method Range(::?CLASS:U:) { Range.new(-Inf, Inf) }
    method isNaN {
        nqp::p6bool(
            nqp::isfalse(self.numerator) && nqp::isfalse(self.denominator)
        )
    }

    multi method Str(::?CLASS:D:) {
        if nqp::istype($!numerator,Int) {
            my $whole  = self.abs.floor;
            my $fract  = self.abs - $whole;

            # fight floating point noise issues RT#126016
            if $fract.Num == 1e0 { $whole++; $fract = 0 }

            my $result = nqp::if(
                nqp::islt_I($!numerator, 0), '-', ''
            ) ~ $whole;

            if $fract {
                my $precision = $!denominator < 100_000
                    ?? 6 !! $!denominator.Str.chars + 1;

                my $fract-result = '';
                while $fract and $fract-result.chars < $precision {
                    $fract *= 10;
                    given $fract.floor {
                        $fract-result ~= $_;
                        $fract        -= $_;
                    }
                }
                $fract-result++ if 2*$fract >= 1; # round off fractional result

                $result ~= '.' ~ $fract-result;
            }
            $result
        }
        else {
            $!numerator.Str
        }
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
        @quotients.=map(*.base($base));
        my @cycle = $nu
          ?? splice(@quotients, @remainders.first($nu,:k) + 1)
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
        self.REDUCE-ME;
        $!denominator == 1
            ?? $!numerator
            !! self;
    }

    method REDUCE-ME(--> Nil) {
        if $!denominator > 1 {
            my $gcd = $!denominator gcd $!numerator;
            if $gcd > 1 {
                nqp::bindattr(self, self.WHAT, '$!numerator',     $!numerator   div $gcd);
                nqp::bindattr(self, self.WHAT, '$!denominator',   $!denominator div $gcd);
            }
        }
    }
}

# vim: ft=perl6 expandtab sw=4
