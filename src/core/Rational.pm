my role Rational[::NuT, ::DeT] does Real {
    has NuT $.numerator;
    has DeT $.denominator;

    multi method WHICH(Rational:D:) {
        nqp::box_s(
            nqp::concat_s(
                nqp::concat_s(nqp::unbox_s(self.^name), '|'),
                nqp::concat_s(
                    nqp::tostr_I($!numerator),
                    nqp::concat_s('/', nqp::tostr_I($!denominator))
                )
            ),
            ObjAt
        );
    }

    method new(NuT \nu = 0, DeT \de = 1) {
        my $new     := nqp::create(self);
        my $gcd     := nu gcd de;
        my $numerator   = nu div $gcd;
        my $denominator = de div $gcd;
        if $denominator < 0 {
            $numerator   = -$numerator;
            $denominator = -$denominator;
        }
        nqp::bindattr($new, self.WHAT, '$!numerator',     nqp::p6decont($numerator));
        nqp::bindattr($new, self.WHAT, '$!denominator',   nqp::p6decont($denominator));
        $new;
    }

    method nude() { $!numerator, $!denominator }
    method Num() {
        $!denominator == 0
          ?? ($!numerator < 0 ?? -$Inf !! $Inf)
          !! nqp::p6box_n(nqp::div_In(
                nqp::p6decont($!numerator),
                nqp::p6decont($!denominator)
             ));
    }

    method floor(Rational:D:) returns Int:D {
        $!denominator == 1
            ?? $!numerator
            !! $!numerator < 0
            ?? ($!numerator div $!denominator - 1) # XXX because div for negati
            !! $!numerator div $!denominator
    }

    method ceiling(Rational:D:) returns Int:D {
        $!denominator == 1
            ?? $!numerator
            !! $!numerator < 0
            ?? ($!numerator div $!denominator) # XXX should be +1, but div is buggy
            !! ($!numerator div $!denominator + 1)
    }

    method Int() { $!numerator div $!denominator }

    method Bridge() { self.Num }

    multi method Str(::?CLASS:D:) {
        my $s = $!numerator < 0 ?? '-' !! '';
        my $r = self.abs;
        my $i = $r.floor;
        $r -= $i;
        $s ~= $i;
        if $r {
            $s ~= '.';
            my $want = $!denominator < 100_000
                       ?? 6
                       !! $!denominator.Str.chars + 1;
            my $f = '';
            while $r and $f.chars < $want {
                $r *= 10;
                $i = $r.floor;
                $f ~= $i;
                $r -= $i;
            }
            $f++ if  2 * $r >= 1;
            $s ~= $f;
        }
        $s;
    }

    method base($base) {
        my $s = $!numerator < 0 ?? '-' !! '';
        my $r = self.abs;
        my $i = $r.floor;
        $r -= $i;
        $s ~= $i.base($base);
        if $r {
            my $want = $!denominator < $base**6 ?? 6 !! $!denominator.log($base).ceiling + 1;
            my @f;
            while $r and @f < $want {
                $r *= $base;
                $i = $r.floor;
                push @f, $i;
                $r -= $i;
            }
            if 2 * $r >= 1 {
                for @f-1 ... 0 -> $x {
                    last if ++@f[$x] < $base;
                    @f[$x] = 0;
                    $s ~= ($i+1).base($base) if $x == 0; # never happens?
                }
            }
            $s ~= '.';
            $s ~= (0..9,'A'..'Z')[@f].join;
        }
        $s;
    }

    method succ {
        self.new($!numerator + $!denominator, $!denominator);
    }

    method pred {
        self.new($!numerator - $!denominator, $!denominator);
    }

    method norm() { self }
}

