my class Complex { ... }

# XxX role Real does Numeric { ... }
my class Real does Numeric {
    method Rat(Real:D: Real $epsilon = 1.0e-6) { self.Bridge.Rat($epsilon) }
    method abs()  { self < 0 ?? -self !! self }
    proto method sign(|$) {*}
    multi method sign(Real:U:) { Mu }
    multi method sign(Real:D:) { self < 0 ?? -1 !! self == 0 ?? 0 !! 1 }
    method conj(Real:D:) { self }
    method sqrt() { self.Bridge.sqrt }
    method sin()  { self.Bridge.sin }
    method asin() { self.Bridge.asin }
    method cos()  { self.Bridge.cos }
    method acos() { self.Bridge.acos }
    method tan()  { self.Bridge.tan }
    method atan() { self.Bridge.atan }
    proto method atan2(|$) {*}
    multi method atan2(Real $x = 1e0) { self.Bridge.atan2($x.Bridge) }
    multi method atan2(Cool $x = 1e0) { self.Bridge.atan2($x.Numeric.Bridge) }
    method sec() { self.Bridge.sec }
    method asec() { self.Bridge.asec }
    method cosec() { self.Bridge.cosec }
    method acosec() { self.Bridge.acosec }
    method cotan()  { self.Bridge.cotan }
    method acotan() { self.Bridge.acotan }
    method sinh() { self.Bridge.sinh }
    method asinh() { self.Bridge.asinh }
    method cosh() { self.Bridge.cosh }
    method acosh() { self.Bridge.acosh }
    method tanh() { self.Bridge.tanh }
    method atanh() { self.Bridge.atanh }
    method sech() { self.Bridge.sech }
    method asech() { self.Bridge.asech }
    method cosech() { self.Bridge.cosech }
    method acosech() { self.Bridge.acosech }
    method cotanh() { self.Bridge.cotanh }
    method acotanh() { self.Bridge.acotanh }
    method floor() { self.Bridge.floor }
    method ceiling() { self.Bridge.ceiling }
    # cannot use '0.5' here, because Rat isn't initialized yet
    method round($scale = 1) { (self / $scale + 1/2).floor * $scale }
    method unpolar(Real $angle) {
        Complex.new(self * $angle.cos, self * $angle.sin);
    }
    method cis() {
        Complex.new(self.cos, self.sin);
    }
    method Complex() { Complex.new(self.Num, 0e0) }
    multi method log()           { self.Bridge.log               }
    multi method log(Real $base) { self.Bridge.log($base.Bridge) }
    multi method exp()           { self.Bridge.exp               }
    method truncate(Real:D:) {
        self == 0 ?? 0 !! self < 0  ?? self.ceiling !! self.floor
    }
    method isNaN { Bool::False }

    method base(Int:D $base) {
        my Int $int_part = self.Int;
        my $frac = abs(self - $int_part);
        my @frac_digits;
        my @conversion = qw/0 1 2 3 4 5 6 7 8 9
                            A B C D E F G H I J
                            K L M N O P Q R S T/;
        # pretty arbitrary precision limit for now
        # but better than endless loops
        my $limit = 1e8.log($base.Num).Int;
        for ^$limit {
            last if $frac == 0;
            $frac = $frac * $base;
            push @frac_digits, @conversion[$frac.Int];
            $frac = $frac - $frac.Int;
        }
        my Str $r = $int_part.base($base) ~ '.' ~ @frac_digits.join('');
        # if $int_part is 0, $int_part.base doesn't see the sign of self
        $int_part == 0 && self < 0 ?? '-' ~ $r !! $r;
    }

    method Real(Real:D:) { self }
    method Bridge(Real:D:) { self.Num }
}

proto sub cis(|$) {*}
multi sub cis(Real $a) { $a.cis }

multi infix:<+>(Real \$a, Real \$b)   { $a.Bridge + $b.Bridge }

multi infix:<->(Real \$a, Real \$b)   { $a.Bridge - $b.Bridge }

multi infix:<*>(Real \$a, Real \$b)   { $a.Bridge * $b.Bridge }

multi infix:</>(Real \$a, Real \$b)   { $a.Bridge / $b.Bridge }

multi infix:<%>(Real \$a, Real \$b)   { $a.Bridge % $b.Bridge }

multi infix:<**>(Real \$a, Real \$b)  { $a.Bridge ** $b.Bridge }

multi infix:«<=>»(Real \$a, Real \$b) { $a.Bridge <=> $b.Bridge }

multi infix:<==>(Real \$a, Real \$b)  { $a.Bridge == $b.Bridge }

multi infix:«<»(Real \$a, Real \$b)   { $a.Bridge < $b.Bridge }

multi infix:«<=»(Real \$a, Real \$b)  { $a.Bridge <= $b.Bridge }

multi infix:«>»(Real \$a, Real \$b)   { $a.Bridge > $b.Bridge }

multi infix:«>=»(Real \$a, Real \$b)  { $a.Bridge >= $b.Bridge }

multi infix:<cmp>(Real \$a, Real \$b) { $a.Bridge cmp $b.Bridge }

proto sub infix:<mod>(|$) {*}
multi sub infix:<mod>(Real $a, Real $b) {
    $a - ($a div $b) * $b;
}

multi prefix:<abs>(Real \$a) {
    $a < 0 ?? -$a !! $a;
}

proto sub truncate(|$) {*}
multi sub truncate(Real:D $x) { $x.truncate }
multi sub truncate(Cool:D $x) { $x.Numeric.truncate }


proto sub atan2(|$)    { * }
multi sub atan2(Real \$a, Real \$b = 1e0) { $a.Bridge.atan2($b.Bridge) }
# should really be (Cool, Cool), and then (Cool, Real) and (Real, Cool)
# candidates, but since Int both conforms to Cool and Real, we'd get lots
# of ambiguous dispatches. So just go with (Any, Any) for now.
multi sub atan2(     \$a,      \$b = 1e0) { $a.Numeric.atan2($b.Numeric) }

