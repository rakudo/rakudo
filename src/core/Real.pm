class Complex { ... }

# XxX role Real does Numeric { ... }
my class Real {
    method abs()  { self < 0 ?? -self !! self }
    proto method sign(|$) {*}
    multi method sign(Real:U:) { Mu }
    multi method sign(Real:D:) { self < 0 ?? -1 !! self == 0 ?? 0 !! 1 }
    method sqrt() { self.Bridge.sqrt }
    method sin()  { self.Bridge.sin }
    method cos()  { self.Bridge.cos }
    method tan()  { self.Bridge.tan }
    method sinh() { self.Bridge.sinh }
    method cosh() { self.Bridge.cosh }
    method tanh() { self.Bridge.tanh }
    proto method atan2(|$) {*}
    multi method atan2(Real $x = 1e0) { self.Bridge.atan2($x.Bridge) }
    method floor() { self.Bridge.floor }
    method ceiling() { self.Bridge.ceiling }
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

multi infix:<!=>(Real \$a, Real \$b)  { $a.Bridge != $b.Bridge }

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
multi sub atan2(Cool \$a, Cool \$b = 1e0) { $a.Real.atan2($b.Real) }

