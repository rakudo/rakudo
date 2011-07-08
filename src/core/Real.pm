class Complex { ... }

# XxX role Real does Numeric { ... }
my class Real {
    method abs()  { self.Bridge.abs }
    method sqrt() { self.Bridge.sqrt }
    method sin()  { self.Bridge.sin }
    method cos()  { self.Bridge.cos }
    method tan()  { self.Bridge.tan }
    method sinh() { self.Bridge.sinh }
    method cosh() { self.Bridge.cosh }
    method tanh() { self.Bridge.tanh }
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

