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
}


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

