# XxX role Real does Numeric { ... }
my class Real {
}


multi infix:<+>(Real \$a, Real \$b)   { $a.Bridge + $b.Bridge }

multi infix:<*>(Real \$a, Real \$b)   { $a.Bridge * $b.Bridge }

multi infix:</>(Real \$a, Real \$b)   { $a.Bridge / $b.Bridge }

multi infix:<%>(Real \$a, Real \$b)   { $a.Bridge % $b.Bridge }

multi infix:<**>(Real \$a, Real \$b)  { $a.Bridge ** $b.Bridge }

multi infix:<==>(Real \$a, Real \$b)  { $a.Bridge == $b.Bridge }

multi infix:<!=>(Real \$a, Real \$b)  { $a.Bridge != $b.Bridge }

multi infix:«<»(Real \$a, Real \$b)   { $a.Bridge < $b.Bridge }

multi infix:«<=»(Real \$a, Real \$b)  { $a.Bridge <= $b.Bridge }

multi infix:«>»(Real \$a, Real \$b)   { $a.Bridge > $b.Bridge }

multi infix:«>=»(Real \$a, Real \$b)  { $a.Bridge >= $b.Bridge }

multi infix:<cmp>(Real \$a, Real \$b) { $a.Bridge cmp $b.Bridge }

