my role Numeric {
    multi method Numeric(Numeric:D:) { self }

    multi method ACCEPTS(Numeric:D: $a) {
        self.isNaN ?? $a.isNaN !! $a == self;
    }

    proto method log(|$) {*}
    multi method log(Cool    $base) { self.log / $base.Numeric.log }
    multi method log(Numeric $base) { self.log / $base.log         }

    method log10() { self.log / 10e0.log }

    proto method exp(|$) {*}
    multi method exp($base) {
        $base ** self;
    }
    method roots(Cool $n) { self.Complex.roots($n.Int) }
    multi method Bool(Numeric:D:) { self != 0 }

    multi method gist(Numeric:D:) { self.Str }
}

multi sub infix:<eqv>(Numeric $a, Numeric $b) {
    $a.WHAT === $b.WHAT && ($a cmp $b) == 0
}

## arithmetic operators

proto prefix:<+>(|$) { * }
multi prefix:<+>(\$a) { $a.Numeric }

proto prefix:<->(|$) { * }
multi prefix:<->(\$a) { -$a.Numeric }

proto prefix:<abs>(|$) { * }
multi prefix:<abs>(\$a) { abs $a.Numeric }

proto sub sign(|$) {*}
multi sub sign(Numeric \$x) { $x.sign }
multi sub sign(Cool \$x)    { $x.Numeric.sign }

proto sub log(|$) {*}
multi sub log(Numeric $x) { $x.log }
multi sub log(Numeric $x, Numeric $base) { $x.log($base) }
multi sub log(Cool $x)    { $x.Numeric.log }
multi sub log(Cool $x, Cool $base) { $x.Numeric.log($base.Numeric) }

proto sub log10(|$) {*}
multi sub log10(Numeric $x) { $x.log(10e0) }
multi sub log10(Cool    $x) { $x.Numeric.log(10e0) }

proto sub exp(|$) {*}
multi sub exp(Numeric $x) { $x.exp }
multi sub exp(Numeric $x, Numeric $base) { $x.exp($base) }

proto sub sin(|$) {*}
multi sub sin(Numeric \$x) { $x.sin }
multi sub sin(Cool \$x)    { $x.Numeric.sin }

proto sub asin(|$) {*}
multi sub asin(Numeric \$x) { $x.asin }
multi sub asin(Cool \$x)    { $x.Numeric.asin }

proto sub cos(|$) {*}
multi sub cos(Numeric \$x) { $x.cos }
multi sub cos(Cool \$x)    { $x.Numeric.cos }

proto sub acos(|$) {*}
multi sub acos(Numeric \$x) { $x.acos }
multi sub acos(Cool \$x)    { $x.Numeric.acos }

proto sub tan(|$) {*}
multi sub tan(Numeric \$x) { $x.tan }
multi sub tan(Cool \$x)    { $x.Numeric.tan }

proto sub atan(|$) {*}
multi sub atan(Numeric \$x) { $x.atan }
multi sub atan(Cool \$x)    { $x.Numeric.atan }

proto sub sec(|$) {*}
multi sub sec(Numeric \$x)  { $x.sec }
multi sub sec(Cool \$x)     { $x.Numeric.sec }

proto sub asec(|$) {*}
multi sub asec(Numeric \$x)  { $x.asec }
multi sub asec(Cool \$x)     { $x.Numeric.asec }

proto sub cosec(|$) {*}
multi sub cosec(Numeric \$x)  { $x.cosec }
multi sub cosec(Cool \$x)     { $x.Numeric.cosec }

proto sub acosec(|$) {*}
multi sub acosec(Numeric \$x)  { $x.acosec }
multi sub acosec(Cool \$x)     { $x.Numeric.acosec }

proto sub cotan(|$) {*}
multi sub cotan(Numeric \$x)  { $x.cotan }
multi sub cotan(Cool \$x)     { $x.Numeric.cotan }

proto sub acotan(|$) {*}
multi sub acotan(Numeric \$x)  { $x.acotan }
multi sub acotan(Cool \$x)     { $x.Numeric.acotan }

proto sub sinh(|$) {*}
multi sub sinh(Numeric \$x) { $x.sinh }
multi sub sinh(Cool \$x)    { $x.Numeric.sinh }

proto sub asinh(|$) {*}
multi sub asinh(Numeric \$x) { $x.asinh }
multi sub asinh(Cool \$x)    { $x.Numeric.asinh }

proto sub cosh(|$) {*}
multi sub cosh(Numeric \$x) { $x.cosh }
multi sub cosh(Cool \$x)    { $x.Numeric.cosh }

proto sub acosh(|$) {*}
multi sub acosh(Numeric \$x) { $x.acosh }
multi sub acosh(Cool \$x)    { $x.Numeric.acosh }

proto sub tanh(|$) {*}
multi sub tanh(Numeric \$x) { $x.tanh }
multi sub tanh(Cool \$x)    { $x.Numeric.tanh }

proto sub atanh(|$) {*}
multi sub atanh(Numeric \$x) { $x.atanh }
multi sub atanh(Cool \$x)    { $x.Numeric.atanh }

proto sub sech(|$) {*}
multi sub sech(Numeric \$x) { $x.sech }
multi sub sech(Cool \$x)    { $x.Numeric.sech }

proto sub asech(|$) {*}
multi sub asech(Numeric \$x) { $x.asech }
multi sub asech(Cool \$x)    { $x.Numeric.asech }

proto sub cosech(|$) {*}
multi sub cosech(Numeric \$x) { $x.cosech }
multi sub cosech(Cool \$x)    { $x.Numeric.cosech }

proto sub acosech(|$) {*}
multi sub acosech(Numeric \$x) { $x.acosech }
multi sub acosech(Cool \$x)    { $x.Numeric.acosech }

proto sub cotanh(|$) {*}
multi sub cotanh(Numeric \$x) { $x.cotanh }
multi sub cotanh(Cool \$x)    { $x.Numeric.cotanh }

proto sub acotanh(|$) {*}
multi sub acotanh(Numeric \$x) { $x.acotanh }
multi sub acotanh(Cool \$x)    { $x.Numeric.acotanh }

proto sub sqrt(|$) {*}
multi sub sqrt(Numeric \$x) { $x.sqrt }
multi sub sqrt(Cool \$x)    { $x.Numeric.sqrt }

proto sub roots($, $)        { * }
multi sub roots($x, Cool $n) { $x.Numeric.Complex.roots($n.Int) }
multi sub roots($x, Numeric $n) { $x.Numeric.Complex.roots($n.Int) }

proto sub floor(|$)          { * }
multi sub floor($a)          { $a.Numeric.floor }
multi sub floor(Numeric $a)  { $a.floor }

proto sub ceiling(|$)          { * }
multi sub ceiling($a)          { $a.Numeric.ceiling }
multi sub ceiling(Numeric $a)  { $a.ceiling }

proto sub round(|$)          { * }
multi sub round($a)          { $a.Numeric.round }
multi sub round(Numeric $a)  { $a.round }

proto infix:<+>($a?, $b?)    { * }
multi infix:<+>($x = 0)      { $x.Numeric }
multi infix:<+>(\$a, \$b)    { $a.Numeric + $b.Numeric }

proto infix:<->($a?, $b?)    { * }
multi infix:<->($x = 0)      { $x.Numeric }
multi infix:<->(\$a, \$b)    { $a.Numeric - $b.Numeric }

proto infix:<*>($a?, $b?)    { * }
multi infix:<*>($x = 1)      { $x.Numeric }
multi infix:<*>(\$a, \$b)    { $a.Numeric * $b.Numeric }

proto infix:</>($a?, $b?)    { * }
multi infix:</>()            { fail "No zero-arg meaning for infix:</>" }
multi infix:</>($x)          { $x.Numeric }
multi infix:</>(\$a, \$b)    { $a.Numeric / $b.Numeric }

proto infix:<div>($a?, $b?)  { * }
# rest of infix:<div> is in Int.pm

proto infix:<%>($a?, $b?)    { * }
multi infix:<%>()            { fail "No zero-arg meaning for infix:<%>" }
multi infix:<%>($x)          { $x }
multi infix:<%>(\$a, \$b)    { $a.Real % $b.Real }

proto infix:<%%>($a?, $b?)   { * }
multi infix:<%%>()           { fail "No zero-arg meaning for infix:<%%>" }
multi infix:<%%>($x)         { Bool::True }
multi infix:<%%>(\$a, \$b)   { $a.Real % $b.Real == 0 }

proto infix:<lcm>($a?, $b?)   { * }
multi infix:<lcm>(Int $x = 1) { $x }
multi infix:<lcm>(\$a, \$b)   { $a.Int lcm $b.Int }

proto infix:<gcd>($a?, $b?)  { * }
multi infix:<gcd>()          { fail 'No zero-arg meaning for infix:<gcd>' }
multi infix:<gcd>(Int $x)    { $x }
multi infix:<gcd>(\$a, \$b)  { $a.Int gcd $b.Int }

proto infix:<**>($a?, $b?)   { * }
multi infix:<**>($x = 1)     { $x.Numeric }
multi infix:<**>(\$a, \$b)   { $a.Numeric ** $b.Numeric }

## relational operators

proto infix:«<=>»(|$)        { * }
multi infix:«<=>»(\$a, \$b)  { $a.Real <=> $b.Real }

proto infix:<==>($a?, $b?)   { * }
multi infix:<==>($x?)        { Bool::True }
multi infix:<==>(\$a, \$b)   { $a.Numeric == $b.Numeric }

proto infix:<!=>($a?, $b?)   { * }
multi infix:<!=>($x?)        { Bool::True }
multi infix:<!=>(Mu \$a, Mu \$b)   { not $a == $b }

proto infix:«<»($a?, $b?)    { * }
multi infix:«<»($x?)         { Bool::True }
multi infix:«<»(\$a, \$b)    { $a.Real < $b.Real }

proto infix:«<=»($a?, $b?)   { * }
multi infix:«<=»($x?)        { Bool::True }
multi infix:«<=»(\$a, \$b)   { $a.Real <= $b.Real }

proto infix:«>»($a?, $b?)    { * }
multi infix:«>»($x?)         { Bool::True }
multi infix:«>»(\$a, \$b)    { $a.Real > $b.Real }

proto infix:«>=»($a?, $b?)   { * }
multi infix:«>=»($x?)        { Bool::True }
multi infix:«>=»(\$a, \$b)   { $a.Real >= $b.Real }

## bitwise operators

proto infix:<+&>(|$)         { * }
multi infix:<+&>()           { +^0 }
multi infix:<+&>($x)         { $x }
multi infix:<+&>($x, $y)     { $x.Numeric.Int +& $y.Numeric.Int }

proto infix:<+|>(|$)         { * }
multi infix:<+|>()           { 0 }
multi infix:<+|>($x)         { $x }
multi infix:<+|>($x, $y)     { $x.Numeric.Int +| $y.Numeric.Int }

proto infix:<+^>(|$)         { * }
multi infix:<+^>()           { 0 }
multi infix:<+^>($x)         { $x }
multi infix:<+^>($x, $y)     { $x.Numeric.Int +^ $y.Numeric.Int }

proto infix:«+<»(|$)         { * }
multi infix:«+<»()           { fail "No zero-arg meaning for infix:«+<»"; }
multi infix:«+<»($x)         { $x }
multi infix:«+<»($x,$y)      { $x.Numeric.Int +< $y.Numeric.Int }

proto infix:«+>»(|$)         { * }
multi infix:«+>»()           { fail "No zero-arg meaning for infix:«+>»"; }
multi infix:«+>»($x)         { $x }
multi infix:«+>»($x,$y)      { $x.Numeric.Int +> $y.Numeric.Int }

proto prefix:<+^>(|$)        { * }
multi prefix:<+^>($x)        { +^ $x.Numeric.Int }

