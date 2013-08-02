# for our tantrums
my class X::Numeric::DivideByZero { ... }

my role Numeric {
    multi method Numeric(Numeric:D:) { self }

    multi method ACCEPTS(Numeric:D: $a) {
        self.isNaN ?? $a.isNaN !! $a == self;
    }

    proto method log(|) {*}
    multi method log(Numeric:D: Cool $base) { self.log / $base.Numeric.log }
    multi method log(Numeric:D: Numeric $base) { self.log / $base.log         }

    method log10() { self.log / 10e0.log }

    proto method exp(|) {*}
    multi method exp(Numeric:D: $base) {
        $base ** self;
    }
    method roots(Cool $n) { self.Complex.roots($n.Int) }

    multi method Bool(Numeric:D:) { self != 0 }

    multi method gist(Numeric:D:) { self.Str }
    multi method DUMP(Numeric:D:) { self.perl }

    method succ() { self + 1 }
    method pred() { self - 1 }
}

multi sub infix:<eqv>(Numeric:D $a, Numeric:D $b) {
    $a.WHAT === $b.WHAT && ($a cmp $b) == 0
}

## arithmetic operators

proto prefix:<+>($?) is pure { * }
multi prefix:<+>(\a) { a.Numeric }

proto prefix:<->($?) is pure { * }
multi prefix:<->(\a) { -a.Numeric }

proto sub abs($) is pure { * }
multi sub abs(\a) { abs a.Numeric }

proto sub sign($) is pure {*}
multi sub sign(Numeric \x) { x.sign }
multi sub sign(Cool \x)    { x.Numeric.sign }

proto sub log($, $?) is pure {*}
multi sub log(Numeric $x) { $x.log }
multi sub log(Numeric $x, Numeric $base) { $x.log($base) }
multi sub log(Cool $x)    { $x.Numeric.log }
multi sub log(Cool $x, Cool $base) { $x.Numeric.log($base.Numeric) }

proto sub log10($, $?) is pure {*}
multi sub log10(Numeric $x) { $x.log(10e0) }
multi sub log10(Cool    $x) { $x.Numeric.log(10e0) }

proto sub exp($, $?) is pure {*}
multi sub exp(Numeric $x) { $x.exp }
multi sub exp(Numeric $x, Numeric $base) { $x.exp($base) }

proto sub sin($) is pure {*}
multi sub sin(Numeric \x) { x.sin }
multi sub sin(Cool \x)    { x.Numeric.sin }

proto sub asin($) is pure {*}
multi sub asin(Numeric \x) { x.asin }
multi sub asin(Cool \x)    { x.Numeric.asin }

proto sub cos($) is pure {*}
multi sub cos(Numeric \x) { x.cos }
multi sub cos(Cool \x)    { x.Numeric.cos }

proto sub acos($) is pure {*}
multi sub acos(Numeric \x) { x.acos }
multi sub acos(Cool \x)    { x.Numeric.acos }

proto sub tan($) is pure {*}
multi sub tan(Numeric \x) { x.tan }
multi sub tan(Cool \x)    { x.Numeric.tan }

proto sub atan($) is pure {*}
multi sub atan(Numeric \x) { x.atan }
multi sub atan(Cool \x)    { x.Numeric.atan }

proto sub sec($) is pure {*}
multi sub sec(Numeric \x)  { x.sec }
multi sub sec(Cool \x)     { x.Numeric.sec }

proto sub asec($) is pure {*}
multi sub asec(Numeric \x)  { x.asec }
multi sub asec(Cool \x)     { x.Numeric.asec }

proto sub cosec($) is pure {*}
multi sub cosec(Numeric \x)  { x.cosec }
multi sub cosec(Cool \x)     { x.Numeric.cosec }

proto sub acosec(|) is pure {*}
multi sub acosec(Numeric \x)  { x.acosec }
multi sub acosec(Cool \x)     { x.Numeric.acosec }

proto sub cotan($) is pure {*}
multi sub cotan(Numeric \x)  { x.cotan }
multi sub cotan(Cool \x)     { x.Numeric.cotan }

proto sub acotan($) is pure {*}
multi sub acotan(Numeric \x)  { x.acotan }
multi sub acotan(Cool \x)     { x.Numeric.acotan }

proto sub sinh($) is pure {*}
multi sub sinh(Numeric \x) { x.sinh }
multi sub sinh(Cool \x)    { x.Numeric.sinh }

proto sub asinh($) is pure {*}
multi sub asinh(Numeric \x) { x.asinh }
multi sub asinh(Cool \x)    { x.Numeric.asinh }

proto sub cosh($) is pure {*}
multi sub cosh(Numeric \x) { x.cosh }
multi sub cosh(Cool \x)    { x.Numeric.cosh }

proto sub acosh($) is pure {*}
multi sub acosh(Numeric \x) { x.acosh }
multi sub acosh(Cool \x)    { x.Numeric.acosh }

proto sub tanh($) is pure {*}
multi sub tanh(Numeric \x) { x.tanh }
multi sub tanh(Cool \x)    { x.Numeric.tanh }

proto sub atanh($) is pure {*}
multi sub atanh(Numeric \x) { x.atanh }
multi sub atanh(Cool \x)    { x.Numeric.atanh }

proto sub sech($) is pure {*}
multi sub sech(Numeric \x) { x.sech }
multi sub sech(Cool \x)    { x.Numeric.sech }

proto sub asech($) is pure {*}
multi sub asech(Numeric \x) { x.asech }
multi sub asech(Cool \x)    { x.Numeric.asech }

proto sub cosech($) is pure {*}
multi sub cosech(Numeric \x) { x.cosech }
multi sub cosech(Cool \x)    { x.Numeric.cosech }

proto sub acosech($) is pure {*}
multi sub acosech(Numeric \x) { x.acosech }
multi sub acosech(Cool \x)    { x.Numeric.acosech }

proto sub cotanh($) is pure {*}
multi sub cotanh(Numeric \x) { x.cotanh }
multi sub cotanh(Cool \x)    { x.Numeric.cotanh }

proto sub acotanh($) is pure {*}
multi sub acotanh(Numeric \x) { x.acotanh }
multi sub acotanh(Cool \x)    { x.Numeric.acotanh }

proto sub sqrt($) is pure {*}
multi sub sqrt(Numeric \x) { x.sqrt }
multi sub sqrt(Cool \x)    { x.Numeric.sqrt }

proto sub roots($, $) is pure { * }
multi sub roots($x, Cool $n) { $x.Numeric.Complex.roots($n.Int) }
multi sub roots($x, Numeric $n) { $x.Numeric.Complex.roots($n.Int) }

proto sub floor($) is pure   { * }
multi sub floor($a)          { $a.Numeric.floor }
multi sub floor(Numeric $a)  { $a.floor }

proto sub ceiling($) is pure   { * }
multi sub ceiling($a)          { $a.Numeric.ceiling }
multi sub ceiling(Numeric $a)  { $a.ceiling }

proto sub round($, $?) is pure      { * }
multi sub round($a)                 { $a.Numeric.round }
multi sub round(Numeric $a)         { $a.round }
multi sub round(Numeric $a, $scale) { $a.round($scale) }

proto infix:<+>($a?, $b?) is pure   { * }
multi infix:<+>($x = 0)      { $x.Numeric }
multi infix:<+>(\a, \b)    { a.Numeric + b.Numeric }

proto infix:<->($a?, $b?) is pure   { * }
multi infix:<->($x = 0)      { $x.Numeric }
multi infix:<->(\a, \b)    { a.Numeric - b.Numeric }

proto infix:<*>($a?, $b?) is pure   { * }
multi infix:<*>($x = 1)      { $x.Numeric }
multi infix:<*>(\a, \b)    { a.Numeric * b.Numeric }

proto infix:</>($a?, $b?) { * }
multi infix:</>()            { fail "No zero-arg meaning for infix:</>" }
multi infix:</>($x)          { $x.Numeric }
multi infix:</>(\a, \b)    { a.Numeric / b.Numeric }

proto infix:<div>($a?, $b?) is pure  { * }
# rest of infix:<div> is in Int.pm

proto infix:<%>($a?, $b?) is pure   { * }
multi infix:<%>()            { fail "No zero-arg meaning for infix:<%>" }
multi infix:<%>($x)          { $x }
multi infix:<%>(\a, \b)    { a.Real % b.Real }

proto infix:<%%>($a?, $b?) is pure  { * }
multi infix:<%%>()           { fail "No zero-arg meaning for infix:<%%>" }
multi infix:<%%>($x)         { Bool::True }
multi infix:<%%>(\a, \b)   {
    fail X::Numeric::DivideByZero.new(using => 'infix:<%%>') unless b;
    a.Real % b.Real == 0;
}

proto infix:<lcm>($a?, $b?) is pure  { * }
multi infix:<lcm>(Int $x = 1) { $x }
multi infix:<lcm>(\a, \b)   { a.Int lcm b.Int }

proto infix:<gcd>($a?, $b?) is pure { * }
multi infix:<gcd>()          { fail 'No zero-arg meaning for infix:<gcd>' }
multi infix:<gcd>(Int $x)    { $x }
multi infix:<gcd>(\a, \b)  { a.Int gcd b.Int }

proto infix:<**>($a?, $b?) is pure  { * }
multi infix:<**>($x = 1)     { $x.Numeric }
multi infix:<**>(\a, \b)   { a.Numeric ** b.Numeric }

## relational operators

proto infix:«<=>»($, $?) is pure       { * }
multi infix:«<=>»(\a, \b)  { a.Real <=> b.Real }

proto infix:<==>($a?, $b?) is pure  { * }
multi infix:<==>($x?)        { Bool::True }
multi infix:<==>(\a, \b)   { a.Numeric == b.Numeric }

proto infix:<!=>(Mu $a?, Mu $b?) is pure  { * }
multi infix:<!=>($x?)        { Bool::True }
multi infix:<!=>(Mu \a, Mu \b)   { not a == b }

proto infix:«<»($a?, $b?) is pure   { * }
multi infix:«<»($x?)         { Bool::True }
multi infix:«<»(\a, \b)    { a.Real < b.Real }

proto infix:«<=»($a?, $b?) is pure  { * }
multi infix:«<=»($x?)        { Bool::True }
multi infix:«<=»(\a, \b)   { a.Real <= b.Real }

proto infix:«>»($a?, $b?) is pure   { * }
multi infix:«>»($x?)         { Bool::True }
multi infix:«>»(\a, \b)    { a.Real > b.Real }

proto infix:«>=»($a?, $b?) is pure  { * }
multi infix:«>=»($x?)        { Bool::True }
multi infix:«>=»(\a, \b)   { a.Real >= b.Real }

## bitwise operators

proto infix:<+&>($?, $?) is pure { * }
multi infix:<+&>()           { +^0 }
multi infix:<+&>($x)         { $x }
multi infix:<+&>($x, $y)     { $x.Numeric.Int +& $y.Numeric.Int }

proto infix:<+|>($?, $?) is pure { * }
multi infix:<+|>()           { 0 }
multi infix:<+|>($x)         { $x }
multi infix:<+|>($x, $y)     { $x.Numeric.Int +| $y.Numeric.Int }

proto infix:<+^>($?, $?) is pure { * }
multi infix:<+^>()           { 0 }
multi infix:<+^>($x)         { $x }
multi infix:<+^>($x, $y)     { $x.Numeric.Int +^ $y.Numeric.Int }

proto infix:«+<»($?, $?) is pure { * }
multi infix:«+<»()           { fail "No zero-arg meaning for infix:«+<»"; }
multi infix:«+<»($x)         { $x }
multi infix:«+<»($x,$y)      { $x.Numeric.Int +< $y.Numeric.Int }

proto infix:«+>»($?, $?) is pure { * }
multi infix:«+>»()           { fail "No zero-arg meaning for infix:«+>»"; }
multi infix:«+>»($x)         { $x }
multi infix:«+>»($x,$y)      { $x.Numeric.Int +> $y.Numeric.Int }

proto prefix:<+^>($?, $?) is pure { * }
multi prefix:<+^>($x)        { +^ $x.Numeric.Int }

