class Complex { ... }

# XXX role Numeric { ... }
my class Numeric {
    multi method Numeric(Numeric:D:) { self }

    multi method ACCEPTS(Numeric:D: $a) { $a == self }

    proto method log(|$) {*}
    multi method log(Cool    $base) { self.log / $base.Numeric.log }
    multi method log(Numeric $base) { self.log / $base.log         }

    method log10() { self.log / 10e0.log }

    proto method exp(|$) {*}
    multi method exp(Numeric $base) {
        $base ** self;
    }
    method roots(Cool $n) { self.Complex.roots($n.Int) }
    multi method Bool(Numeric:D:) { self != 0 }

    multi method gist(Numeric:D:) { self.Str }
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
multi infix:<%>(\$a, \$b)    { $a.Numeric % $b.Numeric }

proto infix:<%%>($a?, $b?)   { * }
multi infix:<%%>()           { fail "No zero-arg meaning for infix:<%%>" }
multi infix:<%%>($x)         { Bool::True }
multi infix:<%%>(\$a, \$b)   { $a.Numeric % $b.Numeric == 0 }

proto infix:<lcm>($a?, $b?)   { * }
multi infix:<lcm>(Int $x = 1) { $x }
multi infix:<lcm>(\$a, \$b)   { $a.Numeric lcm $b.Numeric }

proto infix:<gcd>($a?, $b?)  { * }
multi infix:<gcd>()          { fail 'No zero-arg meaning for infix:<gcd>' }
multi infix:<gcd>(Int $x)    { $x }
multi infix:<gcd>(\$a, \$b)  { $a.Numeric gcd $b.Numeric }

proto infix:<**>($a?, $b?)   { * }
multi infix:<**>($x = 1)     { $x.Numeric }
multi infix:<**>(\$a, \$b)   { $a.Numeric ** $b.Numeric }

## relational operators

proto infix:«<=>»(|$)        { * }
multi infix:«<=>»(\$a, \$b)  { $a.Numeric <=> $b.Numeric }

proto infix:<==>($a?, $b?)   { * }
multi infix:<==>($x?)        { Bool::True }
multi infix:<==>(\$a, \$b)   { $a.Numeric == $b.Numeric }

proto infix:<!=>($a?, $b?)   { * }
multi infix:<!=>($x?)        { Bool::True }
multi infix:<!=>(\$a, \$b)   { $a.Numeric != $b.Numeric }

proto infix:«<»($a?, $b?)    { * }
multi infix:«<»($x?)         { Bool::True }
multi infix:«<»(\$a, \$b)    { $a.Numeric < $b.Numeric }

proto infix:«<=»($a?, $b?)   { * }
multi infix:«<=»($x?)        { Bool::True }
multi infix:«<=»(\$a, \$b)   { $a.Numeric <= $b.Numeric }

proto infix:«>»($a?, $b?)    { * }
multi infix:«>»($x?)         { Bool::True }
multi infix:«>»(\$a, \$b)    { $a.Numeric > $b.Numeric }

proto infix:«>=»($a?, $b?)   { * }
multi infix:«>=»($x?)        { Bool::True }
multi infix:«>=»(\$a, \$b)   { $a.Numeric >= $b.Numeric }

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

