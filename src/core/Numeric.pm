# XXX role Numeric { ... }
my class Numeric {
    method Numeric() { self }

    multi method ACCEPTS(Numeric:D: $a) { $a == self }

    proto method log(|$) {*}
    multi method log(Cool    $base) { self.log / $base.Numeric.log }
    multi method log(Numeric $base) { self.log / $base.log         }

    method log10() { self.log / 10e0.log }

    proto method exp(|$) {*}
    multi method exp(Numeric $base) {
        self.exp * $base.log;
    }
}


## arithmetic operators

proto prefix:<+>(|$) { * }
multi prefix:<+>(\$a) { $a.Numeric }
multi prefix:<+>(Numeric \$a) { $a }

proto prefix:<->(|$) { * }

proto prefix:<abs>(|$) { * }
multi prefix:<abs>(\$a)      { abs $a.Numeric }

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

proto sub sin(|$) {*}
multi sub sin(Numeric \$x) { $x.sin }
multi sub sin(Cool \$x)    { $x.Numeric.sin }

proto sub cos(|$) {*}
multi sub cos(Numeric \$x) { $x.cos }
multi sub cos(Cool \$x)    { $x.Numeric.cos }

proto sub tan(|$) {*}
multi sub tan(Numeric \$x) { $x.tan }
multi sub tan(Cool \$x)    { $x.Numeric.tan }

proto sub sqrt(|$) {*}
multi sub sqrt(Numeric \$x) { $x.sqrt }
multi sub sqrt(Cool \$x)    { $x.Numeric.sqrt }

proto infix:<+>(|$)          { * }
multi infix:<+>($x = 0)      { $x.Numeric }
multi infix:<+>(\$a, \$b)    { $a.Numeric + $b.Numeric }

proto infix:<->(|$)          { * }
multi infix:<->($x = 0)      { $x.Numeric }
multi infix:<->(\$a, \$b)    { $a.Numeric - $b.Numeric }

proto infix:<*>(|$)          { * }
multi infix:<*>($x = 1)      { $x.Numeric }
multi infix:<*>(\$a, \$b)    { $a.Numeric * $b.Numeric }

proto infix:</>(|$)          { * }
multi infix:</>()            { fail "No zero-arg meaning for infix:</>" }
multi infix:</>($x)          { $x.Numeric }
multi infix:</>(\$a, \$b)    { $a.Numeric / $b.Numeric }

proto infix:<div>(|$)        { * }
# rest of infix:<div> is in Int.pm

proto infix:<%>(|$)          { * }
multi infix:<%>()            { fail "No zero-arg meaning for infix:<%>" }
multi infix:<%>($x)          { $x }
multi infix:<%>(\$a, \$b)    { $a.Numeric % $b.Numeric }

proto infix:<lcm>(|$)         { * }
multi infix:<lcm>(Int $x = 1) { $x }
multi infix:<lcm>(\$a, \$b)   { $a.Numeric lcm $b.Numeric }

proto infix:<gcd>(|$)        { * }
multi infix:<gcd>()          { fail 'No zero-arg meaning for infix:<gcd>' }
multi infix:<gcd>(Int $x)    { $x }
multi infix:<gcd>(\$a, \$b)  { $a.Numeric gcd $b.Numeric }

proto infix:<**>(|$)         { * }
multi infix:<**>($x = 1)     { $x.Numeric }
multi infix:<**>(\$a, \$b)   { $a.Numeric ** $b.Numeric }

## relational operators

proto infix:«<=>»(|$)        { * }
multi infix:«<=>»(\$a, \$b)  { $a.Numeric <=> $b.Numeric }

proto infix:<==>(|$)         { * }
multi infix:<==>($x?)        { Bool::True }
multi infix:<==>(\$a, \$b)   { $a.Numeric == $b.Numeric }

proto infix:<!=>(|$)         { * }
multi infix:<==>($x?)        { Bool::True }
multi infix:<!=>(\$a, \$b)   { $a.Numeric != $b.Numeric }

proto infix:«<»(|$)          { * }
multi infix:«<»($x?)         { Bool::True }
multi infix:«<»(\$a, \$b)    { $a.Numeric < $b.Numeric }

proto infix:«<=»(|$)         { * }
multi infix:«<=»($x?)        { Bool::True }
multi infix:«<=»(\$a, \$b)   { $a.Numeric <= $b.Numeric }

proto infix:«>»(|$)          { * }
multi infix:«>»($x?)         { Bool::True }
multi infix:«>»(\$a, \$b)    { $a.Numeric > $b.Numeric }

proto infix:«>=»(|$)         { * }
multi infix:«>=»($x?)        { Bool::True }
multi infix:«>=»(\$a, \$b)   { $a.Numeric >= $b.Numeric }

## bitwise operators

proto infix:<+&>(|$) { * }
multi infix:<+&>()   { +^0 }
multi infix:<+&>($x) { $x }

proto infix:<+|>(|$) { * }
multi infix:<+|>()   { 0 }
multi infix:<+|>($x) { $x }

proto infix:<+^>(|$) { * }
multi infix:<+^>()   { 0 }
multi infix:<+^>($x) { $x }

proto infix:«+<»(|$) { * }
multi infix:«+<»()   { fail "No zero-arg meaning for infix:«+<»"; }
multi infix:«+<»($x) { $x }

proto infix:«+>»(|$) { * }
multi infix:«+>»()   { fail "No zero-arg meaning for infix:«+>»"; }
multi infix:«+>»($x) { $x }

proto prefix:<+^>(|$) { * }

