my role Rational[::NuT = Int, ::DeT = ::("NuT")] does Real { ... }
my class X::Numeric::DivideByZero { ... }

my role Numeric {
    multi method Numeric(Numeric:D:) { self }
    multi method Numeric(Numeric:U:) {
        self.Mu::Numeric; # issue a warning
        # We need to be specific about coercions to make `Numeric() == 0` working as specced.
        (self.HOW.archetypes.coercive ?? self.^nominalize !! self).new
    }

    multi method ACCEPTS(Numeric:D: Any:D \a) {
        (try my \numeric = a.Numeric).defined
                ?? (self.isNaN && numeric.isNaN or numeric == self)
                !! False
    }

    proto method log(|) {*}
    multi method log(Numeric:D: Cool $base) { self.log / $base.Numeric.log }
    multi method log(Numeric:D: Numeric $base) { self.log / $base.log         }

    method log2()  { self.log /  2e0.log }
    method log10() { self.log / 10e0.log }

    proto method exp(|) {*}
    multi method exp(Numeric:D: $base) {
        $base ** self;
    }
    method roots(Cool $n) { self.Complex.roots($n.Int) }

    method FatRat(Numeric:D:) { self.Rat.FatRat }
    multi method Bool(Numeric:D:) { self != 0 }

    multi method gist(Numeric:D:) { self.Str }
    multi method DUMP(Numeric:D:) { self.raku }

    method succ() { self + 1 }
    method pred() { self - 1 }
}

multi sub infix:<eqv>(Numeric:D \a, Numeric:D \b --> Bool:D) {
    # Use === for Nums, to properly handle signed zeros and NaNs
    # For Rationals, properly handle NaN-y Rationals
    nqp::hllbool(
      nqp::eqaddr(nqp::decont(a),nqp::decont(b))
        || nqp::eqaddr(a.WHAT,b.WHAT)
        && nqp::if(
          nqp::istype(a,Num),
          a === b,
          nqp::if(
            nqp::istype(a,Rational)
              && nqp::isfalse(a.denominator) && nqp::isfalse(b.denominator)
              && nqp::isfalse(a.numerator)   && nqp::isfalse(b.numerator),
            True, # got two NaN Rationals
            a == b)))
}

## arithmetic operators

proto sub prefix:<+>($, *%) is pure {*}
multi sub prefix:<+>(\a) { a.Numeric }

proto sub prefix:<->($, *%) is pure {*}
multi sub prefix:<->(\a) { -a.Numeric }
# U+2212 MINUS SIGN
my constant &prefix:<−> := &prefix:<->;

proto sub abs($, *%) is pure {*}
multi sub abs(\a) { abs a.Numeric }

proto sub sign($, *%) is pure {*}
multi sub sign(Numeric \x) { x.sign }
multi sub sign(Cool \x)    { x.Numeric.sign }

proto sub log($, $?, *%) is pure {*}
multi sub log(Numeric $x) { $x.log }
multi sub log(Numeric $x, Numeric $base) { $x.log($base) }
multi sub log(Cool $x)    { $x.Numeric.log }
multi sub log(Cool $x, Cool $base) { $x.Numeric.log($base.Numeric) }

proto sub log2($, *%) is pure {*}
multi sub log2(Numeric $x) { $x.log(2e0) }
multi sub log2(Cool    $x) { $x.Numeric.log(2e0) }

proto sub log10($, *%) is pure {*}
multi sub log10(Numeric $x) { $x.log(10e0) }
multi sub log10(Cool    $x) { $x.Numeric.log(10e0) }

proto sub exp($, $?, *%) is pure {*}
multi sub exp(Numeric $x) { $x.exp }
multi sub exp(Numeric $x, Numeric $base) { $x.exp($base) }

proto sub sin($, *%) is pure {*}
multi sub sin(Numeric \x) { x.sin }
multi sub sin(Cool \x)    { x.Numeric.sin }

proto sub asin($, *%) is pure {*}
multi sub asin(Numeric \x) { x.asin }
multi sub asin(Cool \x)    { x.Numeric.asin }

proto sub cos($, *%) is pure {*}
multi sub cos(Numeric \x) { x.cos }
multi sub cos(Cool \x)    { x.Numeric.cos }

proto sub acos($, *%) is pure {*}
multi sub acos(Numeric \x) { x.acos }
multi sub acos(Cool \x)    { x.Numeric.acos }

proto sub tan($, *%) is pure {*}
multi sub tan(Numeric \x) { x.tan }
multi sub tan(Cool \x)    { x.Numeric.tan }

proto sub atan($, *%) is pure {*}
multi sub atan(Numeric \x) { x.atan }
multi sub atan(Cool \x)    { x.Numeric.atan }

proto sub sec($, *%) is pure {*}
multi sub sec(Numeric \x)  { x.sec }
multi sub sec(Cool \x)     { x.Numeric.sec }

proto sub asec($, *%) is pure {*}
multi sub asec(Numeric \x)  { x.asec }
multi sub asec(Cool \x)     { x.Numeric.asec }

proto sub cosec($, *%) is pure {*}
multi sub cosec(Numeric \x)  { x.cosec }
multi sub cosec(Cool \x)     { x.Numeric.cosec }

proto sub acosec($, *%) is pure {*}
multi sub acosec(Numeric \x)  { x.acosec }
multi sub acosec(Cool \x)     { x.Numeric.acosec }

proto sub cotan($, *%) is pure {*}
multi sub cotan(Numeric \x)  { x.cotan }
multi sub cotan(Cool \x)     { x.Numeric.cotan }

proto sub acotan($, *%) is pure {*}
multi sub acotan(Numeric \x)  { x.acotan }
multi sub acotan(Cool \x)     { x.Numeric.acotan }

proto sub sinh($, *%) is pure {*}
multi sub sinh(Numeric \x) { x.sinh }
multi sub sinh(Cool \x)    { x.Numeric.sinh }

proto sub asinh($, *%) is pure {*}
multi sub asinh(Numeric \x) { x.asinh }
multi sub asinh(Cool \x)    { x.Numeric.asinh }

proto sub cosh($, *%) is pure {*}
multi sub cosh(Numeric \x) { x.cosh }
multi sub cosh(Cool \x)    { x.Numeric.cosh }

proto sub acosh($, *%) is pure {*}
multi sub acosh(Numeric \x) { x.acosh }
multi sub acosh(Cool \x)    { x.Numeric.acosh }

proto sub tanh($, *%) is pure {*}
multi sub tanh(Numeric \x) { x.tanh }
multi sub tanh(Cool \x)    { x.Numeric.tanh }

proto sub atanh($, *%) is pure {*}
multi sub atanh(Numeric \x) { x.atanh }
multi sub atanh(Cool \x)    { x.Numeric.atanh }

proto sub sech($, *%) is pure {*}
multi sub sech(Numeric \x) { x.sech }
multi sub sech(Cool \x)    { x.Numeric.sech }

proto sub asech($, *%) is pure {*}
multi sub asech(Numeric \x) { x.asech }
multi sub asech(Cool \x)    { x.Numeric.asech }

proto sub cosech($, *%) is pure {*}
multi sub cosech(Numeric \x) { x.cosech }
multi sub cosech(Cool \x)    { x.Numeric.cosech }

proto sub acosech($, *%) is pure {*}
multi sub acosech(Numeric \x) { x.acosech }
multi sub acosech(Cool \x)    { x.Numeric.acosech }

proto sub cotanh($, *%) is pure {*}
multi sub cotanh(Numeric \x) { x.cotanh }
multi sub cotanh(Cool \x)    { x.Numeric.cotanh }

proto sub acotanh($, *%) is pure {*}
multi sub acotanh(Numeric \x) { x.acotanh }
multi sub acotanh(Cool \x)    { x.Numeric.acotanh }

proto sub sqrt($, *%) is pure {*}
multi sub sqrt(Numeric \x) { x.sqrt }
multi sub sqrt(Cool \x)    { x.Numeric.sqrt }

proto sub roots($, $, *%) is pure {*}
multi sub roots($x, Cool $n) { $x.Numeric.Complex.roots($n.Int) }
multi sub roots($x, Numeric $n) { $x.Numeric.Complex.roots($n.Int) }

proto sub floor($, *%) is pure   {*}
multi sub floor($a)          { $a.Numeric.floor }
multi sub floor(Numeric $a)  { $a.floor }

proto sub ceiling($, *%) is pure   {*}
multi sub ceiling($a)          { $a.Numeric.ceiling }
multi sub ceiling(Numeric $a)  { $a.ceiling }

proto sub round($, $?, *%) is pure      {*}
multi sub round(Numeric() $a)         { $a.round }
multi sub round(Numeric() $a, $scale) { $a.round($scale) }

proto sub infix:<+>($?, $?, *%) is pure   {*}
multi sub infix:<+>($x = 0)      { $x.Numeric }
multi sub infix:<+>(\a, \b)    { a.Numeric + b.Numeric }

proto sub infix:<->($?, $?, *%) is pure   {*}
multi sub infix:<->($x = 0)      { $x.Numeric }
multi sub infix:<->(\a, \b)    { a.Numeric - b.Numeric }
# U+2212 MINUS SIGN
my constant &infix:<−> := &infix:<->;

proto sub infix:<*>($?, $?, *%) is pure   {*}
multi sub infix:<*>($x = 1)      { $x.Numeric }
multi sub infix:<*>(\a, \b)    { a.Numeric * b.Numeric }
# U+00D7 MULTIPLICATION SIGN
my constant &infix:<×> = &infix:<*>;

proto sub infix:</>($?, $?, *%) is pure {*}
multi sub infix:</>() { Failure.new("No zero-arg meaning for infix:</>") }
multi sub infix:</>($x)          { $x.Numeric }
multi sub infix:</>(\a, \b)    { a.Numeric / b.Numeric }
# U+00F7 DIVISION SIGN
my constant &infix:<÷> = &infix:</>;

proto sub infix:<div>($, $, *%) is pure  {*}
# rest of infix:<div> is in Int.pm6

proto sub infix:<%>($?, $?, *%) is pure   {*}
multi sub infix:<%>() { Failure.new("No zero-arg meaning for infix:<%>") }
multi sub infix:<%>($x)          { $x }
multi sub infix:<%>(\a, \b)    { a.Real % b.Real }

proto sub infix:<%%>($?, $?, *%) is pure  {*}
multi sub infix:<%%>() { Failure.new("No zero-arg meaning for infix:<%%>") }
multi sub infix:<%%>($)         { Bool::True }
multi sub infix:<%%>(\a, \b) {
    b
      ?? (a.Real % b.Real == 0)
      !! Failure.new(
           X::Numeric::DivideByZero.new(using => 'infix:<%%>', numerator => a)
         )
}

proto sub infix:<lcm>($?, $?, *%) is pure  {*}
multi sub infix:<lcm>(\a, \b)   { a.Int lcm b.Int }

proto sub infix:<gcd>($?, $?, *%) is pure {*}
multi sub infix:<gcd>() { Failure.new('No zero-arg meaning for infix:<gcd>') }
multi sub infix:<gcd>(\a, \b)  { a.Int gcd b.Int }

proto sub infix:<**>($?, $?, *%) is pure  {*}
multi sub infix:<**>($x = 1)     { $x.Numeric }
multi sub infix:<**>(\a, \b)   { a.Numeric ** b.Numeric }

proto sub postfix:<ⁿ>($, $, *%) is pure  {*}
multi sub postfix:<ⁿ>(\a, \b)  { a ** b }

## relational operators

proto sub infix:<==>($?, $?, *%) is pure {*}
multi sub infix:<==>($?)        { Bool::True }
multi sub infix:<==>(\a, \b)   { a.Numeric == b.Numeric }

proto sub infix:<=~=>($?, $?, *%) {*}  # note, can't be pure due to dynvar
multi sub infix:<=~=>($?) { Bool::True }
multi sub infix:<=~=>(\a, \b, :$tolerance = $*TOLERANCE)    {
    # If operands are non-0, scale the tolerance to the larger of the abs values.
    # We test b first since $value ≅ 0 is the usual idiom and falsifies faster.
    if b && a && $tolerance {
        abs(a - b) < (a.abs max b.abs) * $tolerance;
    }
    else {  # interpret tolerance as absolute
        abs(a.Num - b.Num) < $tolerance;
    }
}
# U+2245 APPROXIMATELY EQUAL TO
my constant &infix:<≅> = &infix:<=~=>;

proto sub infix:<!=>(Mu $?, Mu $?, *%) is pure  {*}
multi sub infix:<!=>($?)                    { Bool::True }
multi sub infix:<!=>(Mu \a, Mu \b)          { not a == b }
# U+2260 NOT EQUAL TO
my constant &infix:<≠> := &infix:<!=>;

proto sub infix:«<»($?, $?, *%) is pure   {*}
multi sub infix:«<»($?)         { Bool::True }
multi sub infix:«<»(\a, \b)    { a.Real < b.Real }

proto sub infix:«<=»($?, $?, *%) is pure {*}
multi sub infix:«<=»($?)                   { Bool::True }
multi sub infix:«<=»(\a, \b)               { a.Real <= b.Real }
# U+2264 LESS-THAN OR EQUAL TO
my constant &infix:<≤> := &infix:«<=»;

proto sub infix:«>»($?, $?, *%) is pure   {*}
multi sub infix:«>»($?)         { Bool::True }
multi sub infix:«>»(\a, \b)    { a.Real > b.Real }

proto sub infix:«>=»($?, $?, *%) is pure  {*}
multi sub infix:«>=»($?)                    { Bool::True }
multi sub infix:«>=»(\a, \b)                { a.Real >= b.Real }
# U+2265 GREATER-THAN OR EQUAL TO
my constant &infix:<≥> := &infix:«>=»;

## bitwise operators

proto sub infix:<+&>($?, $?, *%) is pure {*}
multi sub infix:<+&>()           { +^0 }
multi sub infix:<+&>($x)         { $x }
multi sub infix:<+&>($x, $y)     { $x.Numeric.Int +& $y.Numeric.Int }

proto sub infix:<+|>($?, $?, *%) is pure {*}
multi sub infix:<+|>()           { 0 }
multi sub infix:<+|>($x)         { $x }
multi sub infix:<+|>($x, $y)     { $x.Numeric.Int +| $y.Numeric.Int }

proto sub infix:<+^>($?, $?, *%) is pure {*}
multi sub infix:<+^>()           { 0 }
multi sub infix:<+^>($x)         { $x }
multi sub infix:<+^>($x, $y)     { $x.Numeric.Int +^ $y.Numeric.Int }

proto sub infix:«+<»($?, $?, *%) is pure {*}
multi sub infix:«+<»() { Failure.new("No zero-arg meaning for infix:«+<»") }
multi sub infix:«+<»($x)         { $x }
multi sub infix:«+<»($x,$y)      { $x.Numeric.Int +< $y.Numeric.Int }

proto sub infix:«+>»($?, $?, *%) is pure {*}
multi sub infix:«+>»() { Failure.new("No zero-arg meaning for infix:«+>»") }
multi sub infix:«+>»($x)         { $x }
multi sub infix:«+>»($x,$y)      { $x.Numeric.Int +> $y.Numeric.Int }

proto sub prefix:<+^>($, *%) is pure {*}
multi sub prefix:<+^>($x)        { +^ $x.Numeric.Int }

# vim: expandtab shiftwidth=4
