my class Complex { ... }

my role Real does Numeric {
    method Rat(Real:D: Real $epsilon = 1.0e-6) { self.Num.Rat($epsilon) }
    method abs()  { self < 0 ?? -self !! self }
    method sign(Real:D:) { self > 0 ?? 1 !! self < 0 ?? -1 !! +self }
    method conj(Real:D:) { self }
    method sqrt() { self.Num.sqrt }
    method rand() { self.Num.rand }
    method sin()  { self.Num.sin }
    method asin() { self.Num.asin }
    method cos()  { self.Num.cos }
    method acos() { self.Num.acos }
    method tan()  { self.Num.tan }
    method atan() { self.Num.atan }
    proto method atan2(|) {*}
    multi method atan2(Real $x = 1e0) { self.Num.atan2($x.Num) }
    multi method atan2(Cool $x = 1e0) { self.Num.atan2($x.Numeric.Num) }
    method sec() { self.Num.sec }
    method asec() { self.Num.asec }
    method cosec() { self.Num.cosec }
    method acosec() { self.Num.acosec }
    method cotan()  { self.Num.cotan }
    method acotan() { self.Num.acotan }
    method sinh() { self.Num.sinh }
    method asinh() { self.Num.asinh }
    method cosh() { self.Num.cosh }
    method acosh() { self.Num.acosh }
    method tanh() { self.Num.tanh }
    method atanh() { self.Num.atanh }
    method sech() { self.Num.sech }
    method asech() { self.Num.asech }
    method cosech() { self.Num.cosech }
    method acosech() { self.Num.acosech }
    method cotanh() { self.Num.cotanh }
    method acotanh() { self.Num.acotanh }
    method floor() { self.Num.floor }
    method ceiling() { self.Num.ceiling }

    proto method round(|) {*}
    multi method round(Real:D:) {
        (self + 1/2).floor; # Rat NYI here, so no .5
    }
    multi method round(Real:D: Real() $scale) {
        (self / $scale + 1/2).floor * $scale;
    }

    method unpolar(Real $angle) {
        Complex.new(self * $angle.cos, self * $angle.sin);
    }
    method cis() {
        Complex.new(self.cos, self.sin);
    }
    method Complex() { Complex.new(self.Num, 0e0) }
    proto method log(|) {*}
    multi method log(Real:D: )           { self.Num.log               }
    multi method log(Real:D: Real $base) { self.Num.log($base.Num) }
    proto method exp(|) {*}
    multi method exp(Real:D: )           { self.Num.exp               }
    method truncate(Real:D:) {
        self == 0 ?? 0 !! self < 0  ?? self.ceiling !! self.floor
    }
    method isNaN { Bool::False }

    method polymod(Real:D: +@mods) {
        my $more = self;
        my $lazy = @mods.is-lazy;
        fail X::OutOfRange.new(
          :what('invocant to polymod'), :got($more), :range<0..Inf>
        ) if $more < 0;
        gather {
            for @mods -> $mod {
                last if $lazy and not $more;
                Failure.new(X::Numeric::DivideByZero.new:
                  using => 'polymod', numerator => $more
                ) unless $mod;
                take my $rem = $more % $mod;
                $more -= $rem;
                $more /= $mod;
            }
            take $more if ($lazy and $more) or not $lazy;
        }
    }

    method base(Int:D $base, $digits? is copy) {
        $digits = Nil if nqp::istype($digits, Whatever);
        fail X::OutOfRange.new(
                :what('digits argument to base'), :got($digits),
                :range<0..1073741824>
            ) if $digits.defined and $digits < 0;
        my $prec = $digits // 1e8.log($base.Num).Int;
        my Int $int_part = self.Int.self; # .self blows up Failures
        my $frac = abs(self - $int_part);
        my @frac_digits;
        my @conversion := <0 1 2 3 4 5 6 7 8 9
                           A B C D E F G H I J
                           K L M N O P Q R S T
                           U V W X Y Z>;
        for ^$prec {
            last unless $digits // $frac;
            $frac = $frac * $base;
            push @frac_digits, $frac.Int;
            $frac = $frac - $frac.Int;
        }
        if 2 * $frac >= 1 {
            if @frac_digits {
                for @frac_digits-1 ... 0 -> $x {
                    last if ++@frac_digits[$x] < $base;
                    @frac_digits[$x] = 0;
                    ++$int_part if $x == 0
                }
            }
            else {
                ++$int_part;
            }
        }
        my Str $r = $int_part.base($base);
        $r ~= '.' ~ @conversion[@frac_digits].join if @frac_digits;
        # if $int_part is 0, $int_part.base doesn't see the sign of self
        $int_part == 0 && self < 0 ?? '-' ~ $r !! $r;
    }

    method Real(Real:D:) { self }
    method Int(Real:D:) { self.Int }
    method Num(Real:D:) { self.Num }
    multi method Str(Real:D:) { self.Str }
}

proto sub cis($) {*}
multi sub cis(Real $a) { $a.cis }

multi sub infix:<+>(Real \a, Real \b)   { a.Num + b.Num }

multi sub infix:<->(Real \a, Real \b)   { a.Num - b.Num }

multi sub infix:<*>(Real \a, Real \b)   { a.Num * b.Num }

multi sub infix:</>(Real \a, Real \b)   { a.Num / b.Num }

multi sub infix:<%>(Real \a, Real \b)   { a.Num % b.Num }

multi sub infix:<**>(Real \a, Real \b)  { a.Num ** b.Num }

multi sub infix:«<=>»(Real \a, Real \b) { a.Num <=> b.Num }

multi sub infix:<==>(Real \a, Real \b)  { a.Num == b.Num }

multi sub infix:«<»(Real \a, Real \b)   { a.Num < b.Num }

multi sub infix:«<=»(Real \a, Real \b)  { a.Num <= b.Num }
multi sub infix:«≤» (Real \a, Real \b)  { a.Num  ≤ b.Num }

multi sub infix:«>»(Real \a, Real \b)   { a.Num > b.Num }

multi sub infix:«>=»(Real \a, Real \b)  { a.Num >= b.Num }
multi sub infix:«≥» (Real \a, Real \b)  { a.Num  ≥ b.Num }

multi sub prefix:<->(Real:D \a)            { -a.Num }

# NOTE: According to the spec, infix:<mod> is "Not coercive,
# so fails on differing types."  Thus no casts here.
proto sub infix:<mod>($, $) is pure {*}
multi sub infix:<mod>(Real $a, Real $b) {
    $a - ($a div $b) * $b;
}

multi sub abs(Real \a) {
    a < 0 ?? -a !! a;
}

proto sub truncate($) {*}
multi sub truncate(Real:D $x) { $x.truncate }
multi sub truncate(Cool:D $x) { $x.Numeric.truncate }


proto sub atan2($, $?)    {*}
multi sub atan2(Real \a, Real \b = 1e0) { a.Num.atan2(b.Num) }
# should really be (Cool, Cool), and then (Cool, Real) and (Real, Cool)
# candidates, but since Int both conforms to Cool and Real, we'd get lots
# of ambiguous dispatches. So just go with (Any, Any) for now.
multi sub atan2(     \a,      \b = 1e0) { a.Numeric.atan2(b.Numeric) }

proto sub unpolar($, $) {*}
multi sub unpolar(Real $mag, Real $angle) { $mag.unpolar($angle) }

# vim: ft=perl6 expandtab sw=4
