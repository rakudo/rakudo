augment class Any {
    multi method abs() {
        pir::abs__Nn(self.Num);
    }

    multi method exp() {
        self.Num.exp;
    }

    our Int multi method ceiling() is export {
        pir::box__PI(pir::ceil__IN(self))
    }

    our Str multi method chr() {
        ~(pir::chr__SI(self))
    }

    multi method cis() {
        1.unpolar(self)
    }

    our Int multi method floor() is export {
        pir::box__PI(pir::floor__IN(self))
    }

    our Int multi method truncate() is export {
        self == 0 ?? 0 !! self < 0  ?? self.ceiling !! self.floor
    }

    our Num method rand() {
        pir::box__PN(pir::rand__NN(self))
    }

    multi method roots($n) {
        $.Complex.roots($n);
    }

    our Int multi method round() is export {
        (self + 0.5).Num.floor;
    }

    multi method sqrt() {
        self.Num.sqrt;
    }

    INIT {
        our @trig-base-conversions = (1.0, pi / 180.0, pi / 200.0, 2.0 * pi);
    }

    # Used by the :Trig subs and methods in the Int and Num classes.
    our multi method !to-radians($base) {
        self * pir::get_global__Ps('@trig-base-conversions')[$base];
    }

    our multi method !from-radians($base) {
        self / pir::get_global__Ps('@trig-base-conversions')[$base];
    }

    multi method log() {
        $.Num.log;
    }

    multi method log($base) {
        $.Num.log($base);
    }

    multi method log10() {
        $.Num.log10;
    }

    our Num multi method sin($base = Radians) {
        self.Num.sin($base);
    }

    our Num multi method cos($base = Radians) {
        self.Num.cos($base);
    }

    our Num multi method tan($base = Radians) {
        self.Num.tan($base);
    }

    our Num multi method sec($base = Radians) {
         self.Num.sec($base);
    }

    our Num multi method cosec($base = Radians) {
        self.Num.cosec($base);
    }

    our Num multi method cotan($base = Radians) {
        self.Num.cotan($base);
    }

    our Num multi method sinh($base = Radians) {
        self.Num.sinh($base);
    }

    our Num multi method cosh($base = Radians) {
        self.Num.cosh($base);
    }

    our Num multi method tanh($base = Radians) {
        self.Num.tanh($base);
    }

    our Num multi method sech($base = Radians) {
        self.Num.sech($base);
    }

    our Num multi method cosech($base = Radians) {
        self.Num.cosech($base);
    }

    our Num multi method cotanh($base = Radians) {
        self.Num.cotanh($base);
    }

    our Num multi method asin($base = Radians) {
        self.Num.asin($base);
    }

    our Num multi method acos($base = Radians) {
        self.Num.acos($base);
    }

    our Num multi method atan($base = Radians) {
        self.Num.atan($base);
    }

    our Num multi method atan2($x = 1, $base = Radians) {
        self.Num.atan2($x.Num, $base);
    }

    our Num multi method asec($base = Radians) {
        self.Num.asec($base);
    }

    our Num multi method acosec($base = Radians) {
        self.Num.acosec($base);
    }

    our Num multi method acotan($base = Radians) {
        self.Num.acotan($base);
    }

    our Num multi method asinh($base = Radians) {
        self.Num.asinh($base);
    }

    our Num multi method acosh($base = Radians) {
        self.Num.acosh($base);
    }

    our Num multi method atanh($base = Radians) {
        self.Num.atanh($base);
    }

    our Num multi method asech($base = Radians) {
        self.Num.asech($base);
    }

    our Num multi method acosech($base = Radians) {
        self.Num.acosech($base);
    }

    our Num multi method acotanh($base = Radians) {
        self.Num.acotanh($base);
    }

    our ::Complex multi method unpolar($angle) {
        Complex.new(self.Num * $angle.cos("radians"), self.Num * $angle.sin("radians"));
    }
}

our proto sub abs($x) { $x.abs }
our multi sub prefix:<abs>($x) { $x.abs }
our proto sub exp($exponent) { $exponent.exp }
our proto sub log($x) { $x.log }
our multi sub log($x, $base) { $x.log($base) }
our proto sub log10($x) { $x.log10 }
our proto sub cis($angle) { $angle.cis; }
our proto sub unpolar($mag, $angle) { $mag.unpolar($angle); }

# jnthn says that we should have both the multi sub declaration and the proto.

our multi sub sin($x, $base = Radians) {
    $x.sin($base)
}

our proto sin($x, $base = Radians) {
    sin($x, $base)
}

our multi sub asin($x, $base = Radians) {
    $x.asin($base)
}

our proto asin($x, $base = Radians) {
    asin($x, $base)
}

our multi sub cos($x, $base = Radians) {
    $x.cos($base)
}

our proto cos($x, $base = Radians) {
    cos($x, $base)
}

our multi sub acos($x, $base = Radians) {
    $x.acos($base)
}

our proto acos($x, $base = Radians) {
    acos($x, $base)
}

our multi sub tan($x, $base = Radians) {
    $x.tan($base)
}

our proto tan($x, $base = Radians) {
    tan($x, $base)
}

our multi sub atan($x, $base = Radians) {
    $x.atan($base)
}

our proto atan($x, $base = Radians) {
    atan($x, $base)
}

our multi sub sec($x, $base = Radians) {
    $x.sec($base)
}

our proto sec($x, $base = Radians) {
    sec($x, $base)
}

our multi sub asec($x, $base = Radians) {
    $x.asec($base)
}

our proto asec($x, $base = Radians) {
    asec($x, $base)
}

our multi sub cosec($x, $base = Radians) {
    $x.cosec($base)
}

our proto cosec($x, $base = Radians) {
    cosec($x, $base)
}

our multi sub acosec($x, $base = Radians) {
    $x.acosec($base)
}

our proto acosec($x, $base = Radians) {
    acosec($x, $base)
}

our multi sub cotan($x, $base = Radians) {
    $x.cotan($base)
}

our proto cotan($x, $base = Radians) {
    cotan($x, $base)
}

our multi sub acotan($x, $base = Radians) {
    $x.acotan($base)
}

our proto acotan($x, $base = Radians) {
    acotan($x, $base)
}

our multi sub sinh($x, $base = Radians) {
    $x.sinh($base)
}

our proto sinh($x, $base = Radians) {
    sinh($x, $base)
}

our multi sub asinh($x, $base = Radians) {
    $x.asinh($base)
}

our proto asinh($x, $base = Radians) {
    asinh($x, $base)
}

our multi sub cosh($x, $base = Radians) {
    $x.cosh($base)
}

our proto cosh($x, $base = Radians) {
    cosh($x, $base)
}

our multi sub acosh($x, $base = Radians) {
    $x.acosh($base)
}

our proto acosh($x, $base = Radians) {
    acosh($x, $base)
}

our multi sub tanh($x, $base = Radians) {
    $x.tanh($base)
}

our proto tanh($x, $base = Radians) {
    tanh($x, $base)
}

our multi sub atanh($x, $base = Radians) {
    $x.atanh($base)
}

our proto atanh($x, $base = Radians) {
    atanh($x, $base)
}

our multi sub sech($x, $base = Radians) {
    $x.sech($base)
}

our proto sech($x, $base = Radians) {
    sech($x, $base)
}

our multi sub asech($x, $base = Radians) {
    $x.asech($base)
}

our proto asech($x, $base = Radians) {
    asech($x, $base)
}

our multi sub cosech($x, $base = Radians) {
    $x.cosech($base)
}

our proto cosech($x, $base = Radians) {
    cosech($x, $base)
}

our multi sub acosech($x, $base = Radians) {
    $x.acosech($base)
}

our proto acosech($x, $base = Radians) {
    acosech($x, $base)
}

our multi sub cotanh($x, $base = Radians) {
    $x.cotanh($base)
}

our proto cotanh($x, $base = Radians) {
    cotanh($x, $base)
}

our multi sub acotanh($x, $base = Radians) {
    $x.acotanh($base)
}

our proto acotanh($x, $base = Radians) {
    acotanh($x, $base)
}

our multi sub atan2($y, $x = 1, $base = Radians) {
    $y.atan2($x, $base)
}

our proto atan2($y, $x = 1, $base = Radians) {
    atan2($y, $x, $base)
}

our Num sub rand (*@args) {
    die "too many arguments passed - 0 params expected" if ?@args;
    1.rand
}

our multi sub sqrt(Any $x) {
    $x.Num.sqrt
}

our proto sign($x) {
    defined($x) ?? $x.Num.sign !! Mu;
}

# This one doesn't seem to be needed with the above defined.
# our multi sub sign($x) {
#     defined($x) ?? $x.Num.sign !! undef;
# }

our multi sub roots($x, $n) {
    $x.Complex.roots($n)
}


our multi sub infix:<cmp>(Num $a, Num $b) {
    # TODO: should be Order::Same, ::Increase, ::Decrease once they work
    if $a == $b {
        0;
    } else {
        $a < $b ?? -1 !! 1;
    }
}

our multi sub infix:«<=>»($a, $b) {
    +$a cmp +$b
}

our proto chr($graph) {
    $graph.chr;
}

our sub srand(Int $seed = time) {
    pir::srand__0I($seed);
}

# vim: ft=perl6
