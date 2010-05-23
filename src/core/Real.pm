class Complex { ... }

role Real does Numeric {
    method Bridge() {
        fail "Bridge must be defined for the Real type " ~ self.WHAT;
    }

    method Int() {
        self.truncate;
    }

    method Rat(Real $epsilon = 1.0e-6) {
        self.Bridge.Rat($epsilon);
    }

    method Num() {
        self.Bridge.Num;
    }

    method Complex() {
        Complex.new(self, 0);
    }

    method reals() {
        (self);
    }

    method abs(Real $x:) {
        $x < 0 ?? -$x !! $x;
    }

    # Hmmm... should the second argument be Numeric for the next two?
    method exp(Real $exponent: Numeric $base = e) {
        $base ** $exponent;
    }

    method ln(Real $x:) {
        $x.Bridge.ln;
    }

    method sign(Real $x:) {
        $x.notdef ?? Mu
                    !! ($x ~~ NaN ?? NaN !! $x <=> 0);
    }

    method ceiling(Real $x:) {
        $x.Bridge.ceiling;
    }

    method floor(Real $x:) {
        $x.Bridge.floor;
    }

    method truncate(Real $x:) {
        $x == 0 ?? 0 !! $x < 0  ?? $x.ceiling !! $x.floor
    }

    method round(Real $x: $scale = 1) {
        floor($x / $scale + 0.5) * $scale;
    }

    method unpolar(Real $mag: Real $angle) {
        Complex.new($mag * $angle.cos(Radians),
                    $mag * $angle.sin(Radians));
    }

    method cis(Real $angle:) {
        1.unpolar($angle);
    }

    method sqrt(Real $x:) {
        $x.Bridge.sqrt;
    }

    method roots(Real $x: Int $n) {
        $x.Complex.roots($n);
    }

    method sin(Real $x: $base = Radians) {
        $x.Bridge.sin($base);
    }

    method asin(Real $x: $base = Radians) {
        $x.Bridge.asin($base);
    }

    method cos(Real $x: $base = Radians) {
        $x.Bridge.cos($base);
    }

    method acos(Real $x: $base = Radians) {
        $x.Bridge.acos($base);
    }

    method tan(Real $x: $base = Radians) {
        $x.Bridge.tan($base);
    }

    method atan(Real $x: $base = Radians) {
        $x.Bridge.atan($base);
    }

    method sec(Real $x: $base = Radians) {
        $x.Bridge.sec($base);
    }

    method asec(Real $x: $base = Radians) {
        $x.Bridge.asec($base);
    }

    method cosec(Real $x: $base = Radians) {
        $x.Bridge.cosec($base);
    }

    method acosec(Real $x: $base = Radians) {
        $x.Bridge.acosec($base);
    }

    method cotan(Real $x: $base = Radians) {
        $x.Bridge.cotan($base);
    }

    method acotan(Real $x: $base = Radians) {
        $x.Bridge.acotan($base);
    }

    method sinh(Real $x: $base = Radians) {
        $x.Bridge.sinh($base);
    }

    method asinh(Real $x: $base = Radians) {
        $x.Bridge.asinh($base);
    }

    method cosh(Real $x: $base = Radians) {
        $x.Bridge.cosh($base);
    }

    method acosh(Real $x: $base = Radians) {
        $x.Bridge.acosh($base);
    }

    method tanh(Real $x: $base = Radians) {
        $x.Bridge.tanh($base);
    }

    method atanh(Real $x: $base = Radians) {
        $x.Bridge.atanh($base);
    }

    method sech(Real $x: $base = Radians) {
        $x.Bridge.sech($base);
    }

    method asech(Real $x: $base = Radians) {
        $x.Bridge.asech($base);
    }

    method cosech(Real $x: $base = Radians) {
        $x.Bridge.cosech($base);
    }

    method acosech(Real $x: $base = Radians) {
        $x.Bridge.acosech($base);
    }

    method cotanh(Real $x: $base = Radians) {
        $x.Bridge.cotanh($base);
    }

    method acotanh(Real $x: $base = Radians) {
        $x.Bridge.acotanh($base);
    }

    method atan2(Real $y: Real $x = 1, $base = Radians) {
        $y.Bridge.atan2($x.Bridge, $base);
    }
}

# Comparison operators

multi sub infix:«<=>»(Real $a, Real $b) {
    $a.Bridge <=> $b.Bridge;
}

multi sub infix:«<=>»(Num $a, Num $b) {
    # TODO: should be Order::Same, ::Increase, ::Decrease once they work
    if $a == $b {
        0;
    } else {
        $a < $b ?? -1 !! 1;
    }
}

multi sub infix:«==»(Real $a, Real $b) {
    $a.Bridge == $b.Bridge;
}

multi sub infix:«==»(Num $a, Num $b) {
    pir::iseq__INN( $a, $b) ?? True !! False
}

multi sub infix:«!=»(Real $a, Real $b) {
    $a.Bridge != $b.Bridge;
}

multi sub infix:«!=»(Num $a, Num $b) {
    pir::iseq__INN( $a, $b) ?? False !! True # note reversed
}

multi sub infix:«<»(Real $a, Real $b) {
    $a.Bridge < $b.Bridge;
}

multi sub infix:«<»(Num $a, Num $b) {
    pir::islt__INN( $a, $b) ?? True !! False
}

multi sub infix:«>»(Real $a, Real $b) {
    $a.Bridge > $b.Bridge;
}

multi sub infix:«>»(Num $a, Num $b) {
    pir::isgt__INN( $a, $b) ?? True !! False
}

multi sub infix:«<=»(Real $a, Real $b) {
    $a.Bridge <= $b.Bridge;
}

multi sub infix:«<=»(Num $a, Num $b) {
    pir::isgt__INN( $a, $b) ?? False !! True # note reversed
}

multi sub infix:«>=»(Real $a, Real $b) {
    $a.Bridge >= $b.Bridge;
}

multi sub infix:«>=»(Num $a, Num $b) {
    pir::islt__INN( $a, $b) ?? False !! True # note reversed
}

# Arithmetic operators

multi sub prefix:<->(Real $a) {
    -($a.Bridge);
}

multi sub prefix:<->(Num $a) {
    pir::neg__NN($a);
}

multi sub infix:<+>(Real $a, Real $b) {
    $a.Bridge + $b.Bridge;
}

multi sub infix:<+>(Num $a, Num $b) {
    pir::add__NNN($a, $b)
}

multi sub infix:<->(Real $a, Real $b) {
    $a.Bridge - $b.Bridge;
}

multi sub infix:<->(Num $a, Num $b) {
    pir::sub__NNN($a, $b)
}

multi sub infix:<*>(Real $a, Real $b) {
    $a.Bridge * $b.Bridge;
}

multi sub infix:<*>(Num $a, Num $b) {
    pir::mul__NNN($a, $b)
}

multi sub infix:</>(Real $a, Real $b) {
    $a.Bridge / $b.Bridge;
}

multi sub infix:</>(Num $a, Num $b) {
    pir::div__NNN($a, $b)
}

multi sub infix:<**>(Real $a, Real $b) {
    $a.Bridge ** $b.Bridge;
}

multi sub infix:<**>(Num $a, Num $b) {
    pir::pow__NNN($a, $b)
}
