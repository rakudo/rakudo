class Complex { ... }

role Real does Numeric {
    method Bridge() {
        fail "Bridge must be defined for the Real type " ~ self.WHAT;
    }

    method abs(Real $x:) {
        $x < 0 ?? -$x !! $x;
    }

    # Hmmm... should the second argument be Numeric for the next two?
    method exp(Real $exponent: Numeric $base = e) {
        $base ** $exponent;
    }

    method log(Real $x: Numeric $base = e) {
        $x.Bridge.log($base);
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

    # CHEAT: the .Bridges in unpolar should go away in the long run
    method unpolar(Real $mag: Real $angle) {
        Complex.new($mag.Bridge * $angle.Bridge.cos("radians"),
                    $mag.Bridge * $angle.Bridge.sin("radians"));
    }

    method cis(Real $angle:) {
        1.unpolar($angle);
    }
}

multi sub infix:«<=>»(Real $a, Real $b) {
    $a.Bridge <=> $b.Bridge;
}

multi sub infix:«<=>»(Num $a, Num $b) {
    $a cmp $b;
}

multi sub infix:«==»(Real $a, Real $b) {
    $a.Bridge == $b.Bridge;
}

multi sub infix:«==»(Num $a, Num $b) {
    pir::iseq__INN( $a, $b) ?? True !! False
}

multi sub infix:«<»(Real $a, Real $b) {
    $a.Bridge < $b.Bridge;
}

multi sub infix:«<»(Num $a, Num $b) {
    pir::islt__INN( $a, $b) ?? True !! False
}

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
