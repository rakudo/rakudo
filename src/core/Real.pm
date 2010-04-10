class Complex { ... }

role Real does Numeric {
    method Bridge() {
        fail "Bridge must be defined for the Real type " ~ self.WHAT;
    }

    method abs() {
        self < 0 ?? -self !! self;
    }

    method sign {
        self.notdef ?? Mu
                    !! (self ~~ NaN ?? NaN !! self <=> 0);
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

multi sub infix:<->(Real $a, Real $b) {
    $a.Bridge - $b.Bridge;
}

multi sub infix:<->(Num $a, Num $b) {
    pir::sub__NNN($a, $b)
}
