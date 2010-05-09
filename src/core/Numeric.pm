role Numeric {
    method Numeric() {
        self;
    }

    method abs(Numeric $x:) {
        note "$.WHAT() needs a version of .abs";
        fail "$.WHAT() needs a version of .abs";
    }

    multi method exp(Numeric $exponent: Numeric $base = e) {
        note "$.WHAT() needs a version of .exp";
        fail "$.WHAT() needs a version of .exp";
    }

    method ln(Numeric $x:) {
        note "$.WHAT() needs a version of .ln";
        fail "$.WHAT() needs a version of .ln";
    }

    method log(Numeric $x: Numeric $base = e) {
        $x.ln / $base.ln;
    }

    method log10(Numeric $x:) {
        self.log(10);
    }

    method sqrt(Numeric $x:) {
        note "$.WHAT() needs a version of .sqrt";
        fail "$.WHAT() needs a version of .sqrt";
    }

    INIT {
        our @trig-base-conversions = (1.0, pi / 180.0, pi / 200.0, 2.0 * pi);
    }

    # Used by the :Trig subs and methods in the Int and Num classes.
    method to-radians(Numeric $x: $base) {
        $x * pir::get_global__Ps('@trig-base-conversions')[$base];
    }

    method from-radians(Numeric $x: $base) {
        $x / pir::get_global__Ps('@trig-base-conversions')[$base];
    }

    method sin(Numeric $x: $base = Radians) {
        note "$.WHAT() needs a version of .sin";
        fail "$.WHAT() needs a version of .sin";
    }
}

multi sub infix:«cmp»(Numeric $a, Numeric $b) { $a <=> $b; }
