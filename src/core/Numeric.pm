role Numeric {
    method ACCEPTS($other) {
        my @a = self.reals;
        my @b = (+$other).reals;
        if @a.grep("NaN").elems > 0 {
            @b.grep("NaN").elems > 0;
        } else {
            $other == self;
        }
    }

    method Numeric() {
        self;
    }

    # NOTE: Real is defined as failing if the number in
    # question is not convertable to Real, eg a Complex
    # with a non-zero imaginary part.
    method Real() {
        note "$.WHAT() needs a version of .Real";
        fail "$.WHAT() needs a version of .Real";
    }

    method Int() {
        self.Real.Int;
    }

    method Rat(::Real $epsilon = 1.0e-6) {
        self.Real.Rat($epsilon);
    }

    method Num() {
        self.Real.Num;
    }

    method Str() {
        note "$.WHAT() needs a version of .Str";
        fail "$.WHAT() needs a version of .Str";
    }

    method reals() {
        note "$.WHAT() needs a version of .reals";
        fail "$.WHAT() needs a version of .reals";
    }

    method succ(Numeric $x:) {
        $x + 1;
    }

    method pred(Numeric $x:) {
        $x - 1;
    }

    method abs(Numeric $x:) {
        note "$.WHAT() needs a version of .abs";
        fail "$.WHAT() needs a version of .abs";
    }

    method exp(Numeric $exponent: Numeric $base = e) {
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

    method roots(Numeric $x: Int $n) {
        note "$.WHAT() needs a version of .roots";
        fail "$.WHAT() needs a version of .roots";
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

    method sign(Numeric $x:) {
        note "sign is only defined for Reals, you have a $.WHAT()";
        fail "sign is only defined for Reals, you have a $.WHAT()";
    }

    method floor(Numeric $x:) {
        note "floor is only defined for Reals, you have a $.WHAT()";
        fail "floor is only defined for Reals, you have a $.WHAT()";
    }

    method ceiling(Numeric $x:) {
        note "ceiling is only defined for Reals, you have a $.WHAT()";
        fail "ceiling is only defined for Reals, you have a $.WHAT()";
    }

    method truncate(Numeric $x:) {
        note "truncate is only defined for Reals, you have a $.WHAT()";
        fail "truncate is only defined for Reals, you have a $.WHAT()";
    }

    method round(Numeric $x: $scale = 1) {
        note "round is only defined for Reals, you have a $.WHAT()";
        fail "round is only defined for Reals, you have a $.WHAT()";
    }

    method cis(Numeric $angle:) {
        note "cis is only defined for Reals, you have a $.WHAT()";
        fail "cis is only defined for Reals, you have a $.WHAT()";
    }

    method unpolar(Numeric $mag: Numeric $angle) {
        note "unpolar is only defined for Reals, you have a $.WHAT()";
        fail "unpolar is only defined for Reals, you have a $.WHAT()";
    }

    method sin(Numeric $x: $base = Radians) {
        note "$.WHAT() needs a version of .sin";
        fail "$.WHAT() needs a version of .sin";
    }

    method asin(Numeric $x: $base = Radians) {
        note "$.WHAT() needs a version of .asin";
        fail "$.WHAT() needs a version of .asin";
    }

    method cos(Numeric $x: $base = Radians) {
        note "$.WHAT() needs a version of .cos";
        fail "$.WHAT() needs a version of .cos";
    }

    method acos(Numeric $x: $base = Radians) {
        note "$.WHAT() needs a version of .acos";
        fail "$.WHAT() needs a version of .acos";
    }

    method tan(Numeric $x: $base = Radians) {
        note "$.WHAT() needs a version of .tan";
        fail "$.WHAT() needs a version of .tan";
    }

    method atan(Numeric $x: $base = Radians) {
        note "$.WHAT() needs a version of .atan";
        fail "$.WHAT() needs a version of .atan";
    }

    method sec(Numeric $x: $base = Radians) {
        note "$.WHAT() needs a version of .sec";
        fail "$.WHAT() needs a version of .sec";
    }

    method asec(Numeric $x: $base = Radians) {
        note "$.WHAT() needs a version of .asec";
        fail "$.WHAT() needs a version of .asec";
    }

    method cosec(Numeric $x: $base = Radians) {
        note "$.WHAT() needs a version of .cosec";
        fail "$.WHAT() needs a version of .cosec";
    }

    method acosec(Numeric $x: $base = Radians) {
        note "$.WHAT() needs a version of .acosec";
        fail "$.WHAT() needs a version of .acosec";
    }

    method cotan(Numeric $x: $base = Radians) {
        note "$.WHAT() needs a version of .cotan";
        fail "$.WHAT() needs a version of .cotan";
    }

    method acotan(Numeric $x: $base = Radians) {
        note "$.WHAT() needs a version of .acotan";
        fail "$.WHAT() needs a version of .acotan";
    }

    method sinh(Numeric $x: $base = Radians) {
        note "$.WHAT() needs a version of .sinh";
        fail "$.WHAT() needs a version of .sinh";
    }

    method asinh(Numeric $x: $base = Radians) {
        note "$.WHAT() needs a version of .asinh";
        fail "$.WHAT() needs a version of .asinh";
    }

    method cosh(Numeric $x: $base = Radians) {
        note "$.WHAT() needs a version of .cosh";
        fail "$.WHAT() needs a version of .cosh";
    }

    method acosh(Numeric $x: $base = Radians) {
        note "$.WHAT() needs a version of .acosh";
        fail "$.WHAT() needs a version of .acosh";
    }

    method tanh(Numeric $x: $base = Radians) {
        note "$.WHAT() needs a version of .tanh";
        fail "$.WHAT() needs a version of .tanh";
    }

    method atanh(Numeric $x: $base = Radians) {
        note "$.WHAT() needs a version of .atanh";
        fail "$.WHAT() needs a version of .atanh";
    }

    method sech(Numeric $x: $base = Radians) {
        note "$.WHAT() needs a version of .sech";
        fail "$.WHAT() needs a version of .sech";
    }

    method asech(Numeric $x: $base = Radians) {
        note "$.WHAT() needs a version of .asech";
        fail "$.WHAT() needs a version of .asech";
    }

    method cosech(Numeric $x: $base = Radians) {
        note "$.WHAT() needs a version of .cosech";
        fail "$.WHAT() needs a version of .cosech";
    }

    method acosech(Numeric $x: $base = Radians) {
        note "$.WHAT() needs a version of .acosech";
        fail "$.WHAT() needs a version of .acosech";
    }

    method cotanh(Numeric $x: $base = Radians) {
        note "$.WHAT() needs a version of .cotanh";
        fail "$.WHAT() needs a version of .cotanh";
    }

    method acotanh(Numeric $x: $base = Radians) {
        note "$.WHAT() needs a version of .acotanh";
        fail "$.WHAT() needs a version of .acotanh";
    }

    method atan2(Numeric $y: Numeric $x = 1, $base = Radians) {
        note "atan2 is only defined for Reals, you have a $.WHAT()";
        fail "atan2 is only defined for Reals, you have a $.WHAT()";
    }
}

multi sub postfix:<i>(Numeric $z) {
    $z * 1i;
}

multi sub infix:«cmp»(Numeric $a, Numeric $b) { $a <=> $b; }

multi sub infix:«<=>»(Numeric $a, Numeric $b) {
    my @a = $a.reals;
    my @b = $b.reals;
    @a.push(0 xx +@b - +@a) if (+@a < +@b);
    @b.push(0 xx +@a - +@b) if (+@b < +@a);

    [||] (@a Z<=> @b);
}

multi sub infix:«==»(Numeric $a, Numeric $b) {
    ($a <=> $b) == 0;
}

multi sub infix:«!=»(Numeric $a, Numeric $b) {
    ($a <=> $b) != 0;
}

multi sub infix:«<»(Numeric $a, Numeric $b) {
    ($a <=> $b) == -1;
}

multi sub infix:«>»(Numeric $a, Numeric $b) {
    ($a <=> $b) == +1;
}

multi sub infix:«<=»(Numeric $a, Numeric $b) {
    ($a <=> $b) != +1;
}

multi sub infix:«>=»(Numeric $a, Numeric $b) {
    ($a <=> $b) != -1;
}
