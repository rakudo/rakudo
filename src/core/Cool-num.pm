augment class Cool {
    method Numeric() {
        pir::set__NP(self);
    }

    method abs() {
        (+self).abs;
    }

    method sign() {
        self.defined ?? (+self).sign !! Mu;
    }

    multi method exp() {
        self.Num.exp;
    }

    our Int multi method ceiling() is export {
        self.Num.ceiling;
    }

    our Int multi method floor() is export {
        self.Num.floor;
    }

    our Int multi method truncate() is export {
        self.Num.truncate;
    }

    our Int multi method round() is export {
        self.Num.round;
    }

    our Str multi method chr() {
        ~(pir::chr__SI(self))
    }

    # TODO: Probably should be little or no mention of .Num in Any
    our ::Complex multi method unpolar($angle) {
        self.Num.unpolar($angle);
    }

    multi method cis() {
        self.Num.cis
    }

    our Num method rand() {
        pir::box__PN(pir::rand__NN(self))
    }

    multi method roots($n) {
        $.Complex.roots($n);
    }

    multi method sqrt() {
        self.Num.sqrt;
    }

    multi method log($base = e) {
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
}

proto sub abs($x) { $x.abs }
multi sub prefix:<abs>($x) { $x.abs }
proto sub sign($x) { $x.sign }
proto sub exp($exponent, $base = e) { $exponent.exp($base) }
proto sub log($x, $base = e) { $x.log($base) }
proto sub log10($x) { $x.log10 }
proto sub cis($angle) { $angle.cis; }
proto sub unpolar($mag, $angle) { $mag.unpolar($angle); }
proto sub ceiling($x) { $x.ceiling; }
proto sub floor($x) { $x.floor; }
proto sub truncate($x) { $x.truncate; }
proto sub round($x, $scale = 1) { $x.round($scale); }

# jnthn says that we should have both the multi sub declaration and the proto.

multi sub sin($x, $base = Radians) {
    $x.sin($base)
}

proto sin($x, $base = Radians) {
    sin($x, $base)
}

multi sub asin($x, $base = Radians) {
    $x.asin($base)
}

proto asin($x, $base = Radians) {
    asin($x, $base)
}

multi sub cos($x, $base = Radians) {
    $x.cos($base)
}

proto cos($x, $base = Radians) {
    cos($x, $base)
}

multi sub acos($x, $base = Radians) {
    $x.acos($base)
}

proto acos($x, $base = Radians) {
    acos($x, $base)
}

multi sub tan($x, $base = Radians) {
    $x.tan($base)
}

proto tan($x, $base = Radians) {
    tan($x, $base)
}

multi sub atan($x, $base = Radians) {
    $x.atan($base)
}

proto atan($x, $base = Radians) {
    atan($x, $base)
}

multi sub sec($x, $base = Radians) {
    $x.sec($base)
}

proto sec($x, $base = Radians) {
    sec($x, $base)
}

multi sub asec($x, $base = Radians) {
    $x.asec($base)
}

proto asec($x, $base = Radians) {
    asec($x, $base)
}

multi sub cosec($x, $base = Radians) {
    $x.cosec($base)
}

proto cosec($x, $base = Radians) {
    cosec($x, $base)
}

multi sub acosec($x, $base = Radians) {
    $x.acosec($base)
}

proto acosec($x, $base = Radians) {
    acosec($x, $base)
}

multi sub cotan($x, $base = Radians) {
    $x.cotan($base)
}

proto cotan($x, $base = Radians) {
    cotan($x, $base)
}

multi sub acotan($x, $base = Radians) {
    $x.acotan($base)
}

proto acotan($x, $base = Radians) {
    acotan($x, $base)
}

multi sub sinh($x, $base = Radians) {
    $x.sinh($base)
}

proto sinh($x, $base = Radians) {
    sinh($x, $base)
}

multi sub asinh($x, $base = Radians) {
    $x.asinh($base)
}

proto asinh($x, $base = Radians) {
    asinh($x, $base)
}

multi sub cosh($x, $base = Radians) {
    $x.cosh($base)
}

proto cosh($x, $base = Radians) {
    cosh($x, $base)
}

multi sub acosh($x, $base = Radians) {
    $x.acosh($base)
}

proto acosh($x, $base = Radians) {
    acosh($x, $base)
}

multi sub tanh($x, $base = Radians) {
    $x.tanh($base)
}

proto tanh($x, $base = Radians) {
    tanh($x, $base)
}

multi sub atanh($x, $base = Radians) {
    $x.atanh($base)
}

proto atanh($x, $base = Radians) {
    atanh($x, $base)
}

multi sub sech($x, $base = Radians) {
    $x.sech($base)
}

proto sech($x, $base = Radians) {
    sech($x, $base)
}

multi sub asech($x, $base = Radians) {
    $x.asech($base)
}

proto asech($x, $base = Radians) {
    asech($x, $base)
}

multi sub cosech($x, $base = Radians) {
    $x.cosech($base)
}

proto cosech($x, $base = Radians) {
    cosech($x, $base)
}

multi sub acosech($x, $base = Radians) {
    $x.acosech($base)
}

proto acosech($x, $base = Radians) {
    acosech($x, $base)
}

multi sub cotanh($x, $base = Radians) {
    $x.cotanh($base)
}

proto cotanh($x, $base = Radians) {
    cotanh($x, $base)
}

multi sub acotanh($x, $base = Radians) {
    $x.acotanh($base)
}

proto acotanh($x, $base = Radians) {
    acotanh($x, $base)
}

multi sub atan2($y, $x = 1, $base = Radians) {
    $y.atan2($x, $base)
}

proto atan2($y, $x = 1, $base = Radians) {
    atan2($y, $x, $base)
}

my Num sub rand (*@args) {
    die "too many arguments passed - 0 params expected" if ?@args;
    1.rand
}

multi sub sqrt(Any $x) {
    $x.Num.sqrt
}

multi sub roots($x, $n) {
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

proto chr($graph) {
    $graph.chr;
}

sub srand(Int $seed = time) {
    pir::srand__0I($seed);
}

# vim: ft=perl6
