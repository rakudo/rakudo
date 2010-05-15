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

    method sqrt($x:) {
        (+$x).sqrt;
    }

    multi method log($x: $base = e) {
        (+$x).log(+$base);
    }

    multi method log10() {
        $.Num.log10;
    }

    method sin($x: $base = Radians) {
        (+$x).sin($base);
    }

    method cos($x: $base = Radians) {
        (+$x).cos($base);
    }

    method tan($x: $base = Radians) {
        (+$x).tan($base);
    }

    method sec($x: $base = Radians) {
         (+$x).sec($base);
    }

    method cosec($x: $base = Radians) {
        (+$x).cosec($base);
    }

    method cotan($x: $base = Radians) {
        (+$x).cotan($base);
    }

    method sinh($x: $base = Radians) {
        (+$x).sinh($base);
    }

    method cosh($x: $base = Radians) {
        (+$x).cosh($base);
    }

    method tanh($x: $base = Radians) {
        (+$x).tanh($base);
    }

    method sech($x: $base = Radians) {
        (+$x).sech($base);
    }

    method cosech($x: $base = Radians) {
        (+$x).cosech($base);
    }

    method cotanh($x: $base = Radians) {
        (+$x).cotanh($base);
    }

    method asin($x: $base = Radians) {
        (+$x).asin($base);
    }

    method acos($x: $base = Radians) {
        (+$x).acos($base);
    }

    method atan($x: $base = Radians) {
        (+$x).atan($base);
    }

    method atan2($y: $x = 1, $base = Radians) {
        (+$y).atan2(+$x, $base);
    }

    method asec($x: $base = Radians) {
        (+$x).asec($base);
    }

    method acosec($x: $base = Radians) {
        (+$x).acosec($base);
    }

    method acotan($x: $base = Radians) {
        (+$x).acotan($base);
    }

    method asinh($x: $base = Radians) {
        (+$x).asinh($base);
    }

    method acosh($x: $base = Radians) {
        (+$x).acosh($base);
    }

    method atanh($x: $base = Radians) {
        (+$x).atanh($base);
    }

    method asech($x: $base = Radians) {
        (+$x).asech($base);
    }

    method acosech($x: $base = Radians) {
        (+$x).acosech($base);
    }

    method acotanh($x: $base = Radians) {
        (+$x).acotanh($base);
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

proto sin($x, $base = Radians) {
    $x.sin($base)
}

proto asin($x, $base = Radians) {
    $x.asin($base)
}

proto cos($x, $base = Radians) {
    $x.cos($base)
}

proto acos($x, $base = Radians) {
    $x.acos($base)
}

proto tan($x, $base = Radians) {
    $x.tan($base)
}

proto atan($x, $base = Radians) {
    $x.atan($base)
}

proto sec($x, $base = Radians) {
    $x.sec($base)
}

proto asec($x, $base = Radians) {
    $x.asec($base)
}

proto cosec($x, $base = Radians) {
    $x.cosec($base)
}

proto acosec($x, $base = Radians) {
    $x.acosec($base)
}

proto cotan($x, $base = Radians) {
    $x.cotan($base)
}

proto acotan($x, $base = Radians) {
    $x.acotan($base)
}

proto sinh($x, $base = Radians) {
    $x.sinh($base)
}

proto asinh($x, $base = Radians) {
    $x.asinh($base)
}

proto cosh($x, $base = Radians) {
    $x.cosh($base)
}

proto acosh($x, $base = Radians) {
    $x.acosh($base)
}

proto tanh($x, $base = Radians) {
    $x.tanh($base)
}

proto atanh($x, $base = Radians) {
    $x.atanh($base)
}

proto sech($x, $base = Radians) {
    $x.sech($base)
}

proto asech($x, $base = Radians) {
    $x.asech($base)
}

proto cosech($x, $base = Radians) {
    $x.cosech($base)
}

proto acosech($x, $base = Radians) {
    $x.acosech($base)
}

proto cotanh($x, $base = Radians) {
    $x.cotanh($base)
}

proto acotanh($x, $base = Radians) {
    $x.acotanh($base)
}

proto atan2($y, $x = 1, $base = Radians) {
    $y.atan2($x, $base)
}

my Num sub rand (*@args) {
    die "too many arguments passed - 0 params expected" if ?@args;
    1.rand
}

proto sub sqrt(Any $x) {
    (+$x).sqrt
}

multi sub roots($x, $n) {
    $x.Complex.roots($n)
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
