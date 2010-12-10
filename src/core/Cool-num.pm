augment class Cool {
    method Numeric() {
        pir::set__NP(self);
    }

    method Real() {
        (+self).Real;
    }

    method Int() {
        (+self).Int;
    }

    method Rat(::Real $epsilon = 1.0e-6) {
        (+self).Rat($epsilon);
    }

    method Num() {
        (+self).Num;
    }

    method abs($x:) {
        (+$x).abs;
    }

    method exp($x: $base = e) {
        (+$x).exp(+$base);
    }

    method log($x: $base = e) {
        (+$x).log(+$base);
    }

    method log10($x:) {
        (+$x).log10;
    }

    method sqrt($x:) {
        (+$x).sqrt;
    }

    method roots($x: $n) {
        (+$x).roots((+$n).Int);
    }

    method to-radians($x: $base) {
        (+$x).to-radians($base);
    }

    method from-radians($x: $base) {
        (+$x).from-radians($base);
    }

    method floor($x:) {
        (+$x).floor;
    }

    method ceiling($x:) {
        (+$x).ceiling;
    }

    method round($x: $scale = 1) is export {
        (+$x).round(+$scale);
    }

    method truncate($x:) {
        (+$x).truncate;
    }

    method sign($x:) {
        $x.defined ?? (+$x).sign !! Mu;
    }

    method cis($angle:) {
        (+$angle).cis
    }

    method unpolar($mag: $angle) {
        (+$mag).unpolar(+$angle);
    }

    our Str multi method chr() {
        ~(pir::chr__SI(self))
    }

    our Str multi method chrs() {
        self>>.chr.join;
    }

    method rand($x:) {
        (+$x).rand;
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

multi sub postfix:<i>($z) {
    (+$z)i;
}

proto sub chr($graph) { $graph.chr; }
proto sub chrs(@graphs) { @graphs.chrs; }
multi sub chrs(@graphs) { @graphs.chrs; }
multi sub chrs(*@graphs) { @graphs.chrs; }

proto sub srand($seed) {
    srand(+$seed);
}

INIT {
    # constant i = 1i;
    pir::set_hll_global__vsP('i', 1i);
}
    

# vim: ft=perl6
