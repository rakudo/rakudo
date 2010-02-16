augment class Any {
    multi method abs() {
        pir::abs__Nn(self.Num);
    }

    multi method exp() {
        self.Num.exp;
    }

    our Int multi method ceiling() is export {
        pir::box__PN(pir::ceil__IN(self))
    }

    our Str multi method chr() {
        ~(pir::chr__SI(self))
    }

    multi method cis() is export {
        1.unpolar(self)
    }

    our Int multi method floor() is export {
        pir::box__PI(pir::floor__IN(self))
    }

    our Num method rand() {
        pir::box__PN(pir::rand__NN(self))
    }

    multi method roots($n) {
        $.Complex.roots($n);
    }

    our Int multi method round() is export {
#        pir::box__PI(pir::floor__IN(pir::add__NNN(self, 0.5)))
    }

    multi method sqrt() {
        self.Num.sqrt;
    }

    # Used by the :Trig subs and methods in the Int and Num classes.
    our multi method !to-radians($base) {
        given $base {
            when /:i degrees/  { self * (312689/99532)/180.0 }    # Convert from degrees.
            when /:i gradians/ { self * (312689/99532)/200.0 }    # Convert from gradians.
            when /:i radians/  { self + 0 }                       # Convert from radians.
            when Num           { self * 2.0 * (312689/99532) }    # Convert from revolutions.
            default            { die "Unable to convert to base: $base" }
        }
    }

    our multi method !from-radians($base) {
        given $base {
            when /:i degrees/  { self * 180/(312689/99532)  }    # Convert to degrees.
            when /:i gradians/ { self * 200/(312689/99532)  }    # Convert to gradians.
            when /:i radians/  { self + 0 }                      # Convert to radians.
            when Num           { self /(2 * (312689/99532)) }    # Convert to revolutions.
            default            { die "Unable to convert to base: $base" }
        }
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

    our Num multi method sin($base = 'radians') {
        self.Num.sin($base);
    }

    our Num multi method cos($base = 'radians') {
        self.Num.cos($base);
    }

    our Num multi method tan($base = 'radians') {
        self.Num.tan($base);
    }

    our Num multi method sec($base = 'radians') is export {
         self.Num.sec($base);
    }

    our Num multi method cosec($base = 'radians') {
        self.Num.cosec($base);
    }

    our Num multi method cotan($base = 'radians') {
        self.Num.cotan($base);
    }

    our Num multi method sinh($base = 'radians') {
        self.Num.sinh($base);
    }

    our Num multi method cosh($base = 'radians') {
        self.Num.cosh($base);
    }

    our Num multi method tanh($base = 'radians') {
        self.Num.tanh($base);
    }

    our Num multi method sech($base = 'radians') {
        self.Num.sech($base);
    }

    our Num multi method cosech($base = 'radians') {
        self.Num.cosech($base);
    }

    our Num multi method cotanh($base = 'radians') {
        self.Num.cotanh($base);
    }

    our Num multi method asin($base = 'radians') {
        self.Num.asin($base);
    }

    our Num multi method acos($base = 'radians') {
        self.Num.acos($base);
    }

    our Num multi method atan($base = 'radians') {
        self.Num.atan($base);
    }

    our Num multi method atan2($x = 1, $base = 'radians') {
        self.Num.atan2($x, $base);
    }

    our Num multi method asec($base = 'radians') {
        self.Num.asec($base);
    }

    our Num multi method acosec($base = 'radians') {
        self.Num.acosec($base);
    }

    our Num multi method acotan($base = 'radians') {
        self.Num.acotan($base);
    }

    our Num multi method asinh($base = 'radians') {
        self.Num.asinh($base);
    }

    our Num multi method acosh($base = 'radians') {
        self.Num.acosh($base);
    }

    our Num multi method atanh($base = 'radians') {
        self.Num.atanh($base);
    }

    our Num multi method asech($base = 'radians') {
        self.Num.asech($base);
    }

    our Num multi method acosech($base = 'radians') {
        self.Num.acosech($base);
    }

    our Num multi method acotanh($base = 'radians') {
        self.Num.acotanh($base);
    }

    our ::Complex multi method unpolar($angle) is export {
        Complex.new(self.Num * $angle.cos("radians"), self.Num * $angle.sin("radians"));
    }
}

our proto sub abs($x) { $x.abs }
our multi sub prefix:<abs>($x) { $x.abs }
our proto sub exp($exponent) { $exponent.exp }
our proto sub log($x) { $x.log }
our multi sub log($x, $base) { $x.log($base) }
our proto sub log10($x) { $x.log10 }

# jnthn says that we should have both the multi sub declaration and the proto.

our multi sub sin($x, $base = 'radians') {
    $x.sin($base)
}

our proto sin($x, $base = 'radians') {
    sin($x, $base)
}

our multi sub asin($x, $base = 'radians') {
    $x.asin($base)
}

our proto asin($x, $base = 'radians') {
    asin($x, $base)
}

our multi sub cos($x, $base = 'radians') {
    $x.cos($base)
}

our proto cos($x, $base = 'radians') {
    cos($x, $base)
}

our multi sub acos($x, $base = 'radians') {
    $x.acos($base)
}

our proto acos($x, $base = 'radians') {
    acos($x, $base)
}

our multi sub tan($x, $base = 'radians') {
    $x.tan($base)
}

our proto tan($x, $base = 'radians') {
    tan($x, $base)
}

our multi sub atan($x, $base = 'radians') {
    $x.atan($base)
}

our proto atan($x, $base = 'radians') {
    atan($x, $base)
}

our multi sub sec($x, $base = 'radians') {
    $x.sec($base)
}

our proto sec($x, $base = 'radians') {
    sec($x, $base)
}

our multi sub asec($x, $base = 'radians') {
    $x.asec($base)
}

our proto asec($x, $base = 'radians') {
    asec($x, $base)
}

our multi sub cosec($x, $base = 'radians') {
    $x.cosec($base)
}

our proto cosec($x, $base = 'radians') {
    cosec($x, $base)
}

our multi sub acosec($x, $base = 'radians') {
    $x.acosec($base)
}

our proto acosec($x, $base = 'radians') {
    acosec($x, $base)
}

our multi sub cotan($x, $base = 'radians') {
    $x.cotan($base)
}

our proto cotan($x, $base = 'radians') {
    cotan($x, $base)
}

our multi sub acotan($x, $base = 'radians') {
    $x.acotan($base)
}

our proto acotan($x, $base = 'radians') {
    acotan($x, $base)
}

our multi sub sinh($x, $base = 'radians') {
    $x.sinh($base)
}

our proto sinh($x, $base = 'radians') {
    sinh($x, $base)
}

our multi sub asinh($x, $base = 'radians') {
    $x.asinh($base)
}

our proto asinh($x, $base = 'radians') {
    asinh($x, $base)
}

our multi sub cosh($x, $base = 'radians') {
    $x.cosh($base)
}

our proto cosh($x, $base = 'radians') {
    cosh($x, $base)
}

our multi sub acosh($x, $base = 'radians') {
    $x.acosh($base)
}

our proto acosh($x, $base = 'radians') {
    acosh($x, $base)
}

our multi sub tanh($x, $base = 'radians') {
    $x.tanh($base)
}

our proto tanh($x, $base = 'radians') {
    tanh($x, $base)
}

our multi sub atanh($x, $base = 'radians') {
    $x.atanh($base)
}

our proto atanh($x, $base = 'radians') {
    atanh($x, $base)
}

our multi sub sech($x, $base = 'radians') {
    $x.sech($base)
}

our proto sech($x, $base = 'radians') {
    sech($x, $base)
}

our multi sub asech($x, $base = 'radians') {
    $x.asech($base)
}

our proto asech($x, $base = 'radians') {
    asech($x, $base)
}

our multi sub cosech($x, $base = 'radians') {
    $x.cosech($base)
}

our proto cosech($x, $base = 'radians') {
    cosech($x, $base)
}

our multi sub acosech($x, $base = 'radians') {
    $x.acosech($base)
}

our proto acosech($x, $base = 'radians') {
    acosech($x, $base)
}

our multi sub cotanh($x, $base = 'radians') {
    $x.cotanh($base)
}

our proto cotanh($x, $base = 'radians') {
    cotanh($x, $base)
}

our multi sub acotanh($x, $base = 'radians') {
    $x.acotanh($base)
}

our proto acotanh($x, $base = 'radians') {
    acotanh($x, $base)
}

our multi sub atan2($y, $x = 1, $base = 'radians') {
    $y.atan2($x, $base)
}

our proto atan2($y, $x = 1, $base = 'radians') {
    atan2($y, $x, $base)
}

our Num sub rand (*@args) {
    die "too many arguments passed - 0 params expected" if @args;
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

# vim: ft=perl6
