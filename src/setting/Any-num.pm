class Any is also {
    multi method exp() {
        self.Num.exp;
    }

    multi method abs {
        Q:PIR {
            $N0 = self
            $N0 = abs $N0
            %r = box $N0
        }
    }

    our Int multi method ceiling() is export {
        Q:PIR {
            $N0 = self
            $I0 = ceil $N0
            %r = box $I0
        }
    }

    our Str multi method chr() is export {
        Q:PIR {
            $I0 = self
            $S0 = chr $I0
            %r = 'prefix:~'($S0)
        }
    }

    multi method cis() is export {
        (1.0).unpolar(self)
    }

    our Int multi method floor() is export {
        Q:PIR {
            $N0 = self
            $I0 = floor $N0
            %r = box $I0
        }
    }

    our Num method rand() {
        Q:PIR {
            $N0 = self
            $N1 = rand $N0
            %r = box $N1
        }
    }

    multi method roots($n) {
        $.Complex.roots($n);
    }

    our Int multi method round() is export {
        Q:PIR {
            $N0 = self
            $N0 = $N0 + 0.5
            $I0 = floor $N0
            %r = box $I0
        }
    }

    multi method sqrt() {
        self.Num.sqrt;
    }

    # Used by the :Trig subs and methods in the Int and Num classes.
    our multi method !to-radians($base) {
        given $base {
            when /:i ^d/ { self * pi/180.0 }    # Convert from degrees.
            when /:i ^g/ { self * pi/200.0 }    # Convert from gradians.
            when /:i ^r/ { self }               # Convert from radians.
            when Num     { self * 2.0 * pi }    # Convert from revolutions.
            default { die "Unable to convert to base: $base" }
        }
    }

    multi method log() {
        $.Num.log();
    }

    our multi method !from-radians($base) {
        given $base {
            when /:i ^d/ { self * 180/pi  }    # Convert to degrees.
            when /:i ^g/ { self * 200/pi  }    # Convert to gradians.
            when /:i ^r/ { self }              # Convert to radians.
            when Num     { self /(2 * pi) }    # Convert to revolutions.
            default { die "Unable to convert to base: $base" }
        }
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

    # Having Any.sec breaks t/spec/S32-io/IO-Socket-INET.t ???
    # our Num multi method sec($base = 'radians') is export {
    #     self.Num.sec($base);
    # }

    our Num multi method cosec($base = 'radians') {
        self.Num.cosec($base);
    }

    our Num multi method cotan($base = 'radians') {
        self.Num.cotan($base);
    }

    our Num multi method sinh($base = 'radians') is export {
        self.Num.sinh($base);
    }

    our Num multi method cosh($base = 'radians') is export {
        self.Num.cosh($base);
    }

    our Num multi method tanh($base = 'radians') is export {
        self.Num.tanh($base);
    }

    our Num multi method sech($base = 'radians') is export {
        self.Num.sech($base);
    }

    our Num multi method cosech($base = 'radians') is export {
        self.Num.cosech($base);
    }

    our Num multi method cotanh($base = 'radians') is export {
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

    our Num multi method atan2($x = 1, $base = 'radians') is export {
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

    our Num multi method asinh($base = 'radians') is export {
        self.Num.asinh($base);
    }

    our Num multi method acosh($base = 'radians') is export {
        self.Num.acosh($base);
    }

    our Num multi method atanh($base = 'radians') is export {
        self.Num.atanh($base);
    }

    our Num multi method asech($base = 'radians') is export {
        self.Num.asech($base);
    }

    our Num multi method acosech($base = 'radians') is export {
        self.Num.acosech($base);
    }

    our Num multi method acotanh($base = 'radians') is export {
        self.Num.acotanh($base);
    }
}

multi sub abs($x) { (+$x).abs() }
multi sub exp($x) { $x.Num.exp() }
multi sub log($x) { $x.Num.log() }
multi sub log10($x) { $x.Num.log10 }

multi sub sin($x, $base = 'radians') {
    $x.sin($base)
}

multi sub asin($x, $base = 'radians') {
    $x.asin($base)
}

multi sub cos($x, $base = 'radians') {
    $x.cos($base)
}

multi sub acos($x, $base = 'radians') {
    $x.acos($base)
}

multi sub tan($x, $base = 'radians') {
    $x.tan($base)
}

multi sub atan($x, $base = 'radians') {
    $x.atan($base)
}

multi sub sec($x, $base = 'radians') {
    $x.sec($base)
}

multi sub asec($x, $base = 'radians') {
    $x.asec($base)
}

multi sub cosec($x, $base = 'radians') {
    $x.cosec($base)
}

multi sub acosec($x, $base = 'radians') {
    $x.acosec($base)
}

multi sub cotan($x, $base = 'radians') {
    $x.cotan($base)
}

multi sub acotan($x, $base = 'radians') {
    $x.acotan($base)
}

our Num sub rand (*@args) {
    die "too many arguments passed - 0 params expected" if @args;
    1.rand
}

multi sub sqrt(Any $x) {
    $x.Num.sqrt
}

multi sub roots($x, $n) {
    $x.Complex.roots($n)
}

# vim: ft=perl6
