augment class Num {
    multi method Int() {
        Q:PIR {
            $P0 = find_lex 'self'
            $I0 = $P0
            $P1 = new ['Int']
            $P1 = $I0
            %r  = $P1
        }
    }

    multi method Num() { self; }

    multi method Complex() { self + 0i; }

    multi method abs() {
        pir::abs__Nn(self);
    }

    multi method exp() {
        pir::exp__Nn(self);
    }

    multi method log() {
        pir::ln__Nn(self);
    }

    multi method log($base) {
        $.log / $base.log;
    }

    multi method log10() {
        pir::log10__Nn(self);
    }

    multi method sqrt() {
        pir::sqrt__Nn(self);
    }

    our Int multi method sign {
        # self ~~ NaN ?? NaN !! self <=> 0;
        self eq NaN ?? NaN !! (self < 0 ?? -1 !! ( self == 0 ?? 0 !! 1));
    }

    multi method sin($base = 'radians') {
        pir::sin__Nn(self!to-radians($base));
    }

    multi method asin($base = 'radians') {
        pir::asin__Nn(self)!from-radians($base);
    }

    multi method cos($base = 'radians') {
        pir::cos__Nn(self!to-radians($base));
    }

    multi method acos($base = 'radians') {
        pir::acos__Nn(self)!from-radians($base);
    }

    multi method tan($base = 'radians') {
        pir::tan__Nn(self!to-radians($base));
    }

    multi method atan($base = 'radians') {
        pir::atan__Nn(self)!from-radians($base);
    }

    multi method sec($base = 'radians') {
        pir::sec__Nn(self!to-radians($base));
    }

    multi method asec($base = 'radians') {
        pir::asec__Nn(self)!from-radians($base);
    }

    multi method sinh($base = 'radians') {
        pir::sinh__Nn(self!to-radians($base));
    }

    multi method asinh($base = 'radians') {
        (self + (self * self + 1).sqrt).log!from-radians($base);
    }

    multi method cosh($base = 'radians') {
        pir::cosh__Nn(self!to-radians($base));
    }

    multi method acosh($base = 'radians') {
        (self + (self * self - 1).sqrt).log!from-radians($base);
    }

    multi method tanh($base = 'radians') {
        pir::tanh__Nn(self!to-radians($base));
    }

    multi method atanh($base = 'radians') {
        (((1 + self) / (1 - self)).log / 2).from-radians($base);
    }

    multi method sech($base = 'radians') {
        pir::sech__Nn(self!to-radians($base));
    }

    multi method asech($base = 'radians') {
        (1 / self).acosh($base);
    }

    multi method cosech($base = 'radians') {
        1 / self.cosh($base);
    }

    multi method acosech($base = 'radians') {
        (1 / self).asinh($base);
    }

    multi method cosec($base = 'radians') {
        1 / self.sin($base);
    }

    multi method cotan($base = 'radians') {
        1 / self.tan($base);
    }

    multi method cotanh($base = 'radians') {
        1 / self.tanh($base);
    }

    multi method atan2(Num $x = 1, $base = 'radians') {
        pir::atan__NNn(self, $x)!from-radians($base);
    }

    our ::Complex multi method unpolar($angle) is export {
        Complex.new(self * $angle.cos("radians"), self * $angle.sin("radians"));
    }
}

# vim: ft=perl6
