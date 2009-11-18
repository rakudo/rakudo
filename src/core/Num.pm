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

    multi method exp() {
        pir::exp__Nn(self);
    }

    multi method log() {
        pir::ln__Nn(self);
    }

    multi method sqrt() {
        pir::sqrt__Nn(self);
    }

    multi method sin($base = 'radians') {
        pir::sin__Nn(self!to-radians($base));
    }
}

# vim: ft=perl6
