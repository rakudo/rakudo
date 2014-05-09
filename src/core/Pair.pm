my class Pair is Enum {
    method key() { nqp::getattr(self, Enum, '$!key') }
    method value() is rw { nqp::getattr(self, Enum, '$!value') }

    multi method ACCEPTS(Pair:D: %h) {
        $.value.ACCEPTS(%h{$.key});
    }
    multi method ACCEPTS(Pair:D: Mu $other) {
        $other."$.key"().Bool === $.value.Bool
    }
}

sub infix:«=>»($key, Mu $value) { 
    Pair.new(:key($key), :value($value))
}

# vim: ft=perl6 expandtab sw=4
