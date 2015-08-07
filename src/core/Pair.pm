my class Pair is Enum {
    method value() is rw { nqp::getattr(self, Enum, '$!value') }

    multi method ACCEPTS(Pair:D: %h) {
        $.value.ACCEPTS(%h{$.key});
    }
    multi method ACCEPTS(Pair:D: Mu $other) {
        $other."$.key"().Bool === $.value.Bool
    }
    multi method AT-KEY(Pair:D: $key) { $key eq nqp::getattr(self, Enum, '$!key')
      ?? nqp::getattr(self, Enum, '$!value')
      !! Any;
    }
}

sub infix:«=>»($key, Mu $value) {
    Pair.new(:$key, :$value)
}

sub pair($key,$value) { Pair.new(:$key,:$value) }

# vim: ft=perl6 expandtab sw=4
