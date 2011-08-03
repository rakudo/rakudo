my class Pair is Enum does Associative {
    method key() is rw { nqp::getattr(self, Enum, '$!key') }
    method value() is rw { nqp::getattr(self, Enum, '$!value') }

    multi method ACCEPTS(Pair:D: %h) {
        $.value.ACCEPTS(%h{%.key});
    }
    multi method ACCEPTS(Pair:D: Mu $other) {
        $other."$.key"().Bool === $.value.Bool
    }
}

sub infix:«=>»($key, Mu $value) { 
    Pair.new(:key($key), :value($value))
}

multi infix:<cmp>(Pair \$a, Pair \$b) {
    ($a.key cmp $b.key) || ($a.value cmp $b.value)
}
