my class Pair is Enum does Associative {
    # method value() is rw { ... }

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
