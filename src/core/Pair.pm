my class Pair is Enum {
    # method value() is rw { ... }
}

sub infix:«=>»($key, Mu $value) { 
    Pair.new(:key($key), :value($value))
}

