my class Pair is Enum does Associative {
    # method value() is rw { ... }
    method hash {
        { self.key => self.value }
    }
}

sub infix:«=>»($key, Mu $value) { 
    Pair.new(:key($key), :value($value))
}
