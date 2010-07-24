augment class Block {
    # in src/builtins/Block.pir:  .arity, .count

    method arity() {
        self.signature.arity;
    }

    method count() {
        self.signature.count;
    }

    method ACCEPTS(Mu $topic) {
        self.count == 0 ?? self.() !! self.($topic);
    }

}

# vim: ft=perl6
