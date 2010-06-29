augment class Block {

    method arity() {
        my $arity = 0;
        for self.signature.params -> $p {
            $arity++ unless $p.slurpy || $p.optional;
        }
        $arity;
    }

    method count() {
        my $count = 0;
        for self.signature.params -> $p {
            return Inf if $p.slurpy;
            $count++;
        }
        $count;
    }

    method ACCEPTS(Mu $topic) {
        self.count == 0 ?? self.() !! self.($topic);
    }

}

# vim: ft=perl6
