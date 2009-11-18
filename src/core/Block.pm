augment class Block {

=begin item arity

=end item
    method arity() {
        my $arity = 0;
        for self.signature.params -> $p {
            $arity++ unless $p.slurpy || $p.optional;
        }
        $arity
    }

=begin item count

=end item
    method count() {
        my $count = 0;
        for self.signature.params -> $p {
            $count++ unless $p.slurpy;
        }
        $count
    }

}

# vim: ft=perl6
