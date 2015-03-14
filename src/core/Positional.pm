my role Positional[::T = Mu] {
    method of() { T }

    proto method roll(|) { * }
    multi method roll(Positional:) {
        fail "Cannot .roll from infinite list" if self.infinite;
        my $elems = self.elems;
        $elems ?? self.AT-POS($elems.rand.floor) !! Nil;
    }
    multi method roll(Positional: $n is copy) {
        fail "Cannot .roll from infinite list" if self.infinite;
        my $elems = self.elems;
        return unless $elems;
        $n = Inf if nqp::istype($n, Whatever);
        return self.AT-POS($elems.rand.floor) if $n == 1;

        gather while $n > 0 {
            take self.roll;
            $n--;
        }
    }

}

# vim: ft=perl6 expandtab sw=4
