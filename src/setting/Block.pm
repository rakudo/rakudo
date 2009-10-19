class Block is also {

=begin item arity

=end item
    method arity() {
        if $.signature -> $sig {
            my $arity = 0;
            for $sig.params -> $p {
                $arity++ unless $p.slurpy || $p.optional;
            }
            $arity
        }
        else {
            Q:PIR {
                $P0 = find_lex 'self'
                $P0 = descalarref $P0
                $P1 = inspect $P0, "pos_required"
                $P2 = inspect $P0, "named_required"
                %r = $P1 + $P2
            };
        }
    }

=begin item count

=end item
    method count() {
        if $.signature -> $sig {
            my $count = 0;
            for $sig.params -> $p {
                $count++ unless $p.slurpy;
            }
            $count
        }
        else {
            Q:PIR {
                $P0 = find_lex 'self'
                $P0 = descalarref $P0
                $P1 = inspect $P0, "pos_required"
                $P2 = inspect $P0, "pos_optional"
                $P3 = $P1 + $P2
                $P1 = inspect $P0, "named_required"
                $P2 = inspect $P0, "named_optional"
                $P4 = $P1 + $P2
                %r = $P3 + $P4
            };
        }
    }

}

# vim: ft=perl6
