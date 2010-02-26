augment class Seq {
    method elems() { pir::set__IP(self!fill); }

    method Str() {
        pir::join(' ', self!fill);
    }

    multi method sort(&by = &infix:<cmp>) {
        # Parrot already provides a sort method that works on
        # ResizablePMCArray, so we aim to make use of that here.
        # Instead of sorting the elements directly, we sort an RPA
        # of indices (from 0 to $list.elems), then use that RPA 
        # as a slice into self.

        # If &by.arity < 2, then it represents a block to be applied
        # to the elements to obtain the values for sorting.
        if (&by.?arity // 2) < 2 {
            my $list = self.map(&by).eager;
            self[(^pir::elements($list)).eager.sort(
                -> $a, $b { $list[$a] cmp $list[$b] || $a <=> $b }
            )];
        }
        else {
            my $list = self.eager;
            self[(^pir::elements($list)).eager.sort(
                -> $a, $b { &by($list[$a],$list[$b]) || $a <=> $b }
            )];
        }
    }
}

# vim: ft=perl6
