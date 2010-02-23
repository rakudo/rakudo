augment class Seq {
    method elems() { pir::set__IP(self!fill); }

    method Str() {
        pir::join(' ', self!fill);
    }

    multi method sort(&by = pir::get_hll_global__Ps('&infix:<cmp>')) {
        # Parrot already provides a sort method that works on
        # ResizablePMCArray, so we aim to make use of that here.
        # Instead of sorting the elements directly, we sort an RPA
        # of indices, then use that RPA as a slice into self.

        # If &by.arity < 2, then it represents a block to be applied
        # to the elements to obtain the values for sorting.
        my ($list, &cmp);
        if (&by.?arity // 2) < 2 {
            $list = self.map(&by);
            &cmp  = pir::get_hll_global__Ps('&infix:<cmp>');
        }
        else {
            $list = self.eager;
            &cmp = &by;
        }

        # Now we sort the list.  We create a RPA (Parcel) list of
        # indices from 0 to $list.elems - 1, sort those using
        # RPA.sort and &cmp, and slice the result.  (If &cmp
        # indicates two elements are equal, we use the indices
        # for the comparison, producing a stable sort.)
        self[
            (^pir::elements($list)).eager.sort( 
                -> $a, $b { &cmp($list[$a], $list[$b]) || $a <=> $b }
            )
        ];
    }
}

# vim: ft=perl6
