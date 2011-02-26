augment class Parcel does Positional {
    method Str() { self.flat.Str }

    method elems() { self.flat.elems }

    # XXX Need these methods here to avoid ResizablePMCArray.sort and
    # ResizablePMCArray.join leaking in from Parrot (we inherit
    # multiply from RPA and Any, and RPA has to come first, so we
    # hit the "wrong" sort; this will go away in the 6model integration
    # refactors, when we'll has-a RPA in Parcel or some such...we sure
    # won't inherit from a PMC as that likely won't be implemented in
    # 6model anyway).
    method sort(&by = &infix:<cmp>) { self.list.sort(&by) }
    method join(Str $joiner = '') { self.list.join($joiner) }

    method at_pos($pos) { self.flat.[$pos] }

    multi method ACCEPTS($x) {
        self.elems == 0
            ?? !$x.defined || ($x ~~ Positional && $x == 0)
            !! self.Seq.ACCEPTS($x)
    }
}
