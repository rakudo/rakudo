augment class Parcel does Positional {
    method Str() { self.flat.Str }

    method elems() { self.flat.elems }

    # Need this method here to avoid ResizablePMCArray.sort from Parrot.
    method sort(&by = &infix:<cmp>) { self.list.sort(&by) }

    multi method postcircumfix:<[ ]>($index) { self.flat.[$index] }
    multi method postcircumfix:<[ ]>(@index) { self.flat.[@index] }

    multi method ACCEPTS($x) {
        self.elems == 0
            ?? $x.notdef || ($x ~~ Positional && $x == 0)
            !! self.Seq.ACCEPTS($x)
    }
}
