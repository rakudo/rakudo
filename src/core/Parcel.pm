augment class Parcel {
    method elems() { self.Seq.elems }

    method list() { self.iterator }

    method rotate(Int $n = 1) { self.Seq.rotate($n) }

    multi method ACCEPTS($x) {
        # smart-matching against Nil
        if self.elems == 0 {
            $x.notdef || ($x.does(::Positional) && $x == 0)
        } else {
            self.Seq.ACCEPTS($x)
        }
    }
}
