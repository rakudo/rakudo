augment class Seq {
    method elems() { pir::set__IP(self!fill); }

    method Num() { self.elems; }

    method Int() { self.elems.Int; }

    method Str() {
        pir::join(' ', self!fill);
    }
}

# vim: ft=perl6
