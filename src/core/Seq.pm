augment class Seq {
    method elems() { pir::set__IP(self!fill); }

    method Str() {
        pir::join(' ', self!fill);
    }
}

# vim: ft=perl6
