augment class Seq {
    multi method eager() { self.iterator.eager; }
    
    method elems() { pir::set__IP(self!fill); }

    method Num() { self.elems; }

    method Int() { self.elems.Int; }

    method Str() {
        pir::join(' ', self!fill);
    }
}

# vim: ft=perl6
