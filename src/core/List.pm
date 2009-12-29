augment class List {
    method Str() {
        pir::join(' ', self!generate);
    }
    method perl() {
        if self.elems() == 0 { return Nil.WHAT };
        # XXX: $_.perl and .perl don't work, but this does...
        '(' ~ self.map({ $^a.perl }).join(', ') ~ ')';
    }
}

# vim: ft=perl6
