augment class List {
    method perl() {
        if !self.elems { return Nil.WHAT };
        # XXX: $_.perl and .perl don't work here yet :-(
        '(' ~ self.map({ $^a.perl }).join(', ') ~ ')';
    }
}

# vim: ft=perl6
