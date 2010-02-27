augment class Iterator {
    multi method elems() {
        my $elems = 0;
        while !(self.get ~~ EMPTY) {
            $elems++;
        }
        $elems;
    }

    multi method Seq() {
        Seq.new!STORE(self);
    }

    # TimToady suggests this should be on Cool,
    # but it makes more sense to me here.  Also
    # should support slices, but I don't know 
    # if that's implemented in ng1 yet.
    multi method postcircumfix:<[ ]>($a) {
        self.Seq[$a];
    }

    multi method sort(&by = &infix:<cmp>) {
        self.Seq.sort(&by);
    }
}
