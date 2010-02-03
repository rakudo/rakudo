augment class Iterator {
    multi method elems() {
        my $elems = 0;
        while !(self.get ~~ EMPTY) {
            $elems++;
        }
        $elems;
    }

    multi method Seq() {
        my $seq = Seq.new;
        $seq = self;
        $seq;
    }

    multi method Str() {
        self.Seq.Str;
    }

    # TimToady suggests this should be on Cool,
    # but it makes more sense to me here.  Also
    # should support slices, but I don't know 
    # if that's implemented in ng1 yet.
    multi method postcircumfix:<[ ]>($a) {
        self.Seq[$a];
    }
}