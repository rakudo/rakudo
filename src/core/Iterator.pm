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

    multi method Str() {
        pir::join(' ', self.eager);
    }

    # TimToady suggests this should be on Cool,
    # but it makes more sense to me here.  Also
    # should support slices, but I don't know 
    # if that's implemented in ng1 yet.
    multi method postcircumfix:<[ ]>($a) {
        self.Seq[$a];
    }

    # CHEAT: this method, not in the spec, included
    # in hopes of simplifying lazy iterator development.
    method GrabAndSay($n) {
        for ^$n {
            self.get.say;
        }
    }
}
