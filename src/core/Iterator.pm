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
}