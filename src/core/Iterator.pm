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

    multi method sort(&by = &infix:<cmp>) {
        self.Seq.sort(&by);
    }
}
