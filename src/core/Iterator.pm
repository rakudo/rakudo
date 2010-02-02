augment class Iterator {
    multi method elems() {
        my $elems = 0;
        while !(self.get ~~ EMPTY) {
            $elems++;
        }
        $elems;
    }
}