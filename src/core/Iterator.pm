augment class Iterator {
    multi method perl(:$limit = 40) {
        # Doing this right is probably beyond mortal dwimmery,
        # and even in simple cases it needs the ability to
        # serialize code to have any hope of producing
        # output like (1, 1, * + * ... *)
        # rather than (1, 1, 2, 3, 5, {???} ... ???).
        # Try to do something useful for debugging, for now.
        my @a = self.batch($limit);
        my $and_more = '';
        $and_more = ', {???} ... ???' if @a == $limit;
        '(' ~ @a.map({ $^a.perl }).join(', ') ~ $and_more ~ ')';
    }

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
    multi method fmt($format='%s', $separator=' ') {
        self.Seq.fmt($format,$separator);
    }
}
