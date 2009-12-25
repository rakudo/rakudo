augment class List {
    method perl() {
        if self.elems() == 0 { return Nil.WHAT };
        # '(' ~ self.map({ $_.perl }).join(', ') ~ ')';
        my $result = '(';
        for self -> $x {
            $result ~= ', ' if $result.chars > 1;
            $result ~= (~$x.WHAT eq 'Array()' ?? gather { for 0 ... $x.end { take $x.[$_] } } !! $x).perl;
        }
        return $result ~ ')';
    }
}

# vim: ft=perl6
