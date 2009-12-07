augment class List {
    method perl() {
        # '(' ~ self.map({ $_.perl }).join(', ') ~ ')';
        my $result = "(";
        for self -> $x {
            $result ~= ", " if $result.chars > 1;
            $result ~= $x.perl;
        }
        return $result ~ ")";
    }
}

# vim: ft=perl6
