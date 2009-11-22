augment class List {
    method perl() {
        '(' ~ self.map({ $_.perl }).join(', ') ~ ')';
    }
}

# vim: ft=perl6
