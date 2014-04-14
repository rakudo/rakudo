my role QuantHash does Associative {
    method Int     ( --> Int)     { self.total.Int }
    method Num     ( --> Num)     { self.total.Num }
    method Numeric ( --> Numeric) { self.total.Numeric }
    method Real    ( --> Real)    { self.total.Real }

    method minpairs ( --> List ) {
        my @found;
        my $min = Inf;
        for self.pairs {
            my $value := .value;
            if $value > $min {
                next;
            }
            elsif $value < $min {
                @found = $_;
                $min = $value;
            }
            else {
                @found.push: $_;
            }
        }
        @found;
    }

    method maxpairs ( --> List ) {
        my @found;
        my $max = -Inf;
        for self.pairs {
            my $value := .value;
            if $value < $max {
                next;
            }
            elsif $value > $max {
                @found = $_;
                $max = $value;
            }
            else {
                @found.push: $_;
            }
        }
        @found;
    }
}

# vim: ft=perl6 expandtab sw=4
