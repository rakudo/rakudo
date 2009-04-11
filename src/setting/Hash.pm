class Hash is also {
    multi method reverse () is export {
        gather { 
            for @.pairs {
                for @( .value ) -> $i {
                    take ($i => .key)
                }
            }
        }
    }

    multi method push (*@values) {
        my $previous;
        my $has_previous;
        for @values -> $e {
            if $has_previous {
                self!push_construct($previous, $e);
                $has_previous = 0;
            } elsif $e ~~ Pair {
                self!push_construct($e.key, $e.value);
            } else {
                $previous = $e;
                $has_previous = 1;
            }
        }
        if $has_previous {
            warn "Trailing item in Hash.push";
        }
    }

    # push a value onto a hash item, constructing an array if necessary
    method !push_construct (Object $key, Object $value) {
        if self.exists($key) {
            if self.{$key} ~~ Array {
                self.{$key}.push: $value;
            } else {
                self.{$key} = [ self.{$key}, $value];
            }
        } else {
            self.{$key} = $value;
        }
    }
}

multi reverse(%hash) {
    %hash.reverse;
}

# vim: ft=perl6
