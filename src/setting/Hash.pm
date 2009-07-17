class Hash is also {

    multi method ACCEPTS(Regex $topic) {
        any(@.keys) ~~ $topic;
    }

    multi method ACCEPTS(%topic) {
        @.keys.sort eqv %topic.keys.sort;
    }

    # the spec says Array, not Positional, so we can't use the @ sigil here
    multi method ACCEPTS(Array $topic) {
        # we can't simply write
        # $.contains(any(@($topic)))
        # because .contains doesn't autothread, so we have to do it manually:
        for $topic.list {
            return Bool::True if $.exists($_);
        }
        Bool::False;
    }

    multi method ACCEPTS($topic) {
        $.contains($topic)
    }

    multi method invert () is export {
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
