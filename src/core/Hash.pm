role Hash is EnumMap {
    method postcircumfix:<{ }>($key) {
        Q:PIR {
            .local pmc self
            self = find_lex 'self'
            $P0 = getattribute self, '$!storage'
            $P1 = find_lex '$key'
            %r = $P0[$P1]
            unless null %r goto done
            %r = new ['Proxy']
            setattribute %r, '$!base', $P0
            setattribute %r, '$!key', $P1
            $P2 = get_hll_global ['Bool'], 'True'
            setprop %r, 'rw', $P2
          done:
        }
    }

    method !STORE(\$to_store) {
        # We create a new storage hash, in case we are referenced in
        # what is being stored.
        pir::setattribute__vPsP(self, '$!storage', pir::new__Ps('Hash'));
        
        # Work through the list, storing the things in it.
        my $need_value = 0;
        my $key;
        for list($to_store) -> $cur {
            if $need_value {
                self{$key} = $cur;
                $need_value = 0;
            }
            else {
                given $cur {
                    when Enum {
                        self{$cur.key} = $cur.value;
                    }
                    when EnumMap {
                        for $cur.iterator -> $pair {
                            self{$pair.key} = $pair.value;
                        }
                    }
                    default {
                        $key = $cur;
                        $need_value = 1;
                    }
                }
            }
        }
        if $need_value {
            die('Odd number of elements found where hash expected');
        }
        self
    }

    method delete(*@keys) {
        my @deleted;
        for @keys -> $k {
            @deleted.push(self{$k});
            Q:PIR {
                $P0 = find_lex '$k'
                $P1 = find_lex 'self'
                $P1 = getattribute $P1, '$!storage'
                delete $P1[$P0]
            }
        }
        @deleted
    }

    method push(*@values) {
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

    # push a value onto a hash Objectitem, constructing an array if necessary
    method !push_construct(Mu $key, Mu $value) {
        if self.exists($key) {
            if self.{$key} ~~ Array {
                self.{$key}.push($value);
            } else {
                self.{$key} = [ self.{$key}, $value];
            }
        } else {
            self.{$key} = $value;
        }
    }
}
