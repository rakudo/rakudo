role Hash is EnumMap {
    method at_key($key) {
        my $z = Any!butWHENCE(
                    { pir::set__vQsP($!storage, $key, $z); }
                );
        Q:PIR {
            .local pmc self
            self = find_lex 'self'
            $P0 = getattribute self, '$!storage'
            $P1 = find_lex '$key'
            %r = $P0[$P1]
            unless null %r goto done
            %r = find_lex '$z'
          done:
        }
    }

    method !STORE(\$to_store) {
        # We create a new storage hash, in case we are referenced in
        # what is being stored.
        pir::setattribute__vPsP(self, '$!storage', pir::new__Ps('Hash'));

        my $items = $to_store.flat;
        while $items {
            given $items.shift {
                when Enum {
                    self{.key} = .value;
                }
                when EnumMap {
                    for $_.list { self{.key} = .value }
                }
                default {
                    die('Odd number of elements found where hash expected')
                        unless $items;
                    self{$_} = $items.shift;
                }
            }
        }
        self
    }

    method Bool() {
        ?pir::istrue__IP(pir::getattribute__PPs(self, '$!storage'));
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
        return |@deleted
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
        return %(self);
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

    method list() {
        return self.pairs;
    }

    method hash() {
        return self;
    }

    multi method sort(&by = &infix:<cmp>) {
        self.pairs.sort(&by)
    }

    multi method pick($num is copy = 1) {
        if ($num == 1) {
            my @weights = [\+] self.values;
            my $value = @weights[*-1].rand;
            return self.keys[0] if @weights[0] > $value;
            my ($l, $r) = (0, @weights.elems-1);
            my $middle = floor ($r + $l) / 2;
            while $middle > $l {
                if @weights[$middle] < $value {
                    $l = $middle;
                }
                else {
                     $r = $middle;
                }
                $middle = floor ($r + $l) / 2;
            }
            return self.keys[$r];
        }

        my %copyHash = @.pairs.grep({ .value != 0});
        gather {
            while $num > 0 && %copyHash {
                take my $picked = %copyHash.pick();
                unless --%copyHash{$picked} {
                    %copyHash.delete($picked);
                }
                $num--;
            }
        }
    }

    multi method pick(Whatever) {
        self.pick(Inf);
    }

    multi method roll($num is copy = 1) {
        if ($num == 1) {
            my @weights = [\+] self.values;
            my $value = @weights[*-1].rand;
            return self.keys[0] if @weights[0] > $value;
            my ($l, $r) = (0, @weights.elems-1);
            my $middle = floor ($r + $l) / 2;
            while $middle > $l {
                if @weights[$middle] < $value {
                    $l = $middle;
                }
                else {
                     $r = $middle;
                }
                $middle = floor ($r + $l) / 2;
            }
            return self.keys[$r];
        }

        gather {
            take self.roll() for ^$num;
        }
    }

    multi method roll(Whatever) {
        self.roll(Inf);
    }
}


