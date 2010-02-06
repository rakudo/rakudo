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
                    # when EnumMap { ... }
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
}
