role EnumMap {
    has $!storage;

    method new(*%values) {
        self.bless(*, storage => pir::getattribute__PPs(%values, '$!storage'));
    }

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
          done:
        }
    }

    method iterator() {
        # We just work off the low-level Parrot iterator.
        my $iter = pir::iter__PP($!storage);
        gather {
            while pir::istrue__IP($iter) {
                my $iter_item = pir::shift__PP($iter);
                take Pair.new(key => $iter_item.key, value => $iter_item.value);
            }
        }
    }

    method keys() {
        self.iterator.map({ $^pair.key })
    }

    method kv() {
        gather {
            for self.iterator -> $pair {
                take $pair.key;
                take $pair.value;
            }
        }
    }

    method values() {
        self.iterator.map({ $^pair.value })
    }
}

