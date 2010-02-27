class EnumMap does Associative {
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

    multi method ACCEPTS(Regex $topic) {
        any(@.keys) ~~ $topic;
    }

    multi method ACCEPTS(%topic) {
        @.keys.sort eqv %topic.keys.sort;
    }

    multi method ACCEPTS(@topic) {
        self.contains(any(@topic))
    }

    multi method ACCEPTS($topic) {
        self.contains($topic)
    }

    method contains($key) {
        self.exists($key)
    }

    method exists($key) {
        # Wish we could do pir:: for keyed things. *sigh*
        ?(Q:PIR {
            $P0 = find_lex '$key'
            $P1 = find_lex 'self'
            $P1 = getattribute $P1, '$!storage'
            $I0 = exists $P1[$P0]
            %r = box $I0
        })
    }

    method fmt($format = "%s\t%s", $sep = "\n") {
        self.pairs.map({ .fmt($format) }).join($sep)
    }

    multi method invert () is export {
        list(gather {
            for @.pairs {
                for @( .value ) -> $i {
                    take ($i => .key)
                }
            }
        })
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

    method pairs() {
        self.iterator()
    }

    method perl() {
        return '{' ~ self.pairs.map({ .perl }).join(", ") ~ '}';
    }

    method reverse() {
        my %result;
        for self.pairs() -> $p {
            %result{$p.value} = $p.key;
        }
        %result
    }

    method values() {
        self.iterator.map({ $^pair.value })
    }

    method Num() {
        pir::box__PN(pir::set__NP($!storage))
    }

    method Int() {
        pir::box__PI(pir::set__IP($!storage))
    }

    method Capture() {
        Q:PIR {
            $P0 = get_hll_global 'Capture'
            $P1 = find_lex 'self'
            $P1 = getattribute $P1, '$!storage'
            %r = $P0.'new'($P1 :flat :named)
        }
    }
}
