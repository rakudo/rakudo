class EnumMap is Iterable does Associative {
    has $!storage;

    method new(*%values) {
        self.bless(*, storage => pir::getattribute__PPs(%values, '$!storage'));
    }

    method at_key($key) {
        Q:PIR {
            .local pmc self
            self = find_lex 'self'
            $P0 = getattribute self, '$!storage'
            $P1 = find_lex '$key'
            %r = $P0[$P1]
            unless null %r goto done
            %r = new ['Perl6Scalar']
          done:
        }
    }

    multi method ACCEPTS(Regex $topic) {
        for @.keys -> $k {
            if $topic.ACCEPTS($k) {
                return True;
            }
        }
        False
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

    method elems() {
        pir::elements__IP($!storage)
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
        # shorter:  @.pairs.map( { ; .value X=> .key } ).flat;
        gather {
            for @.pairs {
                for @( .value ) -> $i {
                    take ($i => .key)
                }
            }
        }
    }

    method iterator() { self.pairs.iterator }

    method keys() {
        self.pairs.map({ $^pair.key })
    }

    method kv() { 
        self.pairs.map({ $^pair.key, $^pair.value }).flat 
    }

    method list() { self.pairs }

    method pairs() {
        gather {
            my $iter = pir::iter__PP($!storage);
            while pir::istrue__IP($iter) {
                my $iter_item = pir::shift__PP($iter);
                take Pair.new(key => $iter_item.key, value => $iter_item.value);
            }
        }
    }

    method perl() {
        '{' ~ self.pairs.map({ .perl }).join(", ") ~ '}';
    }

    method reverse() {
        my %result;
        for self.pairs() -> $p {
            %result{$p.value} = $p.key;
        }
        %result
    }

    method values() {
        self.pairs.map({ $^pair.value })
    }

    method Num() {
        pir::set__Ni(pir::elements($!storage));
    }

    method Int() {
        pir::elements($!storage)
    }

    method Capture() {
        Q:PIR {
            $P0 = get_hll_global 'Capture'
            $P1 = find_lex 'self'
            $P1 = getattribute $P1, '$!storage'
            %r = $P0.'new'($P1 :flat :named)
        }
    }

    method Str() {
        self.pairs.map({ .Str ~ "\n" }).join();
    }
}
