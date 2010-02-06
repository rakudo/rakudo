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

    
}
