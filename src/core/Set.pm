my class Set does Setty {
    has $!WHICH;

    submethod WHICH { $!WHICH }
    submethod BUILD (:%elems) {
        my @keys := %elems.keys.sort;
        $!WHICH = self.^name ~ '|' ~ @keys.sort;
        nqp::bindattr(self, Set, '%!elems', %elems);
    }

    method at_key($k --> Bool) {
        so nqp::getattr(self, Set, '%!elems').exists($k.WHICH);
    }

    method delete($k --> Bool) is hidden_from_backtrace {
        X::Immutable.new( method => 'delete', typename => self.^name ).throw;
    }

    method Set { self }
    method KeySet { KeySet.new(self.keys) }
}
