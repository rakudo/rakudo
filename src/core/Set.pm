my class Set does Setty {
    has $!WHICH;

    submethod WHICH { $!WHICH }
    submethod BUILD (:%elems) {
        my @keys := %elems.keys.sort;
        $!WHICH = self.^name ~ '|' ~ @keys.sort;
        nqp::bindattr(self, Set, '%!elems', %elems);
    }

    method at_key($k --> Bool) {
        so nqp::getattr(self, Set, '%!elems').exists_key($k.WHICH);
    }

    method delete ($a --> Bool) {  # is DEPRECATED doesn't work in settings
        once DEPRECATED("Method 'Set.delete'","the :delete adverb");
        self.delete_key($a);
    }
    method delete_key($k --> Bool) is hidden_from_backtrace {
        X::Immutable.new( method => 'delete', typename => self.^name ).throw;
    }

    method Set { self }
    method SetHash { SetHash.new(self.keys) }
}

sub set(*@args --> Set) { Set.new(@args) }
# U+2205 EMPTY SET
#constant term:<<"\x2205">> = set();  #Cannot call ACCEPTS; no signatures match
