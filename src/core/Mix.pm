my class Mix does Mixy {
    has $!WHICH;

    method at_key($k --> Real) {
        my $elems := nqp::getattr(self, Mix, '%!elems');
        my $key   := $k.WHICH;
        $elems.exists_key($key)
          ?? $elems{$key}.value
          !! 0;
    }

    method delete ($a --> Real) {  # is DEPRECATED doesn't work in settings
        once DEPRECATED("Method 'Mix.delete'","the :delete adverb");
        self.delete_key($a);
    }
    method delete_key($a --> Real) is hidden_from_backtrace {
        X::Immutable.new( method => 'delete', typename => self.^name ).throw;
    }

    method Mix { self }
    method MixHash { MixHash.new-fp(nqp::getattr(self, Mix, '%!elems').values) }
}
