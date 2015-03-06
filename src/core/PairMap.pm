my class PairMap is Hash {

    method BUILD(@pairs) {
        my Mu $storage    := nqp::hash();
        my Mu $descriptor := nqp::getattr(self,Hash,'$!descriptor');
        for @pairs -> $pair {
            nqp::bindkey($storage,nqp::unbox_s($pair.key.Str),
              nqp::p6scalarfromdesc($descriptor) = $pair.value)
        }

        nqp::bindattr(self, EnumMap, '$!storage', $storage);
        self;
    }

    multi method new(PairMap:U: *@params) {
        nqp::create(self).BUILD(@params.pairup);
    }

    multi method at_key(PairMap:D: \key) is rw {
        self.exists_key(key)
          ?? nextsame()
          !! fail X::PairMap::DoesNotExist.new(:key(key), :method<at_key>);
    }
    multi method bind_key(PairMap:D: \key) is rw {
        self.exists_key(key)
          ?? nextsame()
          !! fail X::PairMap::DoesNotExist.new(:key(key), :method<bind_key>);
    }
    multi method assign_key(PairMap:D: \key, Mu \assignval) {
        self.exists_key(key)
          ?? nextsame()
          !! fail X::PairMap::DoesNotExist.new(:key(key), :method<assign_key>);
    }
    multi method delete_key(PairMap:D: \key) {
        fail X::PairMap::NotAllowed.new(:method<delete_key>);
    }
    multi method push(PairMap:D: *@values) {
        for @values.pairup -> $pair {
            my $key   := $pair.key;
            my $value := self.at_key($key);
            nqp::istype($value,Array)
              ?? $value.push($pair.value)
              !! self.assign_key($key, [ $value, $pair.value ]);
        }
    }

    multi method perl(PairMap:D:) {
        self.^name
          ~ '.new('
          ~ self.pairs.pick(*).map({.perl}).join(', ')
          ~ ')';
    }
}

# vim: ft=perl6 expandtab sw=4
