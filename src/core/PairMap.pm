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

    multi method AT-KEY(PairMap:D: \key) is rw {
        self.EXISTS-KEY(key)
          ?? nextsame()
          !! fail X::PairMap::DoesNotExist.new(:key(key), :method<AT-KEY>);
    }
    multi method BIND-KEY(PairMap:D: \key) is rw {
        self.EXISTS-KEY(key)
          ?? nextsame()
          !! fail X::PairMap::DoesNotExist.new(:key(key), :method<BIND-KEY>);
    }
    multi method ASSIGN-KEY(PairMap:D: \key, Mu \assignval) {
        self.EXISTS-KEY(key)
          ?? nextsame()
          !! fail X::PairMap::DoesNotExist.new(:key(key), :method<ASSIGN-KEY>);
    }
    multi method DELETE-KEY(PairMap:D: \key) {
        fail X::PairMap::NotAllowed.new(:method<DELETE-KEY>);
    }
    multi method push(PairMap:D: *@values) {
        for @values.pairup -> $pair {
            my $key   := $pair.key;
            my $value := self.AT-KEY($key);
            nqp::istype($value,Array)
              ?? $value.push($pair.value)
              !! self.ASSIGN-KEY($key, [ $value, $pair.value ]);
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
