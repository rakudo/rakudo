my class SetHash does Setty {

    method ISINSET(\key) {
        Proxy.new(
          FETCH => { %!elems.EXISTS-KEY(key) },
          STORE => -> $, \value {
              %!elems.DELETE-KEY(key) unless value;
              value;
          }
        );
    }

    multi method kv(SetHash:D:) {
        %!elems.kv.map: -> \k,\v { |(v,self.ISINSET(k)) }
    }
    multi method values(SetHash:D:) {
        %!elems.keys.map: -> \key { self.ISINSET(key) }
    }
    multi method pairs(SetHash:D:) {
        %!elems.kv.map: -> \k,\v --> Pair { Pair.new(v,self.ISINSET(k)) }
    }
    multi method antipairs(SetHash:D:) {
        %!elems.values.map: -> \key --> Pair { Pair.new(True,key) }
    }

    method clone(SetHash:D:) { self.new-from-pairs(self.pairs) }

    method Set(SetHash:D: :$view) {
        nqp::p6bindattrinvres(
          nqp::create(Set),Set,'%!elems',
          $view ?? %!elems !! %!elems.clone
        )
    }
    method SetHash(SetHash:D:) { self }

    multi method AT-KEY(SetHash:D: \k --> Bool) is raw {
        Proxy.new(
          FETCH => {
              %!elems.EXISTS-KEY(k.WHICH);
          },
          STORE => -> $, $value {
              $value
                ?? %!elems.ASSIGN-KEY(k.WHICH,k)
                !! %!elems.DELETE-KEY(k.WHICH);
              so $value;
          });
    }
    multi method DELETE-KEY(SetHash:D: \k --> Bool) {
        nqp::if(
          %!elems.EXISTS-KEY(my $key := k.WHICH),
          nqp::stmts(
            %!elems.DELETE-KEY($key),
            True
          )
        )
    }
}

# vim: ft=perl6 expandtab sw=4
