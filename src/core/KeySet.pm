my class KeySet does Setty {

    submethod BUILD (:%elems)  {
        nqp::bindattr(self, KeySet, '%!elems', %elems);
    }

    method at_key($k --> Bool) {
        Proxy.new(
          FETCH => {
              so nqp::getattr(self, KeySet, '%!elems').exists($k.WHICH);
          },
          STORE => -> $, $value {
              if $value {
                  nqp::getattr(self, KeySet, '%!elems'){$k.WHICH} = $k;
              }
              else {
                  nqp::getattr(self, KeySet, '%!elems').delete($k.WHICH);
              }
              so $value;
          });
    }

    method delete($k --> Bool) {
        my $elems := nqp::getattr(self, KeySet, '%!elems');
        my $key   := $k.WHICH;
        return False unless $elems.exists($key);

        $elems.delete($key);
        True;
    }

    method Set { Set.new(self.keys) }
    method KeySet { self }
}
