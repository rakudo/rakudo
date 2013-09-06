my class KeyBag does Baggy {

    submethod BUILD (:%elems)  {
        nqp::bindattr(self, KeyBag, '%!elems', %elems);
    }

    method at_key($k) {
        Proxy.new(
          FETCH => {
              my $key   := $k.WHICH;
              my $elems := nqp::getattr(self, KeyBag, '%!elems');
              $elems.exists($key) ?? $elems{$key}.value !! 0;
          },
          STORE => -> $, $value {
              if $value > 0 {
                  (nqp::getattr(self, KeyBag, '%!elems'){$k.WHICH}
                    //= ($k => 0)).value = $value;
              }
              elsif $value == 0 {
                  self.delete($k);
              }
              else {
                  fail "Cannot put negative value $value for $k in {self.^name}";
              }
              $value;
          }
        );
    }

    method delete($k) {
        my $key   := $k.WHICH;
        my $elems := nqp::getattr(self, KeyBag, '%!elems');
        if $elems.exists($key) {
            my $value = $elems{$key}.value;
            $elems.delete($key);
            $value;
        }
        else {
            0;
        }
    }

    method Bag { Bag.new-fp(nqp::getattr(self, KeyBag, '%!elems').values) }
    method KeyBag { self }
}
