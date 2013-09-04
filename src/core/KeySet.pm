my class KeySet is Set {

    method delete($k --> Bool) {
        my $elems := nqp::getattr(self, Set, '%!elems');
        my $key   := $k.WHICH;
        return False unless nqp::existskey($elems, nqp::unbox_s($key));

        $elems.delete($key);
        True;
    }

    method at_key($k --> Bool) {
        Proxy.new(
          FETCH => {
              so nqp::existskey(nqp::getattr(self, Set, '%!elems'), nqp::unbox_s($k.WHICH));
          },
          STORE => -> $, $value {
              if $value {
                  nqp::getattr(self, Set, '%!elems'){$k.WHICH} = $k;
              }
              else {
                  nqp::getattr(self, Set, '%!elems').delete($k.WHICH);
              }
              so $value;
          });
    }
}
