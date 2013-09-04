my class KeySet is Set {

    method delete($k) {
        my $elems := nqp::getattr(self, Set, '%!elems');
        my $key   := $k.WHICH;
        return False unless nqp::existskey($elems, nqp::unbox_s($key));

        $elems.delete($key);
        True;
    }

    method at_key($k) {
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

    multi method gist(Any:D $ : --> Str) { self.^name ~ ".new({ nqp::getattr(self, Set, '%!elems').values».gist.join(', ') })" }
    multi method perl(Any:D $ : --> Str) { self.^name ~ '.new(' ~ join(', ', map { .perl }, nqp::getattr(self, Set, '%!elems').values) ~ ')' }
}
