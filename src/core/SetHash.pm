my class SetHash does Setty {

    method at_key($k --> Bool) {
        Proxy.new(
          FETCH => {
              so nqp::getattr(self, SetHash, '%!elems').exists_key($k.WHICH);
          },
          STORE => -> $, $value {
              if $value {
                  nqp::getattr(self, SetHash, '%!elems'){$k.WHICH} = $k;
              }
              else {
                  nqp::getattr(self, SetHash, '%!elems').delete_key($k.WHICH);
              }
              so $value;
          });
    }

    method delete($k) {  # is DEPRECATED doesn't work in settings
        once DEPRECATED("Method 'SetHash.delete'","the :delete adverb");
        self.delete_key($k);
    }
    method delete_key($k --> Bool) {
        my $elems := nqp::getattr(self, SetHash, '%!elems');
        my $key   := $k.WHICH;
        return False unless $elems.exists_key($key);

        $elems.delete_key($key);
        True;
    }

    method Set (:$view) {
        if $view {
            my $set := nqp::create(Set);
            $set.BUILD( :elems(nqp::getattr(self, SetHash, '%!elems')) );
            $set;
        }
        else {
            Set.new(self.keys);
        }
    }

    method SetHash { self }
}
