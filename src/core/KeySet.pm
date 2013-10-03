my class KeySet does Setty {

    method at_key($k --> Bool) {
        Proxy.new(
          FETCH => {
              so nqp::getattr(self, KeySet, '%!elems').exists_key($k.WHICH);
          },
          STORE => -> $, $value {
              if $value {
                  nqp::getattr(self, KeySet, '%!elems'){$k.WHICH} = $k;
              }
              else {
                  nqp::getattr(self, KeySet, '%!elems').delete_key($k.WHICH);
              }
              so $value;
          });
    }

    method delete($k) {  # is DEPRECATED doesn't work in settings
        once DEPRECATED("Method 'KeySet.delete'","the :delete adverb");
        self.delete_key($k);
    }
    method delete_key($k --> Bool) {
        my $elems := nqp::getattr(self, KeySet, '%!elems');
        my $key   := $k.WHICH;
        return False unless $elems.exists_key($key);

        $elems.delete_key($key);
        True;
    }

    method Set (:$view) {
        if $view {
            my $set := nqp::create(Set);
            $set.BUILD( :elems(nqp::getattr(self, KeySet, '%!elems')) );
            $set;
        }
        else {
            Set.new(self.keys);
        }
    }

    method KeySet { self }
}
