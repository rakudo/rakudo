my class MixHash does Mixy {

    method at_key($k) {
        Proxy.new(
          FETCH => {
              my $key   := $k.WHICH;
              my $elems := nqp::getattr(self, MixHash, '%!elems');
              $elems.exists_key($key) ?? $elems{$key}.value !! 0;
          },
          STORE => -> $, $value {
              if $value != 0 {
                  (nqp::getattr(self, MixHash, '%!elems'){$k.WHICH}
                    //= ($k => 0)).value = $value;
              }
              else {
                  self.delete_key($k);
              }
              $value;
          }
        );
    }

    method delete($k) {  # is DEPRECATED doesn't work in settings
        once DEPRECATED("Method 'MixHash.delete'","the :delete adverb");
        self.delete_key($k);
    }
    method delete_key($k) {
        my $key   := $k.WHICH;
        my $elems := nqp::getattr(self, MixHash, '%!elems');
        if $elems.exists_key($key) {
            my $value = $elems{$key}.value;
            $elems.delete_key($key);
            $value;
        }
        else {
            0;
        }
    }

    method Mix (:$view) {
        if $view {
            my $mix := nqp::create(Mix);
            $mix.BUILD( :elems(nqp::getattr(self, MixHash, '%!elems')) );
            $mix;
        }
        else {
            Mix.new-fp(nqp::getattr(self, MixHash, '%!elems').values);
        }
    }           
    method MixHash { self }
}
