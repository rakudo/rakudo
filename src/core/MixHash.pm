my class MixHash does Mixy {

    method at_key($k) {
        Proxy.new(
          FETCH => {
              my $key   := $k.WHICH;
              %!elems.exists_key($key) ?? %!elems{$key}.value !! 0;
          },
          STORE => -> $, $value {
              if $value != 0 {
                  (%!elems{$k.WHICH} //= ($k => 0)).value = $value;
              }
              else {
                  self.delete_key($k);
              }
              $value;
          }
        );
    }

    method delete($k) {  # is DEPRECATED doesn't work in settings
        DEPRECATED("the :delete adverb");
        self.delete_key($k);
    }
    method delete_key($k) {
        my $key   := $k.WHICH;
        if %!elems.exists_key($key) {
            my $value = %!elems{$key}.value;
            %!elems.delete_key($key);
            $value;
        }
        else {
            0;
        }
    }

    method Mix (:$view) {
        if $view {
            my $mix := nqp::create(Mix);
            $mix.BUILD( :elems(%!elems) );
            $mix;
        }
        else {
            Mix.new-fp(%!elems.values);
        }
    }           
    method MixHash { self }
    method Bag     { Bag.new-fp(%!elems.values) }
    method BagHash { BagHash.new-fp(%!elems.values) }
}
