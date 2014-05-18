my class BagHash does Baggy {

    method at_key($k) {
        Proxy.new(
          FETCH => {
              my $key   := $k.WHICH;
              %!elems.exists_key($key) ?? %!elems{$key}.value !! 0;
          },
          STORE => -> $, $value is copy {
              if $value > 0 {
                  (%!elems{$k.WHICH} //= ($k => 0)).value = $value;
              }
              elsif $value == 0 {
                  self.delete_key($k);
              }
              else {
                  $value = 0;
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

    method Bag (:$view) {
        if $view {
            my $bag := nqp::create(Bag);
            $bag.BUILD( :elems(%!elems) );
            $bag;
        }
        else {
            Bag.new-fp(%!elems.values);
        }
    }           
    method BagHash { self }
    method Mix     { Mix.new-fp(%!elems.values) }
    method MixHash { MixHash.new-fp(%!elems.values) }
}

# vim: ft=perl6 expandtab sw=4
