my class MixHash does Mixy {

    multi method AT-KEY(MixHash:D: $k) {
        Proxy.new(
          FETCH => {
              my $key := $k.WHICH;
              %!elems.EXISTS-KEY($key) ?? %!elems{$key}.value !! 0;
          },
          STORE => -> $, $value {
              if $value != 0 {
                  (%!elems{$k.WHICH} //= ($k => 0)).value = $value;
              }
              else {
                  self.DELETE-KEY($k);
              }
              $value;
          }
        );
    }

    method DELETE-KEY($k) {
        my $key   := $k.WHICH;
        if %!elems.EXISTS-KEY($key) {
            my $value = %!elems{$key}.value;
            %!elems.DELETE-KEY($key);
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
            Mix.new-from-pairs(%!elems.values);
        }
    }
    method MixHash { self }
    method Bag     { Bag.new-from-pairs(%!elems.values) }
    method BagHash { BagHash.new-from-pairs(%!elems.values) }
}

# vim: ft=perl6 expandtab sw=4
