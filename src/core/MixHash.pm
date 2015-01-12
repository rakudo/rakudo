my class MixHash does Mixy {

    multi method at_key(MixHash:D: $k) {
        Proxy.new(
          FETCH => {
              my $key := $k.WHICH;
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
            Mix.new-from-pairs(%!elems.values);
        }
    }
    method MixHash { self }
    method Bag     { Bag.new-from-pairs(%!elems.values) }
    method BagHash { BagHash.new-from-pairs(%!elems.values) }
}

# vim: ft=perl6 expandtab sw=4
