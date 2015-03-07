my class BagHash does Baggy {

    multi method AT-KEY(BagHash:D: $k) {
        Proxy.new(
          FETCH => {
              my $key   := $k.WHICH;
              %!elems.EXISTS-KEY($key) ?? %!elems{$key}.value !! 0;
          },
          STORE => -> $, $value is copy {
              if $value > 0 {
                  (%!elems{$k.WHICH} //= ($k => 0)).value = $value;
              }
              elsif $value == 0 {
                  self.DELETE-KEY($k);
              }
              else {
                  $value = 0;
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

    method Bag (:$view) {
        if $view {
            my $bag := nqp::create(Bag);
            $bag.BUILD( :elems(%!elems) );
            $bag;
        }
        else {
            Bag.new-from-pairs(%!elems.values);
        }
    }
    method BagHash { self }
    method Mix     { Mix.new-from-pairs(%!elems.values) }
    method MixHash { MixHash.new-from-pairs(%!elems.values) }
}

# vim: ft=perl6 expandtab sw=4
