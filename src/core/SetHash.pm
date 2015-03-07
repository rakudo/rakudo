my class SetHash does Setty {

    multi method AT-KEY(SetHash:D: $k --> Bool) {
        Proxy.new(
          FETCH => {
              %!elems.EXISTS-KEY($k.WHICH);
          },
          STORE => -> $, $value {
              if $value {
                  %!elems{$k.WHICH} = $k;
              }
              else {
                  %!elems.DELETE-KEY($k.WHICH);
              }
              so $value;
          });
    }

    method DELETE-KEY($k --> Bool) {
        my $key   := $k.WHICH;
        return False unless %!elems.EXISTS-KEY($key);

        %!elems.DELETE-KEY($key);
        True;
    }

    method Set (:$view) {
        if $view {
            my $set := nqp::create(Set);
            $set.BUILD( :elems(%!elems) );
            $set;
        }
        else {
            Set.new(self.keys);
        }
    }

    method SetHash { self }
}

# vim: ft=perl6 expandtab sw=4
