my class SetHash does Setty {

    multi method at_key(SetHash:D: $k --> Bool) {
        Proxy.new(
          FETCH => {
              %!elems.exists_key($k.WHICH);
          },
          STORE => -> $, $value {
              if $value {
                  %!elems{$k.WHICH} = $k;
              }
              else {
                  %!elems.delete_key($k.WHICH);
              }
              so $value;
          });
    }

    method delete_key($k --> Bool) {
        my $key   := $k.WHICH;
        return False unless %!elems.exists_key($key);

        %!elems.delete_key($key);
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
