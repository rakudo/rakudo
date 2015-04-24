my class SetHash does Setty {

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

    multi method AT-KEY(SetHash:D: \k --> Bool) is rw {
        Proxy.new(
          FETCH => {
              %!elems.EXISTS-KEY(k.WHICH);
          },
          STORE => -> $, $value {
              $value
                ?? %!elems.ASSIGN-KEY(k.WHICH,k)
                !! %!elems.DELETE-KEY(k.WHICH);
              so $value;
          });
    }
    multi method DELETE-KEY(SetHash:D: \k --> Bool) {
        my $key := k.WHICH;
        return False unless %!elems.EXISTS-KEY($key);

        %!elems.DELETE-KEY($key);
        True;
    }
}

# vim: ft=perl6 expandtab sw=4
