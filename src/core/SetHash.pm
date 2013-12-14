my class SetHash does Setty {

    method at_key($k --> Bool) {
        Proxy.new(
          FETCH => {
              so %!elems.exists_key($k.WHICH);
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

    method delete($k) {  # is DEPRECATED doesn't work in settings
        DEPRECATED("the :delete adverb");
        self.delete_key($k);
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
