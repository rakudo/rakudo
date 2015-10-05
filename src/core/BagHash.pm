my class BagHash does Baggy {

    method BUILD(%!elems) { self }

    method Bag(:$view) {
        if $view {
            my \bag = nqp::create(Bag);
            nqp::bindattr(bag,Bag,'%!elems',%!elems);
            bag
        }
        else {
           Bag.new-from-pairs(%!elems.values)
        }
    }
    method BagHash { self }
    method Mix     { Mix.new-from-pairs(%!elems.values) }
    method MixHash { MixHash.new-from-pairs(%!elems.values) }

    multi method AT-KEY(BagHash:D: \k) is raw {
        Proxy.new(
          FETCH => {
              my \v := %!elems.AT-KEY(k.WHICH);
              nqp::istype(v,Pair) ?? v.value !! 0;
          },
          STORE => -> $, $value is copy {
              if $value > 0 {
                  (%!elems.AT-KEY(k.WHICH) //=
                    ((k) => my Int $ = 0)).value = $value;
              }
              elsif $value == 0 {
                  %!elems.DELETE-KEY(k.WHICH);
              }
              else {
                  $value = 0;
              }
              $value;
          }
        );
    }
}

# vim: ft=perl6 expandtab sw=4
