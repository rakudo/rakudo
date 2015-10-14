my class BagHash does Baggy {

#--- interface methods
    multi method AT-KEY(BagHash:D: \k) is raw {
        Proxy.new(
          FETCH => {
              my $hash := nqp::getattr(%!elems,Map,'$!storage');
              my str $which = nqp::unbox_s(k.WHICH);
              nqp::existskey($hash,$which)
                ?? nqp::getattr(nqp::decont(nqp::atkey($hash,$which)),Pair,'$!value')
                !! 0
          },
          STORE => -> $, $value is copy {
              my $hash := nqp::getattr(%!elems,Map,'$!storage');
              my str $which = nqp::unbox_s(k.WHICH);
              if nqp::existskey($hash,$which) {
                  $value > 0
                    ?? (nqp::getattr(nqp::decont(nqp::atkey($hash,$which)),Pair,'$!value') = $value)
                    !! nqp::deletekey($hash,$which);
              }
              elsif $value > 0 {
                  nqp::bindkey($hash,$which,self!PAIR(k,$value));
              }
              $value < 0 ?? 0 !! $value;
          }
        );
    }

#--- introspection methods
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
}

# vim: ft=perl6 expandtab sw=4
