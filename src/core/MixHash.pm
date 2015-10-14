my class MixHash does Mixy {

#--- interface methods
    multi method AT-KEY(MixHash:D: \k) is raw {
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
                  $value == 0
                    ?? nqp::deletekey($hash,$which)
                    !! (nqp::getattr(nqp::decont(nqp::atkey($hash,$which)),Pair,'$!value') = $value);
              }
              elsif $value {
                  nqp::bindkey($hash,$which,self!PAIR(k,$value));
              }
              $value;
          }
        );
    }

#--- coercion methods
    method Mix(:$view) {
        if $view {
            my \mix = nqp::create(Mix);
            nqp::bindattr(mix,Mix,'%!elems',%!elems);
            mix
        }
        else {
            Mix.new-from-pairs(%!elems.values)
        }
    }
    method MixHash { self }
    method Bag     { Bag.new-from-pairs(%!elems.values) }
    method BagHash { BagHash.new-from-pairs(%!elems.values) }
}

# vim: ft=perl6 expandtab sw=4
