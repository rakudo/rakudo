my class MixHash does Mixy {

#--- interface methods
    multi method WHICH(MixHash:D:) { self.Mu::WHICH }
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
        nqp::p6bindattrinvres(
          nqp::create(Mix),Mix,'%!elems',
          $view ?? %!elems !! %!elems.clone
        )
    }
    method MixHash { self }
    method Bag     {     Bag.new-from-pairs(%!elems.values.grep(*.value > 0).map({.key => .value.Int})) }
    method BagHash { BagHash.new-from-pairs(%!elems.values.grep(*.value > 0).map({.key => .value.Int})) }
}

# vim: ft=perl6 expandtab sw=4
