my class MixHash does Mixy {

    method BUILD(%!elems) { self }

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
                  nqp::bindkey($hash,$which,self.PAIR(k,$value));
              }
              $value;
          }
        );
    }

    my class WeightedRoll {
        has @!pairs;
        has $!total;

        method BUILD(\mix) {
            $!total = 0;
            for mix.pairs {
                my $value := .value;
                if $value > 0 {
                    @!pairs.push($_);
                    $!total = $!total + $value;
                }
            }
            self
        }
        method new(\mix) { nqp::create(self).BUILD(mix) }
        method roll() {
            my $rand = $!total.rand;
            my $seen = 0;
            return .key if ( $seen = $seen + .value ) > $rand for @!pairs;
        }
    }

    multi method roll(MixHash:D:) { WeightedRoll.new(self).roll }
    multi method roll(MixHash:D: $count) {
        my $roller = WeightedRoll.new(self);
        map { $roller.roll }, 1 .. $count;
    }
}

# vim: ft=perl6 expandtab sw=4
