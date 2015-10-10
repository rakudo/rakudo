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
              my \v := %!elems.AT-KEY(k.WHICH);
              nqp::istype(v,Pair) ?? v.value !! 0;
          },
          STORE => -> $, $value is copy {
              if $value != 0 {
                  (%!elems.AT-KEY(k.WHICH) //=
                    ((k) => my Real $ = 0)).value = $value;
              }
              else {
                  %!elems.DELETE-KEY(k.WHICH);
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
