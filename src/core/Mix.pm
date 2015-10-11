my class Mix does Mixy {
    has Real $!total;
    has $!WHICH;

    method BUILD(%!elems) { .freeze for %!elems.values; self }

    multi method WHICH (Mix:D:) {
        $!WHICH //= self.^name
          ~ '|'
          ~ %!elems.keys.sort.map( { $_ ~ '(' ~ %!elems{$_}.value ~ ')' } );
    }

    method total (--> Real) { $!total //= [+] self.values }

    multi method grab($count? --> Real) {
        X::Immutable.new( method => 'grab', typename => self.^name ).throw;
    }
    multi method grabpairs($count? --> Real) {
        X::Immutable.new( method => 'grabpairs', typename => self.^name ).throw;
    }

    method Mix { self }
    method MixHash { MixHash.new-from-pairs(%!elems.values) }
    method Bag     {     Bag.new-from-pairs(%!elems.values) }
    method BagHash { BagHash.new-from-pairs(%!elems.values) }

    # identical to Bag.pm
    multi method AT-KEY(Mix:D: \k) {
        my $hash := nqp::getattr(%!elems,Map,'$!storage');
        my str $which = nqp::unbox_s(k.WHICH);
        nqp::existskey($hash,$which)
          ?? nqp::getattr(nqp::decont(nqp::atkey($hash,$which)),Pair,'$!value')
          !! 0
    }
    multi method ASSIGN-KEY(Mix:D: \k,\v) {
        X::Assignment::RO.new(typename => self.^name).throw;
    }
    multi method DELETE-KEY(Mix:D: \k) {
        X::Immutable.new(method => 'DELETE-KEY', typename => self.^name).throw;
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

    multi method roll(Mix:D:) { WeightedRoll.new(self).roll }
    multi method roll(Mix:D: $count) {
        my $roller = WeightedRoll.new(self);
        map { $roller.roll }, 1 .. $count;
    }
}

# vim: ft=perl6 expandtab sw=4
