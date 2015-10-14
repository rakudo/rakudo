my role Mixy does Baggy  {

    method !PAIR(\key,\value) { Pair.new(key, my Real $ = value ) }
    method !SANITY(%elems --> Nil) {
        for %elems -> $p {
            %elems.DELETE-KEY($p.key) if $p.value.value == 0;
        }
    }

    multi method kxxv(Mixy:D:) {
        fail ".kxxv is not supported on a {self.^name}";
    }

    multi method grab(Mixy:D: $count?) {
        fail ".grab is not supported on a {self.^name}";
    }

    multi method pick(Mixy:D: $count?) {
        fail ".pick is not supported on a {self.^name}";
    }

    multi method roll(Mixy:D:) {
        Rakudo::Internals::WeightedRoll.new(self).roll
    }
    multi method roll(Mixy:D: $count) {
        my $roller = Rakudo::Internals::WeightedRoll.new(self);
        map { $roller.roll }, 1 .. $count;
    }
}

# vim: ft=perl6 expandtab sw=4
