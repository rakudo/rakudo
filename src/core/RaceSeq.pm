# A RaceSeq performs batches of work in parallel, and will deliver the results
# in the order they are produced (so potentially disordering them relative to
# the input).
my class RaceSeq does Iterable {
    has HyperConfiguration $.configuration;
    has Rakudo::Internals::HyperWorkStage $!work-stage-head;

    submethod BUILD(:$!configuration!, :$!work-stage-head!) {}

    method iterator(RaceSeq:D: --> Iterator) {
        my $joiner := Rakudo::Internals::RaceToIterator.new:
            source => $!work-stage-head;
        Rakudo::Internals::HyperPipeline.start($joiner, $!configuration);
        $joiner
    }

    method grep(RaceSeq:D: $matcher, *%options) {
        Rakudo::Internals::HyperRaceSharedImpl.grep:
            self, $!work-stage-head, $matcher, %options
    }

    method map(RaceSeq:D: $matcher, *%options) {
        Rakudo::Internals::HyperRaceSharedImpl.map:
            self, $!work-stage-head, $matcher, %options
    }

    method race(RaceSeq:D:) { self }

    method sink(--> Nil) {
        Rakudo::Internals::HyperRaceSharedImpl.sink(self, $!work-stage-head)
    }
}

# vim: ft=perl6 expandtab sw=4
