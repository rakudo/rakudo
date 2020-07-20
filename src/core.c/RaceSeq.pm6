# A RaceSeq performs batches of work in parallel, and will deliver the results
# in the order they are produced (so potentially disordering them relative to
# the input).

#?if !js
my class RaceSeq does Iterable does Sequence {
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

    method invert(RaceSeq:D:) {
        Rakudo::Internals::HyperRaceSharedImpl.invert:
            self, $!work-stage-head
    }

    method hyper(RaceSeq:D:) {
        HyperSeq.new(:$!configuration, :$!work-stage-head)
    }

    method race(RaceSeq:D:) { self }

    method is-lazy(--> False) { }

    multi method serial(RaceSeq:D:) { self.Seq }

    method sink(--> Nil) {
        Rakudo::Internals::HyperRaceSharedImpl.sink(self, $!work-stage-head)
    }
}
#?endif
#?if js
my class RaceSeq is Seq {
}
#?endif

# vim: expandtab shiftwidth=4
