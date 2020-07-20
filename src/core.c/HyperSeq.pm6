# A HyperSeq performs batches of work in parallel, but retains order of output
# values relative to input values.
#?if !js
my class HyperSeq does Iterable does Sequence {
    has HyperConfiguration $.configuration;
    has Rakudo::Internals::HyperWorkStage $!work-stage-head;

    submethod BUILD(:$!configuration!, :$!work-stage-head!) {}

    method iterator(HyperSeq:D: --> Iterator) {
        my $joiner := Rakudo::Internals::HyperToIterator.new:
            source => $!work-stage-head;
        Rakudo::Internals::HyperPipeline.start($joiner, $!configuration);
        $joiner
    }

    method grep(HyperSeq:D: $matcher, *%options) {
        Rakudo::Internals::HyperRaceSharedImpl.grep:
            self, $!work-stage-head, $matcher, %options
    }

    method map(HyperSeq:D: $matcher, *%options) {
        Rakudo::Internals::HyperRaceSharedImpl.map:
            self, $!work-stage-head, $matcher, %options
    }

    method invert(HyperSeq:D:) {
        Rakudo::Internals::HyperRaceSharedImpl.invert(self, $!work-stage-head)
    }

    method hyper(HyperSeq:D:) { self }

    method race(HyperSeq:D:) {
        RaceSeq.new(:$!configuration, :$!work-stage-head)
    }

    method is-lazy(--> False) { }

    multi method serial(HyperSeq:D:) { self.Seq }

    method sink(--> Nil) {
        Rakudo::Internals::HyperRaceSharedImpl.sink(self, $!work-stage-head)
    }
}
#?endif
#?if js
my class HyperSeq is Seq {
}
#?endif

# vim: expandtab shiftwidth=4
