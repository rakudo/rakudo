# A HyperSeq performs batches of work in parallel, but retains order of output
# values relative to input values.
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

    method hyper(HyperSeq:D:) { self }

    method is-lazy() { False }

    method race(HyperSeq:D:) {
        RaceSeq.new(:$!configuration, :$!work-stage-head)
    }

    method sink(--> Nil) {
        Rakudo::Internals::HyperRaceSharedImpl.sink(self, $!work-stage-head)
    }
}

# vim: ft=perl6 expandtab sw=4
