# ParallelSequence role implements common functionality of HyperSeq and RaceSeq classes.
my role ParallelSequence[::Joiner] does Iterable does Sequence {
    has HyperConfiguration $.configuration;
    has Rakudo::Internals::HyperWorkStage $!work-stage-head;
#?if moar
    has atomicint $!has-iterator;
#?endif
#?if !moar
    has $!has-iterator;
#?endif


    submethod BUILD(:$!configuration!, :$!work-stage-head!) {
        $!has-iterator = 0;
    }

    method iterator(::?CLASS:D: --> Iterator) {
        X::Seq::Consumed.new(:kind(::?CLASS)).throw
#?if moar
            if nqp::cas_i($!has-iterator, 0, 1);
#?endif
#?if !moar
            if nqp::cas($!has-iterator, 0, 1);
#?endif
        my $joiner := Joiner.new:
                        source => $!work-stage-head;
        Rakudo::Internals::HyperPipeline.start($joiner, $!configuration);
        $joiner
    }

    method grep(::?CLASS:D: $matcher, *%options) {
        Rakudo::Internals::HyperRaceSharedImpl.grep:
            self, $!work-stage-head, $matcher, %options
    }

    method map(::?CLASS:D: $matcher, *%options) {
        Rakudo::Internals::HyperRaceSharedImpl.map:
            self, $!work-stage-head, $matcher, %options
    }

    method invert(::?CLASS:D:) {
        Rakudo::Internals::HyperRaceSharedImpl.invert(self, $!work-stage-head)
    }

    method hyper(::?CLASS:D:) {...}
    method race(::?CLASS:D:) {...}

    method is-lazy(--> False) { }

    multi method serial(::?CLASS:D:) { self.Seq }

    method sink(--> Nil) {
        Rakudo::Internals::HyperRaceSharedImpl.sink(self, $!work-stage-head)
    }
}
