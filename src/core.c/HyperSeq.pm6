# A HyperSeq performs batches of work in parallel, but retains order of output
# values relative to input values.
#?if !js

my role Sequence::Hyper does Iterable does Sequence {
    has HyperConfiguration $.configuration;
    has Rakudo::Internals::HyperWorkStage $!work-stage-head;
    has $!joiner;

    submethod BUILD(:$!configuration!, :$!work-stage-head!) {}

    method iterator(::?CLASS:D: --> Iterator) {
        X::Seq::Consumed.new(:kind(::?CLASS.^name)).throw
            if nqp::isconcrete($!joiner);
        $!joiner := Rakudo::Internals::HyperToIterator.new:
            source => $!work-stage-head;
        Rakudo::Internals::HyperPipeline.start($!joiner, $!configuration);
        $!joiner
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

my class HyperSeq does Sequence::Hyper {
    method hyper(HyperSeq:D:) { self }

    method race(HyperSeq:D:) {
        RaceSeq.new(
            :$!configuration,
            work-stage-head =>
                Rakudo::Internals::HyperIteratorBatcher.new(:$.iterator))
    }
}
#?endif
#?if js
my class HyperSeq is Seq {
}
#?endif

# vim: expandtab shiftwidth=4
