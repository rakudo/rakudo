# A HyperSeq performs batches of work in parallel, but retains order of output
# values relative to input values.
#?if !js
my class HyperSeq does ParallelSequence[Rakudo::Internals::HyperToIterator] {
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
