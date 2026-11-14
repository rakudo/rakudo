# A HyperSeq performs batches of work in parallel, but retains order of output
# values relative to input values.
my class HyperSeq does ParallelSequence[Rakudo::Internals::HyperToIterator] {
    method hyper(HyperSeq:D:) { self }

    method race(HyperSeq:D:) {
        RaceSeq.new(
            :$!configuration,
            work-stage-head =>
                Rakudo::Internals::HyperIteratorBatcher.new(:$.iterator))
    }
}

# vim: expandtab shiftwidth=4
