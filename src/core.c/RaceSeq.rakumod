# A RaceSeq performs batches of work in parallel, and will deliver the results
# in the order they are produced (so potentially disordering them relative to
# the input).

my class RaceSeq does ParallelSequence[Rakudo::Internals::RaceToIterator] {
    method hyper(RaceSeq:D:) {
        HyperSeq.new(
            :$!configuration,
            work-stage-head =>
                Rakudo::Internals::HyperIteratorBatcher.new(:$.iterator))
    }

    method race(RaceSeq:D:) { self }
}

# vim: expandtab shiftwidth=4
