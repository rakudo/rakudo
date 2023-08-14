# A RaceSeq performs batches of work in parallel, and will deliver the results
# in the order they are produced (so potentially disordering them relative to
# the input).

#?if !js
my class RaceSeq does ParallelSequence[Rakudo::Internals::RaceToIterator] {
    method hyper(RaceSeq:D:) {
        HyperSeq.new(
            :$!configuration,
            work-stage-head =>
                Rakudo::Internals::HyperIteratorBatcher.new(:$.iterator))
    }

    method race(RaceSeq:D:) { self }
}
#?endif
#?if js
my class RaceSeq is Seq {
}
#?endif

# vim: expandtab shiftwidth=4
