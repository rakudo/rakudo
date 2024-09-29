# Batches values sourced from an iterator, producing a work batch from them.
my class Rakudo::Internals::HyperIteratorBatcher does Rakudo::Internals::HyperBatcher {
    my constant NO_LOOKAHEAD = Mu.CREATE;
    has Iterator $!iterator;
    has $!lookahead;
    has int $!seq-num;

    submethod BUILD(Iterator :$iterator!) {
        $!iterator := $iterator;
        $!lookahead := NO_LOOKAHEAD;
    }

    method produce-batch(int $batch-size --> Rakudo::Internals::HyperWorkBatch) {
        my $items := nqp::create(IterationBuffer);

        nqp::unless(
          (my int $first = nqp::eqaddr($!lookahead,NO_LOOKAHEAD)),
          nqp::push($items,$!lookahead)        # not first, get from previous
        );

        nqp::unless(
          (my int $last = nqp::eqaddr(
            $!iterator.push-exactly(
              $items,
              nqp::sub_i($batch-size,nqp::not_i($first))
            ),
            IterationEnd
          )),
          ($last = nqp::eqaddr(                # not last batch
            ($!lookahead := $!iterator.pull-one),
            IterationEnd                       # but is in this case
          ))
        );
        Rakudo::Internals::HyperWorkBatch.new($!seq-num++,$items,$first,$last)
    }
}

# vim: expandtab shiftwidth=4
