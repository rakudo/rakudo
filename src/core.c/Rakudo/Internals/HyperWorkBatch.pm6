# A batch of work sent to a worker in a hyper or race operation. It is an
# Iterable, and iterates to the items in the batch. This is so that it can be
# easily processed in terms of (non-hyper) Iterable implementations.
my class Rakudo::Internals::HyperWorkBatch does Iterable {
    # The items in the batch.
    has IterationBuffer $.items;

    # Sequence number of the batch, starting from zero.
    has int $.sequence-number;

    # Is this the first batch that was produced at the last fork point or the
    # last batch that the fork point will produce?
    has Bool $.first;
    has Bool $.last;

    method !SET-SELF(\sequence-number, \items, \first, \last) {
        $!sequence-number = sequence-number;
        $!items := items;
        $!first = first.Bool;
        $!last  = last.Bool;
        self
    }
    method new(\sn,\it,\f,\l) { nqp::create(self)!SET-SELF(sn,it,f,l) }

    # Iterator for a HyperWorkBatch;
    my class HyperWorkBatchIterator does Iterator {
        has $!items;
        has int $!i;
        has int $!n;

        method !SET-SELF(\items) {
            $!items := items;
            $!i = -1;
            $!n = nqp::elems(items);
            self
        }
        method new(\items) { nqp::create(self)!SET-SELF(items) }

        method pull-one() {
            ++$!i < $!n
                ?? nqp::atpos($!items, $!i)
                !! IterationEnd
        }
    }

    method iterator(--> Iterator) {
        HyperWorkBatchIterator.new($!items)
    }

    method replace-with(IterationBuffer $ib --> Nil) {
        $!items := $ib;
    }
}

# vim: expandtab shiftwidth=4
