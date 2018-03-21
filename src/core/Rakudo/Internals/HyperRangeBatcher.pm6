# Batches values sourced from a Range, producing a work batch from them.
my class Rakudo::Internals::HyperRangeBatcher does Rakudo::Internals::HyperBatcher {
    has int $!first;    # 1 = first batch
    has int $!this;     # next value to produce - 1
    has int $!end;      # last value to produce
    has int $!seq-num;  # batch sequence number
   
    method new(\r) {
        nqp::if(
          nqp::istype(nqp::getattr(r,Range,'$!min'),Int)
            && (nqp::istype(nqp::getattr(r,Range,'$!max'),Int)
                 || nqp::getattr(r,Range,'$!max') == Inf),
          nqp::create(self).SET-SELF(r),         # yes, can use fast path
          Rakudo::Internals::HyperIteratorBatcher.new(
            iterator => r.iterator)              # no, use slower iterator path
        )
    }

    method SET-SELF(\range) {
        nqp::stmts(
          ($!first = 1),
          ($!this = nqp::add_i(
            nqp::sub_i(nqp::getattr(range,Range,'$!min'),1),
            nqp::getattr_i(range,Range,'$!excludes-min')
          )),
          ($!end = nqp::if(
            nqp::getattr(range,Range,'$!max') == Inf,
            int.Range.max,
            nqp::sub_i(
              nqp::getattr(range,Range,'$!max'),
              nqp::getattr_i(range,Range,'$!excludes-max')
            )
          )),
          self
        )
    }

    method produce-batch(int $batch-size --> Rakudo::Internals::HyperWorkBatch) {
        nqp::stmts(
          (my $items := nqp::create(IterationBuffer)),
          (my int $i = $!this),
          (my int $end = $!end),
          (my int $todo = nqp::add_i($batch-size,1)),
          nqp::while(
            ($todo = nqp::sub_i($todo,1))                   # done completely?
              && nqp::isle_i(($i = nqp::add_i($i,1)),$end), # just this batch?
            # need to decont($i) to make it a rvalue, otherwise aliases
            nqp::push($items,nqp::decont($i))
          ),
          ($!this = $i),
          (my $first = $!first),
          ($!first = 0),
          Rakudo::Internals::HyperWorkBatch.new($!seq-num++,$items,$first,$todo)
        )
    }
}

# vim: ft=perl6 expandtab sw=4
