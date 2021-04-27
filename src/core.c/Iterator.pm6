# The Iterator role defines the API for an iterator and provides simple
# fallback implementations for most of it, so any given iterator can pick
# and choose what bits it can implement better for performance and/or
# correctness reasons.
my role Iterator {
    # Pulls one value from the iterator. If there's nothing more to pull,
    # returns the constant IterationEnd. If you don't override any other
    # methods in this role, they'll all end up falling back to using this.
    method pull-one() { ... }

    # Skip one value from the iterator.  Should return a true-like value to
    # indicate the skip was successful.  Override this method if you can
    # make an iterator that has significantly less to do when skipping a
    # generated value.
    method skip-one() {
        nqp::not_i(nqp::eqaddr(self.pull-one,IterationEnd))
    }

    # Has the iterator produce a certain number of values and push them into
    # the target. The only time the iterator may push less values than asked
    # for is when it reaches the end of the iteration. It may never push more
    # values than are requested. Iterators that can do something smarter than
    # the default implementation here should override this method. Should
    # return how many things were pushed. Note that if the iterator does any
    # side-effects as a result of producing values then up to $n of them will
    # occur; you must be sure this is desired. Returns the number of things
    # pushed, or IterationEnd if it reached the end of the iteration.
    method push-exactly(\target, int $n) {
        my int $todo = nqp::add_i($n,1);

        nqp::until(  # doesn't sink
          nqp::not_i($todo = nqp::sub_i($todo,1))
            || nqp::eqaddr((my $pulled := self.pull-one),IterationEnd),
          target.push($pulled) # don't .sink $pulled here, it can be a Seq
        );

        nqp::eqaddr($pulled,IterationEnd) ?? IterationEnd !! $n
    }

    # Has the iteration push at least a certain number of values into the
    # target buffer. For iterators that do side-effects, this should always
    # be the same as push-exactly. Those that know they can safely work ahead
    # to achieve better throughput may do so. Returns the number of things
    # pushed, or IterationEnd if it reached the end of the iteration.
    method push-at-least(\target, int $n) {
        self.push-exactly(target, $n)
    }

    # Has the iterator produce all of its values into the target.  Typically
    # called in .STORE if the iterator is non-lazy.  Returns IterationEnd.
    method push-all(\target --> IterationEnd) {
        nqp::until( # we may not .sink $pulled here, since it can be a Seq
          nqp::eqaddr((my \pulled := self.pull-one),IterationEnd),
          target.push(pulled)
        )
    }

    # Pushes things until we hit a lazy iterator (one whose is-lazy method returns
    # True). The default works well for non-composite iterators (that is, those
    # that don't trigger the evaluation of other iterators): it looks at the
    # lazy property of itself, and if it's true, does nothing, otherwise it
    # calls push-all. If all values the iterator can produce are pushed, then
    # IterationEnd should be returned. Otherwise, return something else (Mu
    # will do fine).
    method push-until-lazy(\target) {
        nqp::unless(
          self.is-lazy,
          self.push-all(target)
        )
    }

    # Skip the given number of values.  Return true if succesful in
    # skipping that many values.
    method skip-at-least(int $toskip) {
        my int $left = $toskip;
        nqp::while(
          nqp::isge_i(($left = nqp::sub_i($left,1)),0) && self.skip-one,
          nqp::null
        );
        nqp::islt_i($left,0)
    }

    # Skip the given number of values produced before returning the next
    # pulled value.  Given 0 it is an expensive way to do .pull-one
    method skip-at-least-pull-one(int $toskip) {
        self.skip-at-least($toskip)
          ?? self.pull-one
          !! IterationEnd
    }

    # Consumes all of the values in the iterator for their side-effects only.
    # May be overridden by iterators to either warn about use of things in
    # sink context that should not be used that way, or to process things in
    # a more efficient way when we know we don't need the results.
    method sink-all(--> IterationEnd) {
        nqp::while(
          self.skip-one,
          nqp::null
        )
    }

    # Whether the iterator is lazy (True if yes, False if no).
    # If True, the iterator must *never* try to evaluate more than the
    # user absolutely asks for.  This has e.g. effect on the behaviour
    # on .STORE: a lazy iterator would not reify, a non-lazy would.
    method is-lazy(--> False) { }

    # Whether the iterator will produce values in a deterministic way (always
    # the same for a given data source).  This is True for most iterators,
    # but *not* true for iterators that typically return keys and/or values
    # from a hash.
    method is-deterministic(--> True) { }
}

# The PredictiveIterator role is a refinement of the Iterator role for those
# cases when the number of values to be generated (still) can be determined
# *without* actually generating those values.
my role PredictiveIterator does Iterator {
    # The "count-only" method in a PredictiveIterator class returns the number
    # of elements that the iterator would still be able to generate but
    # *without* actually generating any values.  This can e.g. be the case
    # when an iterator for all the characters in a string, of which the number
    # elements is already known and the number of values generated as well.
    method count-only(--> Int:D) { ... }

    # The "bool-only" method in a PredictiveIterator class returns a Bool
    # to indicate whether the generator is (still) able to generate at least
    # one value, *without* actually generating that value.
    method bool-only(--> Bool:D) { self.count-only.Bool }
}

# vim: expandtab shiftwidth=4
