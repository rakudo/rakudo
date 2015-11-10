# We use a sentinel value to mark the end of an iteration.
my constant IterationEnd = Mu.CREATE;

# The Iterator role defines the API for an iterator and provides simple
# fallback implementations for most of it, so any given iterator can pick
# and choose what bits it can implement better for performance and/or
# correctness reasons.
my role Iterator {
    # Pulls one value from the iterator. If there's nothing more to pull,
    # returns the constant IterationEnd. If you don't override any other
    # methods in this role, they'll all end up falling back to using this.
    method pull-one() { ... }

    # Has the iterator produce a certain number of values and push them into
    # the target. The only time the iterator may push less values than asked
    # for is when it reaches the end of the iteration. It may never push more
    # values than are requested. Iterators that can do something smarter than
    # the default implementation here should override this method. Should
    # return how many things were pushed. Note that if the iterator does any
    # side-effects as a result of producing values then up to $n of them will
    # occur; you must be sure this is desired. Returns the number of things
    # pushed, or IterationEnd if it reached the end of the iteration.
    method push-exactly($target, int $n) {
        my int $i = 0;
        my $pulled;
        my $no-sink;
        while $i < $n && !(IterationEnd =:= ($pulled := self.pull-one())) {
            $no-sink := $target.push($pulled); # we may not .sink $pulled here, since it can be a Seq
            $i = $i + 1;
        }
        $pulled =:= IterationEnd
            ?? IterationEnd
            !! $i
    }

    # Has the iteration push at least a certain number of values into the
    # target buffer. For iterators that do side-effects, this should always
    # be the same as push-exactly. Those that know they can safely work ahead
    # to achieve better throughput may do so. Returns the number of things
    # pushed, or IterationEnd if it reached the end of the iteration.
    method push-at-least($target, int $n) {
        self.push-exactly($target, $n)
    }

    # Has the iterator produce all of its values into the target. This is
    # mostly just for convenience/clarity; it calls push-at-least with a
    # very large value in a loop, but will probably only ever need to do
    # one call to it. Thus, overriding push-at-least or push-exactly is
    # sufficient; you needn't override this. Returns IterationEnd.
    method push-all($target) {
        # Size chosen for when int is 32-bit
        until self.push-at-least($target, 0x7FFFFFFF) =:= IterationEnd { }
        IterationEnd
    }

    # Pushes things until we hit a lazy iterator (one whose is-lazy method returns
    # True). The default works well for non-composite iterators (that is, those
    # that don't trigger the evaluation of other iterators): it looks at the
    # lazy property of itself, and if it's true, does nothing, otherwise it
    # calls push-all. If all values the iterator can produce are pushed, then
    # IterationEnd should be returned. Otherwise, return something else (Mu
    # will do fine).
    method push-until-lazy($target) {
        self.is-lazy
            ?? Mu
            !! self.push-all($target)
    }

    # Does not push anything but consumes the iterator to find out the number
    # items that were generated, and returns that number.  Intended to be used
    # in situations such as "foo".IO.lines.elems, where we're only interested
    # in the number of lines in the file, rather than the contents of the
    # lines.
    method count-only() {
        my int $i = 0;
        $i = $i + 1 until self.pull-one() =:= IterationEnd;
        $i
    }

    # Does not push anything, but tries to consume the iterator once to find
    # out if anything is there.  Intended to be used in situations such as
    # if "foo".IO.lines { , where we're only interested whether there is *any*
    # line in the file, rather than the content of the line.
    method bool-only() {
        !(self.pull-one() =:= IterationEnd)
    }

    # Consumes all of the values in the iterator for their side-effects only.
    # May be overridden by iterators to either warn about use of things in
    # sink context that should not be used that way, or to process things in
    # a more efficient way when we know we don't need the results.
    method sink-all() {
        until self.pull-one() =:= IterationEnd { }
        IterationEnd
    }

    # Whether the iterator is lazy (True if yes, False if no).
    method is-lazy() {
        False
    }
}

# vim: ft=perl6 expandtab sw=4
