# A Seq represents anything that can lazily produce a sequence of values. A
# Seq is born in a state where iterating it will consume the values. However,
# calling .list on a Seq will return a List that will lazily reify to the
# values in the Seq. The List is memoized, so that subsequent calls to .list
# will always return the same List (safe thanks to List being immutable). More
# than one call to .iterator throws an exception (and calling .list calls the
# .iterator method the first time also). The memoization can be avoided by
# asking very specifically for the Seq to be coerced to a List (.List), a
# Slip (.Slip) or an Array (.Array). The actual memoization functionality is
# factored out into a role, MemoizeAsPositional, which is used by the binder
# to identify types that, on failure to bind to an @-sigilled thing, can have
# .list called on them and expect memoization semantics. This not only makes
# it easy for HyperSeq to also have this functionality, but makes it available
# for other kinds of paradigm that show up in the future (beyond sequential
# and parallel) that also want to have this behavior.
my class X::Seq::Consumed { ... }
my class X::Seq::NotIndexable { ... }
my role PositionalBindFailover {
    has $!list;

    method list() {
        $!list.DEFINITE
            ?? $!list
            !! ($!list := List.from-iterator(self.iterator))
    }
}
my class Seq does Iterable does PositionalBindFailover {
    # The underlying iterator that iterating this sequence will work its
    # way through. Can only be obtained once.
    has Iterator $!iter;

    # The only valid way to create a Seq directly is by giving it the
    # iterator it will consume and maybe memoize.
    method new(Iterator:D $iter) {
        my $seq := self.CREATE;
        nqp::bindattr($seq, Seq, '$!iter', nqp::decont($iter));
        $seq
    }

    method iterator(Seq:D:) {
        my \iter = $!iter;
        X::Seq::Consumed.new.throw unless iter.DEFINITE;
        $!iter := Iterator;
        iter
    }

    method List() {
        List.from-iterator(self.iterator)
    }

    method Slip() {
        Slip.from-iterator(self.iterator)
    }

    method Array() {
        Array.from-iterator(self.iterator)
    }

    method sink() {
        self.iterator.sink-all;
        self
    }

    multi method AT-POS(Seq:D: $) {
        X::Seq::NotIndexable.new.throw
    }

    multi method EXISTS-POS(Seq:D: $) {
        X::Seq::NotIndexable.new.throw
    }

    multi method DELETE-POS(Seq:D: $) {
        X::Seq::NotIndexable.new.throw
    }

    # Lazy loops produce a Seq wrapping a loop iterator. We have a few
    # special cases of that.
    my class InfiniteLoopIter does SlippyIterator {
        has &!body;

        method new(&body) {
            my \iter = self.CREATE;
            nqp::bindattr(iter, self, '&!body', &body);
            iter
        }

        method pull-one() {
            my int $redo = 1;
            my $result;
            if $!slipping && ($result := self.slip-one()) !=:= IterationEnd {
                $result
            }
            else {
                nqp::while(
                    $redo,
                    nqp::stmts(
                        $redo = 0,
                        nqp::handle(
                            nqp::stmts(
                                ($result := &!body()),
                                nqp::if(
                                    nqp::istype($result, Slip),
                                    nqp::stmts(
                                        ($result := self.start-slip($result)),
                                        nqp::if(
                                            nqp::eqaddr($result, IterationEnd),
                                            ($redo = 1)
                                        ))
                                    )),
                            'NEXT', ($redo = 1),
                            'REDO', ($redo = 1),
                            'LAST', ($result := IterationEnd))),
                    :nohandler);
                $result
            }
        }

        method lazy() { True }
    }

    my class WhileLoopIter does SlippyIterator {
        has &!body;
        has &!cond;
        has int $!skip-cond;

        method new(&body, &cond, :$repeat) {
            my \iter = self.CREATE;
            nqp::bindattr(iter, self, '&!body', &body);
            nqp::bindattr(iter, self, '&!cond', &cond);
            nqp::bindattr_i(iter, self, '$!skip-cond', $repeat ?? 1 !! 0);
            iter
        }

        method pull-one() {
            my int $redo = 1;
            my $result;
            if $!slipping && ($result := self.slip-one()) !=:= IterationEnd {
                $result
            }
            else {
                if $!skip-cond || &!cond() {
                    $!skip-cond = 0;
                    nqp::while(
                        $redo,
                        nqp::stmts(
                            $redo = 0,
                            nqp::handle(
                                nqp::stmts(
                                    ($result := &!body()),
                                    nqp::if(
                                        nqp::istype($result, Slip),
                                        nqp::stmts(
                                            ($result := self.start-slip($result)),
                                            nqp::if(
                                                nqp::eqaddr($result, IterationEnd),
                                                ($redo = &!cond() ?? 1 !! 0)
                                            ))
                                        )),
                                'NEXT', ($redo = &!cond() ?? 1 !! 0),
                                'REDO', ($redo = 1),
                                'LAST', ($result := IterationEnd))),
                        :nohandler);
                    $result
                }
                else {
                    IterationEnd
                }
            }
        }

        method lazy() { False }
    }

    my class CStyleLoopIter does SlippyIterator {
        has &!body;
        has &!cond;
        has &!afterwards;
        has int $!first-time;

        method new(&body, &cond, &afterwards) {
            my \iter = self.CREATE;
            nqp::bindattr(iter, self, '&!body', &body);
            nqp::bindattr(iter, self, '&!cond', &cond);
            nqp::bindattr(iter, self, '&!afterwards', &afterwards);
            nqp::bindattr_i(iter, self, '$!first-time', 1);
            iter
        }

        method pull-one() {
            my int $redo = 1;
            my $result;
            if $!slipping && ($result := self.slip-one()) !=:= IterationEnd {
                $result
            }
            else {
                $!first-time
                    ?? ($!first-time = 0)
                    !! &!afterwards();
                if &!cond() {
                    nqp::while(
                        $redo,
                        nqp::stmts(
                            $redo = 0,
                            nqp::handle(
                                nqp::stmts(
                                    ($result := &!body()),
                                    nqp::if(
                                        nqp::istype($result, Slip),
                                        nqp::stmts(
                                            ($result := self.start-slip($result)),
                                            nqp::if(
                                                nqp::eqaddr($result, IterationEnd),
                                                nqp::stmts(
                                                    &!afterwards(),
                                                    ($redo = &!cond() ?? 1 !! 0))
                                            ))
                                        )),
                                'NEXT', nqp::stmts(
                                    &!afterwards(),
                                    ($redo = &!cond() ?? 1 !! 0)),
                                'REDO', ($redo = 1),
                                'LAST', ($result := IterationEnd))),
                        :nohandler);
                    $result
                }
                else {
                    IterationEnd
                }
            }
        }

        method lazy() { False }
    }

    proto method from-loop(|) { * }
    multi method from-loop(&body) {
        Seq.new(InfiniteLoopIter.new(&body))
    }
    multi method from-loop(&body, &cond, :$repeat) {
        Seq.new(WhileLoopIter.new(&body, &cond, :$repeat))
    }
    multi method from-loop(&body, &cond, &afterwards) {
        Seq.new(CStyleLoopIter.new(&body, &cond, &afterwards))
    }
}

# vim: ft=perl6 expandtab sw=4
