# A Slip is a kind of List that is immediately incorporated into an iteration
# or another List. Other than that, it's a totally normal List.
my class Slip {
    multi method Slip(Slip:D:) { self }
    method defined () { self.so }
}

# The slip(...) function creates a Slip.
proto slip(|) { * }
multi slip() {
    my \result = Slip.CREATE;
    nqp::bindattr(result, List, '$!reified', BEGIN IterationBuffer.CREATE);
    result
}
multi slip(|) {
    my \result  = Slip.CREATE;
    my \in      = nqp::p6argvmarray();
    my \reified = IterationBuffer.CREATE;
    nqp::bindattr(result, List, '$!reified', reified);
    while nqp::elems(in) {
        if nqp::istype(nqp::atpos(in, 0), Slip) {
            # We saw a Slip, so we'll lazily deal with the rest of the things
            # (as the Slip may expand to something lazy).
            my \todo := List::Reifier.CREATE;
            nqp::bindattr(result, List, '$!todo', todo);
            nqp::bindattr(todo, List::Reifier, '$!reified', reified);
            nqp::bindattr(todo, List::Reifier, '$!future', in);
            nqp::bindattr(todo, List::Reifier, '$!reification-target',
                result.reification-target());
            last;
        }
        else {
            nqp::push(reified, nqp::shift(in));
            Nil # don't Sink the thing above
        }
    }
    result
}

# vim: ft=perl6 expandtab sw=4
