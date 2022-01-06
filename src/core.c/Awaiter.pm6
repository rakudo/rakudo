my role Awaiter {
    method await(Awaitable:D $a) { ... }
    method await-all(Iterable:D $i) { ... }
}

my class Awaiter::Blocking does Awaiter {
    method await(Awaitable:D $a) {
        my $handle := $a.get-await-handle;
        if $handle.already {
            $handle.success
                ?? $handle.result
                !! $handle.cause.rethrow
        }
        else {
            my $s = Semaphore.new(0);
            my $success;
            my $result;
            $handle.subscribe-awaiter(-> \success, \result {
                $success := success;
                $result := result;
                $s.release;
            });
            $s.acquire;
            $success
                ?? $result
                !! $result.rethrow
        }
    }

    method await-all(Iterable:D \i) {
        # Collect results that are already available, and handles where the
        # results are not yet available together with the matching insertion
        # indices.
        my \results = nqp::list();
        my \handles = nqp::list();
        my \indices = nqp::list_i();
        my int $insert = 0;
        my $saw-slip = False;
        for i -> $awaitable {
            unless nqp::istype($awaitable, Awaitable) {
                die "Can only specify Awaitable objects to await (got a $awaitable.^name())";
            }
            unless nqp::isconcrete($awaitable) {
                die "Must specify a defined Awaitable to await (got an undefined $awaitable.^name())";
            }

            my $handle := $awaitable.get-await-handle;
            if $handle.already {
                if $handle.success {
                    my \result = $handle.result;
                    nqp::bindpos(results, $insert, result);
                    $saw-slip = True if nqp::istype(result, Slip);
                }
                else {
                    $handle.cause.rethrow
                }
            }
            else {
                nqp::push(handles, $handle);
                nqp::push_i(indices, $insert);
            }

            ++$insert;
        }

        # See if we have anything that we need to really block on. If so, we
        # use a lock and condition variable to handle the blocking. The lock
        # protects writes into the array.
        my int $num-handles = nqp::elems(handles);
        if $num-handles {
            my $exception = Mu;
            my $l = Lock.new;
            my $ready = $l.condition();
            my int $remaining = $num-handles;
            loop (my int $i = 0; $i < $num-handles; ++$i) {
                my $handle := nqp::atpos(handles, $i);
                my int $insert = nqp::atpos_i(indices, $i);
                $handle.subscribe-awaiter(-> \success, \result {
                    $l.protect: {
                        if success && $remaining {
                            nqp::bindpos(results, $insert, result);
                            $saw-slip = True if nqp::istype(result, Slip);
                            --$remaining;
                            $ready.signal unless $remaining;
                        }
                        elsif !nqp::isconcrete($exception) {
                            $exception := result;
                            $remaining = 0;
                            $ready.signal;
                        }
                    }
                });
            }

            $l.protect: {
                $ready.wait: { $remaining == 0 }
            }

            # If we got an exception, throw it.
            $exception.rethrow if nqp::isconcrete($exception);
        }

        my \result-list = nqp::p6bindattrinvres(nqp::create(List), List, '$!reified', results);
        $saw-slip ?? result-list.map(-> \val { val }).List !! result-list
    }
}

PROCESS::<$AWAITER> := Awaiter::Blocking;

# vim: expandtab shiftwidth=4
