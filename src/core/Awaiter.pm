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

    method await-all(Iterable:D $i) {
        die "await-all NYI";
    }
}

PROCESS::<$AWAITER> := Awaiter::Blocking;
