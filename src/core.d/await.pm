proto sub await(|) { * }
multi sub await() {
    die "Must specify an Awaitable to await (got an empty list)";
}
multi sub await(Any:U $x) {
    die "Must specify a defined Awaitable to await (got an undefined $x.^name())";
}
multi sub await(Any:D $x) {
    die "Must specify an Awaitable to await (got a $x.^name())";
}
multi sub await(Awaitable:D \a) { $*AWAITER.await(a) }
multi sub await(Iterable:D \i)  { $*AWAITER.await-all(i) }
multi sub await(*@awaitables)   { $*AWAITER.await-all(@awaitables) }
