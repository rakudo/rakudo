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

sub REACT(&block --> Nil) {
    my class ReactAwaitable does Awaitable {
        has $!handle;

        method new($handle) {
            self.CREATE!set-handle($handle)
        }
        method !set-handle($handle) {
            $!handle = $handle;
            self
        }

        method get-await-handle() { $!handle }
    }
    my class ReactAwaitHandle does Awaitable::Handle {
        has &!react-block;

        method not-ready(&react-block) {
            self.CREATE!set-react-block(&react-block)
        }
        method !set-react-block(&react-block) {
            &!react-block = &react-block;
            self
        }

        method subscribe-awaiter(&subscriber) {
            SUPPLY(&!react-block).tap:
                { warn "Useless use of emit in react" },
                done => { subscriber(True, Nil) },
                quit => { subscriber(False, $_) };
        }
    }
    $*AWAITER.await(ReactAwaitable.new(ReactAwaitHandle.not-ready(&block)));
}
