my role X::Await::Died {
    has $.await-backtrace;
    multi method gist(::?CLASS:D:) {
        "An operation first awaited:\n" ~
            ((try $!await-backtrace ~ "\n") // '<unknown location>') ~
            "Died with the exception:\n" ~
            callsame().indent(4)
    }
}

proto sub await(|) {*}
multi sub await() {
    die "Must specify an Awaitable to await (got an empty list)";
}
multi sub await(Any:U $x) {
    die "Must specify a defined Awaitable to await (got an undefined $x.^name())";
}
multi sub await(Awaitable:D \a) {
    CATCH {
        unless nqp::istype($_, X::Await::Died) {
            ($_ but X::Await::Died(Backtrace.new(5))).rethrow
        }
    }
    $*AWAITER.await(a)
}
multi sub await(*@awaitables) {
    CATCH {
        unless nqp::istype($_, X::Await::Died) {
            ($_ but X::Await::Died(Backtrace.new(5))).rethrow
        }
    }
    $*AWAITER.await-all(@awaitables)
}

my role X::React::Died {
    has $.react-backtrace;
    multi method gist(::?CLASS:D:) {
        "A react block:\n" ~
            ((try $!react-backtrace ~ "\n") // '<unknown location>') ~
            "Died because of the exception:\n" ~
            callsame().indent(4)
    }
}

my class Rakudo::Internals::ReactAwaitable does Awaitable {
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
my class Rakudo::Internals::ReactAwaitHandle does Awaitable::Handle {
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
my class Rakudo::Internals::ReactOneWheneverAwaitHandle does Awaitable::Handle {
    has &!react-block;

    method not-ready(&react-block) {
        self.CREATE!set-react-block(&react-block)
    }
    method !set-react-block(&react-block) {
        &!react-block = &react-block;
        self
    }

    method subscribe-awaiter(&subscriber) {
        SUPPLY-ONE-WHENEVER(&!react-block).tap:
            { warn "Useless use of emit in react" },
            done => { subscriber(True, Nil) },
            quit => { subscriber(False, $_) };
    }
}
sub REACT(&block --> Nil) is implementation-detail {
    CATCH {
        ($_ but X::React::Died(Backtrace.new(5))).rethrow
    }
    $*AWAITER.await(Rakudo::Internals::ReactAwaitable.new(
        Rakudo::Internals::ReactAwaitHandle.not-ready(&block)));
}
sub REACT-ONE-WHENEVER(&block --> Nil) is implementation-detail {
    CATCH {
        ($_ but X::React::Died(Backtrace.new(5))).rethrow
    }
    $*AWAITER.await(Rakudo::Internals::ReactAwaitable.new(
        Rakudo::Internals::ReactOneWheneverAwaitHandle.not-ready(&block)));
}

# vim: expandtab shiftwidth=4
