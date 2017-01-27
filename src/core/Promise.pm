# A promise is a synchronization mechanism for a piece of work that will
# produce a single result (keeping the promise) or fail (breaking the
# promise).
my enum PromiseStatus (:Planned(0), :Kept(1), :Broken(2));
my class X::Promise::Combinator is Exception {
    has $.combinator;
    method message() { "Can only use $!combinator to combine defined Promise objects" }
}
my class X::Promise::CauseOnlyValidOnBroken is Exception {
    has $.promise;
    has $.status;
    method message() { "Can only call cause on a broken promise (status: $.status)" }
}
my class X::Promise::Vowed is Exception {
    has $.promise;
    method message() { "Access denied to keep/break this Promise; already vowed" }
}
my role X::Promise::Broken {
    has $.result-backtrace;
    multi method gist(::?CLASS:D:) {
        "Tried to get the result of a broken Promise\n" ~
            ((try $!result-backtrace ~ "\n") // '') ~
            "Original exception:\n" ~
            callsame().indent(4)
    }
}
my class Promise does Awaitable {
    has $.scheduler;
    has $.status;
    has $!result is default(Nil);
    has int $!vow_taken;
    has $!lock;
    has $!cond;
    has @!thens;
    has Mu $!dynamic_context;

    submethod BUILD(:$!scheduler = $*SCHEDULER --> Nil) {
        $!lock            := nqp::create(Lock);
        $!cond            := $!lock.condition();
        $!status           = Planned;
    }

    # A Vow is used to enable the right to keep/break a promise
    # to be restricted to a given "owner". Taking the Vow for a Promise
    # prevents anybody else from getting hold of it.
    my class Vow { ... }
    trusts Vow;
    my class Vow {
        has $.promise;
        method keep(Mu \result) {
            $!promise!Promise::keep(result)
        }
        method break(\exception) {
            $!promise!Promise::break(exception)
        }
    }
    method vow() {
        nqp::lock($!lock);
        if $!vow_taken {
            nqp::unlock($!lock);
            X::Promise::Vowed.new(promise => self).throw
        }
        my $vow := nqp::create(Vow);
        nqp::bindattr($vow, Vow, '$!promise', self);
        $!vow_taken = 1;
        nqp::unlock($!lock);
        $vow
    }

    proto method keep(|) { * }
    multi method keep(Promise:D:) {
        self.vow.keep(True)
    }
    multi method keep(Promise:D: Mu \result) {
        self.vow.keep(result)
    }

    method !keep(Mu \result) {
        $!lock.protect({
            $!result := result;
            $!status = Kept;
            self!schedule_thens();
            $!cond.signal_all;
        });
        Nil
    }

    proto method break(|) { * }
    multi method break(Promise:D:) {
        self.vow.break(False)
    }
    multi method break(Promise:D: \result) {
        self.vow.break(result)
    }

    method !break(\result) {
        $!lock.protect({
            $!result = nqp::istype(result, Exception)
                ?? result
                !! X::AdHoc.new(payload => result);
            $!status = Broken;
            self!schedule_thens();
            $!cond.signal_all;
        });
        Nil
    }

    method !schedule_thens() {
        while @!thens {
            $!scheduler.cue(@!thens.shift, :catch(@!thens.shift))
        }
    }

    method result(Promise:D:) {
        # One important missing optimization here is that if the promise is
        # not yet started, then the work can be done immediately by the
        # thing that is blocking on it. (Note the while loop is there to cope
        # with spurious wake-ups).
        while $!status == Planned {
            $!lock.protect({
                # Re-check planned to avoid data race.
                $!cond.wait() if $!status == Planned;
            });
        }
        if $!status == Kept {
            $!result
        }
        elsif $!status == Broken {
            ($!result but X::Promise::Broken(Backtrace.new)).rethrow
        }
    }

    multi method Bool(Promise:D:) {
        so $!status == Broken || $!status == Kept
    }

    method cause(Promise:D:) {
        my $status = $!status;
        if $status == Broken {
            $!result
        } else {
            X::Promise::CauseOnlyValidOnBroken.new(
                promise => self,
                status  => $status,
            ).throw
        }
    }

    method then(Promise:D: &code) {
        nqp::lock($!lock);
        if $!status == Broken || $!status == Kept {
            # Already have the result, start immediately.
            nqp::unlock($!lock);
            Promise.start( { code(self) }, :$!scheduler);
        }
        else {
            # Create a Promise, and push 2 entries to @!thens: something that
            # starts the then code, and something that handles its exceptions.
            # They will be sent to the scheduler when this promise is kept or
            # broken.
            my $then_promise = Promise.new(:$!scheduler);
            my $vow = $then_promise.vow;
            @!thens.push({ $vow.keep(code(self)) });
            @!thens.push(-> $ex { $vow.break($ex) });
            nqp::unlock($!lock);
            $then_promise
        }
    }

    my class PromiseAwaitableHandle does Awaitable::Handle {
        has &!add-subscriber;

        method not-ready(&add-subscriber) {
            self.CREATE!not-ready(&add-subscriber)
        }
        method !not-ready(&add-subscriber) {
            $!already = False;
            &!add-subscriber := &add-subscriber;
            self
        }

        method subscribe-awaiter(&subscriber --> Nil) {
            &!add-subscriber(&subscriber);
        }
    }

    method get-await-handle(--> Awaitable::Handle) {
        if $!status == Broken {
            PromiseAwaitableHandle.already-failure($!result)
        }
        elsif $!status == Kept {
            PromiseAwaitableHandle.already-success($!result)
        }
        else {
            PromiseAwaitableHandle.not-ready: -> &on-ready {
                nqp::lock($!lock);
                if $!status == Broken || $!status == Kept {
                    # Already have the result, call on-ready immediately.
                    nqp::unlock($!lock);
                    on-ready($!status == Kept, $!result)
                }
                else {
                    # Push 2 entries to @!thens (only need the first one in
                    # this case; second we push 'cus .then uses it).
                    @!thens.push({ on-ready($!status == Kept, $!result) });
                    @!thens.push(Callable);
                    nqp::unlock($!lock);
                }
            }
        }
    }

    method start(Promise:U: &code, :&catch, :$scheduler = $*SCHEDULER, |c) {
        my $p := Promise.new(:$scheduler);
        nqp::bindattr($p, Promise, '$!dynamic_context', nqp::ctx());
        my $vow = $p.vow;
        $scheduler.cue(
            { my $*PROMISE := $p; $vow.keep(code(|c)) },
            :catch(-> $ex { catch($ex) if &catch; $vow.break($ex); }) );
        $p
    }

    method in(Promise:U: $seconds, :$scheduler = $*SCHEDULER) {
        my $p   = Promise.new(:$scheduler);
        my $vow = $p.vow;
        $scheduler.cue({ $vow.keep(True) }, :in($seconds));
        $p
    }
    method at(Promise:U: $at, :$scheduler = $*SCHEDULER) {
        self.in( $at - now, :$scheduler )
    }

    method anyof(Promise:U: *@p) { self!until_n_kept(@p,   1, 'anyof') }
    method allof(Promise:U: *@p) { self!until_n_kept(@p, +@p, 'allof') }

    method !until_n_kept(@promises, Int $N, Str $combinator) {
        my $p = Promise.new;
        unless @promises {
            $p.keep;
            return $p
        }

        X::Promise::Combinator.new(:$combinator).throw
          unless Rakudo::Internals.ALL_DEFINED_TYPE(@promises, Promise);

        my int $n  = $N;
        my int $c  = $n;
        my $lock  := nqp::create(Lock);
        my $vow    = $p.vow;
        for @promises -> $cand {
            $cand.then({
                if $lock.protect({ $c = $c - 1 }) == 0 {
                    $vow.keep(True)
                }
            })
        }
        $p
    }

    method Supply(Promise:D:) {
        Supply.on-demand: -> $s {
            self.then({
                if self.status == Kept {
                    $s.emit(self.result);
                    $s.done();
                }
                else {
                    $s.quit(self.cause);
                }
            });
        }
    }
}

multi sub infix:<eqv>(Promise:D \a, Promise:D \b) {
    nqp::p6bool(
      nqp::eqaddr(a,b) || a.result eqv b.result
    )
}

# vim: ft=perl6 expandtab sw=4
