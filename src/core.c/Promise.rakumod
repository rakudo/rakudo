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
my class X::Promise::Resolved is Exception {
    has $.promise;
    method message() {
        "Cannot keep/break a Promise more than once (status: $!promise.status())";
    }
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
    has $!thens;
    has $!thens-sync;
    has Mu $!dynamic_context;
    has Bool $!report-broken-if-sunk;

    method !SET-SELF($scheduler, $report) {
        $!scheduler             := $scheduler;
        $!report-broken-if-sunk := nqp::if(nqp::istrue($report),True,False);
        $!lock                  := nqp::create(Lock);
        $!cond                  := $!lock.condition;
        $!status                := Planned;
        $!thens                 := nqp::null;
        $!thens-sync            := nqp::null;
        self
    }
    submethod new(:$scheduler = $*SCHEDULER, :$report-broken-if-sunk) {
        if nqp::eqaddr(self,Promise) {
            nqp::create(self)!SET-SELF($scheduler, $report-broken-if-sunk)
        }
        else {
            my \p = nqp::create(self);
            p.BUILD(:$scheduler, :$report-broken-if-sunk);
            p
        }
    }

    submethod BUILD(
      :$!scheduler = $*SCHEDULER,
      :$report-broken-if-sunk
    --> Nil) {
        $!report-broken-if-sunk := so $report-broken-if-sunk;
        $!lock                  := nqp::create(Lock);
        $!cond                  := $!lock.condition();
        $!status                := Planned;
        $!thens                 := nqp::null;
        $!thens-sync            := nqp::null;
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
        $!vow_taken = 1;
        nqp::unlock($!lock);
        nqp::p6bindattrinvres(nqp::create(Vow), Vow, '$!promise', self);
    }

    proto method kept(|) {*}
    multi method kept(Promise:U:) {
        my \rv := self.new;
        rv!keep(True);
        rv;
    }
    multi method kept(Promise:U: Mu \result) {
        my \rv := self.new;
        rv!keep(result);
        rv;
    }

    proto method keep(|) {*}
    multi method keep(Promise:D:) {
        self.vow.keep(True)
    }
    multi method keep(Promise:D: Mu \result) {
        self.vow.keep(result)
    }

    method !keep(Mu \result --> Nil) {
        $!lock.protect({
            X::Promise::Resolved.new(promise => self).throw
             if $!status != Planned;

            $!result := result;
            $!status := Kept;
            self!schedule_thens();
            $!cond.signal_all;
        });
    }

    proto method broken(|) {*}
    multi method broken(Promise:U:) {
        my \rv := self.new;
        rv!break("Died");
        rv;
    }
    multi method broken(Promise:U: Mu \exception) {
        my \rv := self.new;
        rv!break(exception);
        rv;
    }

    proto method break(|) {*}
    multi method break(Promise:D:) {
        self.vow.break("Died")
    }
    multi method break(Promise:D: \result) {
        self.vow.break(result)
    }

    method !break(\result --> Nil) {
        $!lock.protect({
            X::Promise::Resolved.new(promise => self).throw
             if $!status != Planned;

            $!result := nqp::istype(result, Exception)
                ?? result
                !! X::AdHoc.new(payload => result);
            $!status := Broken;
            self!schedule_thens();
            $!cond.signal_all;
        });
    }

    method !schedule_thens(--> Nil) {
        nqp::unless(
          nqp::isnull($!thens),
          nqp::while(
            nqp::elems($!thens),
            $!scheduler.cue(nqp::shift($!thens), :catch(nqp::shift($!thens)))
          )
        );
        nqp::unless(
          nqp::isnull($!thens-sync),
          nqp::while(
            nqp::elems($!thens-sync),
            nqp::stmts(
              (my &code := nqp::shift($!thens-sync)),
              (my &ex := nqp::shift($!thens-sync)),
              nqp::handle(code(), 'CATCH', ex())
            )
          )
        );
    }

    method result(Promise:D:) {
        # One important missing optimization here is that if the promise is
        # not yet started, then the work can be done immediately by the
        # thing that is blocking on it.
        $!lock.protect: {
            $!cond.wait: { $!status != Planned }
        };
        if $!status == Kept {
            $!result
        }
        elsif $!status == Broken {
            ($!result but X::Promise::Broken(Backtrace.new)).rethrow
        }
    }

    multi method Bool(Promise:D:) {
        $!status != Planned
    }

    method cause(Promise:D:) {
        my $status := $!status;
        if $status == Broken {
            $!result
        } else {
            X::Promise::CauseOnlyValidOnBroken.new(
                promise => self,
                status  => $status,
            ).throw
        }
    }

    method !PLANNED-THEN(\then-promise, \vow, \then-code, \synchronous) {
        # Push 2 entries to $!thens: something that starts the then code,
        # and something that handles its exceptions. They will be sent to the
        # scheduler when this promise is kept or broken.
        nqp::bindattr(then-promise, Promise, '$!dynamic_context', nqp::ctx());
        if synchronous {
            nqp::push(nqp::ifnull($!thens-sync,($!thens-sync := nqp::list)), then-code);
            nqp::push($!thens-sync, -> $ex { vow.break($ex) });
        }
        else {
            nqp::push(nqp::ifnull($!thens, ($!thens := nqp::list)), then-code);
            nqp::push($!thens, -> $ex { vow.break($ex) });
        }
        nqp::unlock($!lock);
        then-promise
    }

    method then(Promise:D: &code, :$synchronous) {
        nqp::lock($!lock);
        if $!status == Broken || $!status == Kept {
            # Already have the result, start immediately.
            nqp::unlock($!lock);
            $synchronous
                ?? code(self)
                !! self.WHAT.start( { code(self) }, :$!scheduler);
        }
        else {
            my $then-p := self.new(:$!scheduler);
            my $vow := $then-p.vow;
            self!PLANNED-THEN( $then-p,
                               $vow,
                               { my $*PROMISE := $then-p; $vow.keep(code(self)) },
                               $synchronous)
        }
    }

    method andthen(Promise:D: &code, :$synchronous) {
        nqp::lock($!lock);
        if $!status == Broken {
            nqp::unlock($!lock);
            self.WHAT.broken($!result)
        }
        elsif $!status == Kept {
            # Already have the result, start immediately.
            nqp::unlock($!lock);
            $synchronous
                ?? code(self)
                !! self.WHAT.start( { code(self) }, :$!scheduler);
        }
        else {
            my $then-p := self.new(:$!scheduler);
            my $vow := $then-p.vow;
            self!PLANNED-THEN( $then-p,
                               $vow,
                               { $!status == Kept
                                    ?? do { my $*PROMISE := $then-p; $vow.keep(code(self)) }
                                    !! $vow.break($!result) },
                               $synchronous)
        }
    }

    method orelse(Promise:D: &code, :$synchronous) {
        nqp::lock($!lock);
        if $!status == Broken {
            nqp::unlock($!lock);
            $synchronous
                ?? code(self)
                !! self.WHAT.start( { code(self) }, :$!scheduler);
        }
        elsif $!status == Kept {
            # Already have the result, start immediately.
            nqp::unlock($!lock);
            self.WHAT.kept($!result);
        }
        else {
            my $then-p := self.new(:$!scheduler);
            my $vow := $then-p.vow;
            self!PLANNED-THEN( $then-p,
                               $vow,
                               { $!status == Kept
                                   ?? $vow.keep($!result)
                                   !! do { my $*PROMISE := $then-p; $vow.keep(code(self)) } },
                               $synchronous)
        }
    }

    my class PromiseAwaitableHandle does Awaitable::Handle {
        has &!add-subscriber;

        method not-ready(&add-subscriber) {
            nqp::create(self)!not-ready(&add-subscriber)
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

    method get-await-handle(--> Awaitable::Handle:D) {
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
                    # Push 2 entries to $!thens (only need the first one in
                    # this case; second we push 'cus .then uses it).
                    nqp::push(
                      nqp::ifnull($!thens,($!thens := nqp::list)),
                      { on-ready($!status == Kept, $!result) }
                    );
                    nqp::push($!thens, Callable);
                    nqp::unlock($!lock);
                }
            }
        }
    }

    method sink(--> Nil) {
         self.then({
           $!scheduler.handle_uncaught(.cause) if .status == Broken
         }) if $!report-broken-if-sunk
            && $!lock.protect({
                   nqp::hllbool(
                     nqp::isnull($!thens) || nqp::not_i(nqp::elems($!thens))
                   )
               });
    }

    method start(Promise:U: &code, :&catch, :$scheduler = $*SCHEDULER,
            :$report-broken-if-sunk, |c) {
        my $p := self.new(:$scheduler, :$report-broken-if-sunk);

        # If there was already a pre-start context saved, use that.
        # Else use the current context as the pre-start context in
        # which to check for dynamic variables
        nqp::bindattr($p, Promise, '$!dynamic_context',
          nqp::istype($*PROMISE,Failure)
            ?? nqp::ctx()
            !! nqp::getattr($*PROMISE, Promise, '$!dynamic_context')
        );

        my $vow := $p.vow;
        $scheduler.cue(
            { my $*PROMISE := $p; $vow.keep(code(|c)) },
            :catch(-> $ex { catch($ex) if &catch; $vow.break($ex); }) );
        $p
    }

    method in(Promise:U: $seconds, :$scheduler = $*SCHEDULER) {
        my $p   := self.new(:$scheduler);
        my $vow := $p.vow;
        $scheduler.cue({ $vow.keep(True) }, :in($seconds));
        $p
    }
    method at(Promise:U: $at, :$scheduler = $*SCHEDULER) {
        self.in( $at - now, :$scheduler )
    }

    method anyof(Promise:U: *@p) { self!until_n_kept(@p,   1, 'anyof') }
    method allof(Promise:U: *@p) { self!until_n_kept(@p, +@p, 'allof') }

    method !until_n_kept(@promises, Int:D $N, Str $combinator) {
        my $p := self.new;
        unless @promises {
            $p.keep;
            return $p
        }

        X::Promise::Combinator.new(:$combinator).throw
          unless Rakudo::Internals.ALL_DEFINED_TYPE(@promises, Promise);

        my int $n  = $N;
        my int $c  = $n;
        my $lock  := nqp::create(Lock);
        my $vow   := $p.vow;
        for @promises -> $cand {
            $cand.then({
                if $lock.protect({ $c = $c - 1 }) == 0 {
                    $vow.keep(True)
                }
            })
        }
        $p
    }

    multi method Supply(Promise:D:) {
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

multi sub infix:<eqv>(Promise:D $a, Promise:D $b --> Bool:D) {
    nqp::hllbool(
      nqp::eqaddr($a,$b) || $a.result eqv $b.result
    )
}

# vim: expandtab shiftwidth=4
