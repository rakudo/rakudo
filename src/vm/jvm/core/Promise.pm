# A promise is a synchronization mechanism for a piece of work that will
# produce a single result (keeping the promise) or fail (breaking the
# promise).
my enum PromiseStatus (:Planned(0), :Kept(1), :Broken(2));
my class X::Promise::Combinator is Exception {
    has $.combinator;
    method message() { "Can only use $!combinator to combine other Promise objects" }
}
my class X::Promise::CauseOnlyValidOnBroken is Exception {
    method message() { "Can only call cause on a broken promise" }
}
my class X::Promise::KeeperTaken is Exception {
    method message() { "Access denied to keep/break this Promise; the keeper was already taken" }
}
my class Promise {
    has $.scheduler;
    has $.status;
    has $!result;
    has int $!keeper_taken;
    has Mu $!ready_semaphore;
    has Mu $!lock;
    has @!thens;
    
    submethod BUILD(:$!scheduler = $*SCHEDULER) {
        my $interop       := nqp::jvmbootinterop();
        my \Semaphore     := $interop.typeForName('java.util.concurrent.Semaphore');
        my \ReentrantLock := $interop.typeForName('java.util.concurrent.locks.ReentrantLock');
        $!ready_semaphore := Semaphore.'constructor/new/(I)V'(-1);
        $!lock            := ReentrantLock.'constructor/new/()V'();
        $!status           = Planned;
    }
    
    # A Promise::Keeper is used to enable the right to keep/break a promise
    # to be restricted to a given "owner". Taking the keeper for a Promise
    # prevents anybody else from getting hold of it.
    class Keeper { ... }
    trusts Keeper;
    class Keeper {
        has $.promise;
        method keep(\result) {
            $!promise!Promise::keep(result)
        }
        method break(\exception) {
            $!promise!Promise::break(exception)
        }
    }
    method keeper() {
        $!lock.lock();
        if $!keeper_taken {
            $!lock.unlock();
            X::Promise::KeeperTaken.new.throw
        }
        my $k := nqp::create(Keeper);
        nqp::bindattr($k, Keeper, '$!promise', self);
        $!keeper_taken = 1;
        $!lock.unlock();
        $k
    }

    method keep(Promise:D: $result) {
        self.keeper.keep($result)
    }
    
    method !keep($!result) {
        $!status = Kept;
        $!ready_semaphore.'method/release/(I)V'(32768);
        self!schedule_thens();
        $!result
    }
    
    method break(Promise:D: $result) {
        self.keeper.break($result)
    }
    
    method !break($result) {
        $!result = $result ~~ Exception ?? $result !! X::AdHoc.new(payload => $result);
        $!status = Broken;
        $!ready_semaphore.'method/release/(I)V'(32768);
        self!schedule_thens();
    }
    
    method !schedule_thens() {
        $!lock.lock();
        while @!thens {
            $!scheduler.schedule_with_catch(@!thens.shift, @!thens.shift)
        }
        $!lock.unlock();
    }
    
    method result(Promise:D:) {
        # One important missing optimization here is that if the promise is
        # not yet started, then the work can be done immediately by the
        # thing that is blocking on it.
        if $!status == Planned {
            $!ready_semaphore.'method/acquire/()V'();
        }
        if $!status == Kept {
            $!result
        }
        elsif $!status == Broken {
            $!result.throw
        }
    }
    
    method has_result(Promise:D:) {
        so $!status == any(Broken, Kept)
    }
    
    multi method Bool(Promise:D:) {
        self.has_result
    }
    
    method cause(Promise:D:) {
        if $!status == Broken {
            $!result
        } else {
            X::Promise::CauseOnlyValidOnBroken.new.throw
        }
    }
    
    method then(Promise:D: &code) {
        $!lock.lock();
        if $!status == any(Broken, Kept) {
            # Already have the result, start immediately.
            $!lock.unlock();
            Promise.start(:$!scheduler, :code({ code(self) }))
        }
        else {
            # Create a Promise, and push 2 entries to @!thens: something that
            # starts the then code, and something that handles its exceptions.
            # They will be sent to the scheduler when this promise is kept or
            # broken.
            my $then_promise = Promise.new(:$!scheduler);
            my $k = $then_promise.keeper;
            @!thens.push({ $k.keep(code(self)) });
            @!thens.push(-> $ex { $k.break($ex) });
            $!lock.unlock();
            $then_promise
        }
    }
    
    method start(Promise:U: &code, :$scheduler = $*SCHEDULER) {
        my $p = Promise.new(:$scheduler);
        my $k = $p.keeper;
        $scheduler.schedule_with_catch(
            { $k.keep(code()) },
            -> $ex { $k.break($ex) });
        $p
    }
    
    method sleep(Promise:U: $seconds, :$scheduler = $*SCHEDULER) {
        my $p = Promise.new(:$scheduler);
        my $k = $p.keeper;
        $scheduler.schedule_in({ $k.keep(True) }, $seconds);
        $p
    }
    
    method anyof(Promise:U: *@promises) {
        X::Promise::Combinator.new(combinator => 'anyof').throw
            unless @promises >>~~>> Promise;
        self!until_n_kept(@promises, 1)
    }
    
    method allof(Promise:U: *@promises) {
        X::Promise::Combinator.new(combinator => 'allof').throw
            unless @promises >>~~>> Promise;
        self!until_n_kept(@promises, @promises.elems)
    }
    
    my Mu $AtomicInteger;
    method !until_n_kept(@promises, Int $n) {
        once {
            $AtomicInteger := nqp::jvmbootinterop().typeForName('java.util.concurrent.atomic.AtomicInteger');
            Nil;
        }
        my Mu $c := $AtomicInteger.'constructor/new/(I)V'(nqp::decont($n));
        my $p = Promise.new;
        my $k = $p.keeper;
        for @promises -> $cand {
            $cand.then({
                if .status == Kept {
                    if $c.'decrementAndGet'() == 0 {
                        $k.keep(True)
                    }
                }
                else {
                    if $c.'getAndAdd'(-($n + 1)) > 0 {
                        $k.break(.cause)
                    }
                }
            })
        }
        $p
    }
}

# Schedules a piece of asynchronous work using the current scheduler, and
# returns a Promise that represents it.
sub start(&code) { Promise.start(&code) }
