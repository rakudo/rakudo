# This file contains early work on concurrency support for Rakudo on the JVM.
# The implementation is clearly VM specific, however the aim is to iterate
# towards a backend-independent API.

# Thread represents an OS-level thread. While it could be used directly, it
# is not the preferred way to work in Perl 6. It's a building block for the
# interesting things.
my class Thread {
    # This thread's underlying JVM thread object.
    has Mu $!jvm_thread;

    # Is the thread's lifetime bounded by that of the application, such
    # that when it exits, so does the thread?
    has Bool $.app_lifetime;

    # Thread's (user-defined) name.
    has Str $.name;

    submethod BUILD(:&code!, :$!app_lifetime as Bool = False, :$!name as Str = "<anon>") {
        my $interop   := nqp::jvmbootinterop();
        my \JVMThread := $interop.typeForName('java.lang.Thread');
        $!jvm_thread  := JVMThread."constructor/new/(Ljava/lang/Runnable;)V"(
            $interop.proxy('java.lang.Runnable', nqp::hash('run',
                {
                    my $*THREAD = self;
                    code();
                })));
        $!jvm_thread.setDaemon(1) if $!app_lifetime;
    }

    method run(&code, *%adverbs) {
        Thread.new(:&code, |%adverbs).start()
    }

    method start(Thread:D:) {
        $!jvm_thread.start();
        self
    }

    method id(Thread:D:) {
        $!jvm_thread.getId();
    }

    method join(Thread:D:) {
        $!jvm_thread.'method/join/()V'();
        self
    }

    multi method Str(Thread:D:) {
        "Thread<$.id>($.name)"
    }

    method yield(Thread:U:) {
        nqp::jvmbootinterop().typeForName('java.lang.Thread').yield();
    }
}

{
    # This code is a little funky to avoid hitting jvmbootinterop at startup
    # even if we never use anything that needs it. This is because it carries
    # some cost and may have a bad interaction with the evalserver.
    my $init_thread;
    PROCESS::<$THREAD> := Proxy.new(
        FETCH => -> | {
            unless nqp::isconcrete($init_thread) {
                my $interop   := nqp::jvmbootinterop();
                my \JVMThread := $interop.typeForName('java.lang.Thread');
                $init_thread  := nqp::create(Thread);
                nqp::bindattr($init_thread, Thread, '$!jvm_thread', JVMThread.currentThread());
                nqp::bindattr($init_thread, Thread, '$!app_lifetime', False);
                nqp::bindattr($init_thread, Thread, '$!name', 'Initial thread');
            }
            $init_thread
        },
        STORE => -> | {
            X::Assignment::RO.new.throw
        });
}

# A simple, reentrant lock mechanism.
my class Lock {
    has $!lock;

    submethod BUILD() {
        my \ReentrantLock := nqp::jvmbootinterop().typeForName('java.util.concurrent.locks.ReentrantLock');
        $!lock            := ReentrantLock.'constructor/new/()V'();
    }

    method lock() { $!lock.lock() }

    method unlock() { $!lock.unlock() }

    method run(&code) {
        $!lock.lock();
        my \res := code();
        $!lock.unlock();
        CATCH { $!lock.unlock(); }
        res
    }
}

# Schedulers do this role. It mostly serves as an interface for the things
# that schedulers must do, as well as a way to factor out some common "sugar"
# and infrastructure.
my role Scheduler {
    has &.uncaught_handler is rw;

    method handle_uncaught($exception) {
        my $ch = &!uncaught_handler;
        if $ch {
            $ch($exception);
        }
        else {
            # No default handler, so terminate the application.
            note "Unhandled exception in code scheduled on thread " ~ $*THREAD.id;
            note $exception.gist;
            exit(1);
        }
    }

    method schedule(&code) { ... }

    method schedule_with_catch(&code, &catch) {
        self.schedule({
            code();
            CATCH { default { catch($_) } }
        })
    }
    
    method schedule_in(&code, $delay) { ... }
    method schedule_every(&code, $interval, $delay = 0) { ... }
    method outstanding() { ... }
}

# The ThreadPoolScheduler is a straightforward scheduler that maintains a
# pool of threads and schedules work items in the order they are added
# using them.
my class ThreadPoolScheduler does Scheduler {
    # A concurrent work queue that blocks any worker threads that poll it
    # when empty until some work arrives.
    has Mu $!queue;
    
    # Semaphore to ensure we don't start more than the maximum number of
    # threads allowed.
    has Mu $!thread_start_semaphore;
    
    # Atomic integer roughly tracking outstanding work, used for rough
    # management of the pool size.
    has Mu $!outstanding;
    
    # Initial and maximum thereads.
    has $!initial_threads;
    has $!max_threads;
    
    # Have we started any threads yet?
    has int $!started_any;
    
    # Timer for interval-scheduled things.
    has $!timer;

    # Adds a new thread to the pool, respecting the maximum.
    method !maybe_new_thread() {
        if $!thread_start_semaphore.'method/tryAcquire/(I)Z'(1) {
            my $interop := nqp::jvmbootinterop();
            $!started_any = 1;
            Thread.run(:app_lifetime, {
                loop {
                    my Mu $task := $interop.javaObjectToSixmodel($!queue.take());
                    try {
                        $task();
                        CATCH {
                            default {
                                self.handle_uncaught($_)
                            }
                        }
                    }
                    $!outstanding.decrementAndGet();
                }
            });
        }
    }
    
    submethod BUILD(:$!initial_threads = 0, :$!max_threads = 16) {
        die "Initial thread pool threads must be less than or equal to maximum threads"
            if $!initial_threads > $!max_threads;
    }
    
    method schedule(&code) {
        self!initialize unless $!started_any;
        my $outstanding = $!outstanding.incrementAndGet();
        self!maybe_new_thread()
            if !$!started_any || $outstanding > 1;
        $!queue.add(nqp::jvmbootinterop().sixmodelToJavaObject(&code));
    }

    method schedule_in(&code, $delay) {
        self!initialize() unless $!started_any;
        $!timer.'method/schedule/(Ljava/util/TimerTask;J)V'(
            nqp::jvmbootinterop().proxy(
                'java.util.TimerTask',
                nqp::hash('run', -> { code() })),
            ($delay * 1000).Int);
    }

    method schedule_every(&code, $interval, $delay = 0) {
        self!initialize() unless $!started_any;
        $!timer.'method/scheduleAtFixedRate/(Ljava/util/TimerTask;JJ)V'(
            nqp::jvmbootinterop().proxy(
                'java.util.TimerTask',
                nqp::hash('run', -> { code() })),
            ($delay * 1000).Int,
            ($interval * 1000).Int);
    }

    method outstanding() {
        $!outstanding.get()
    }
    
    method !initialize() {
        # Things we will use from the JVM.
        my $interop              := nqp::jvmbootinterop();
        my \LinkedBlockingQueue  := $interop.typeForName('java.util.concurrent.LinkedBlockingQueue');
        my \Semaphore            := $interop.typeForName('java.util.concurrent.Semaphore');
        my \AtomicInteger        := $interop.typeForName('java.util.concurrent.atomic.AtomicInteger');
        my \Timer                := $interop.typeForName('java.util.Timer');
        $!queue                  := LinkedBlockingQueue.'constructor/new/()V'();
        $!thread_start_semaphore := Semaphore.'constructor/new/(I)V'($!max_threads.Int);
        $!outstanding            := AtomicInteger.'constructor/new/()V'();
        $!timer                  := Timer.'constructor/new/(Z)V'(True);
        self!maybe_new_thread() for 1..$!initial_threads;
    }
}

# This thread pool scheduler will be the default one.
$PROCESS::SCHEDULER = ThreadPoolScheduler.new();

# Scheduler that always does things immediately, on the current thread.
my class CurrentThreadScheduler does Scheduler {
    method handle_uncaught($exception) {
        $exception.throw
    }

    method schedule(&code) {
        code()
    }

    method schedule_in(&code, $delay) {
        sleep $delay;
        code();
    }

    method schedule_every(&code, $interval, $delay = 0) {
        sleep $delay;
        loop {
            code();
            sleep $interval;
        }
    }

    method outstanding() {
        0
    }
}

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
            # Already have the result, run immediately.
            $!lock.unlock();
            Promise.run(:$!scheduler, :code({ code(self) }))
        }
        else {
            # Create a Promise, and push 2 entries to @!thens: something that
            # runs the then code, and something that handles its exceptions.
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
    
    method run(Promise:U: &code, :$scheduler = $*SCHEDULER) {
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
sub async(&code) {
    Promise.run(&code);
}

# A channel provides a thread-safe way to send a series of values from some
# producer(s) to some consumer(s).
my class X::Channel::SendOnClosed is Exception {
    method message() { "Cannot send a message on a closed channel" }
}
my class X::Channel::ReceiveOnClosed is Exception {
    method message() { "Cannot receive a message on a closed channel" }
}
my class Channel {
    # The queue of events moving through the channel.
    has Mu $!queue;
    
    # Promise that is triggered when all values are received, or an error is
    # received and the channel is thus closed.
    has $!closed_promise;
    
    # Closed promise's keeper.
    has $!closed_promise_keeper;
    
    # Flag for if the channel is closed to senders.
    has $!closed;
    
    # Magical objects for various ways a channel can end.
    my class CHANNEL_CLOSE { }
    my class CHANNEL_FAIL  { has $.error }
    
    my Mu $interop;
    submethod BUILD() {
        $interop := nqp::jvmbootinterop() unless nqp::isconcrete($interop);
        my \LinkedBlockingQueue := $interop.typeForName('java.util.concurrent.LinkedBlockingQueue');
        $!queue := LinkedBlockingQueue.'constructor/new/()V'();
        $!closed_promise = Promise.new;
        $!closed_promise_keeper = $!closed_promise.keeper;
    }
    
    method send(Channel:D: \item) {
        X::Channel::SendOnClosed.new.throw if $!closed;
        $!queue.add($interop.sixmodelToJavaObject(nqp::decont(item)))
    }
    
    method receive(Channel:D:) {
        my \msg := $interop.javaObjectToSixmodel($!queue.take());
        if nqp::istype(msg, CHANNEL_CLOSE) {
            $!closed_promise_keeper.keep(Nil);
            X::Channel::ReceiveOnClosed.new.throw
        }
        elsif nqp::istype(msg, CHANNEL_FAIL) {
            $!closed_promise_keeper.break(msg.error);
            die msg.error;
        }
        msg
    }
    
    method poll(Channel:D:) {
        my \fetched := $!queue.'method/poll/()Ljava/lang/Object;'();
        if nqp::jvmisnull(fetched) {
            Nil
        } else {
            my \msg := $interop.javaObjectToSixmodel(fetched);
            if nqp::istype(msg, CHANNEL_CLOSE) {
                $!closed_promise_keeper.keep(Nil);
                Nil
            }
            elsif nqp::istype(msg, CHANNEL_FAIL) {
                $!closed_promise_keeper.break(msg.error);
                Nil
            }
            else {
                msg
            }
        }
    }
    
    method peek(Channel:D:) {
        my \fetched := $!queue.'method/peek/()Ljava/lang/Object;'();
        if nqp::jvmisnull(fetched) {
            Nil
        } else {
            my \msg := $interop.javaObjectToSixmodel(fetched);
            if nqp::istype(msg, CHANNEL_CLOSE) {
                $!closed_promise_keeper.keep(Nil);
                Nil
            }
            elsif nqp::istype(msg, CHANNEL_FAIL) {
                $!closed_promise_keeper.break(msg.error);
                Nil
            }
            else {
                msg
            }
        }
    }

    method close() {
        $!closed = 1;
        $!queue.add($interop.sixmodelToJavaObject(CHANNEL_CLOSE))
    }
    
    method fail($error is copy) {
        $!closed = 1;
        $error = X::AdHoc.new(payload => $error) unless nqp::istype($error, Exception);
        $!queue.add($interop.sixmodelToJavaObject(CHANNEL_FAIL.new(:$error)))
    }
    
    method closed() {
        self.peek();
        $!closed_promise
    }
}

# Anything that can be subscribed to does this role. It provides the basic
# subscription management infrastructure, as well as various coercions that
# turn Subscribable things into something else and convenience forms of calls
# to SubscribableOperations.
my class SubscribableOperations { ... }
my role Subscribable {
    my class Subscription {
        has &.next;
        has &.last;
        has &.fail;
        has $.subscribable;
        method unsubscribe() {
            $!subscribable.unsubscribe(self)
        }
    }

    has @!subscriptions;
    has $!subscriptions_lock = Lock.new;

    method subscribe(&next, &last?, &fail?) {
        my $sub = Subscription.new(:&next, :&last, :&fail, :subscribable(self));
        $!subscriptions_lock.run({
            @!subscriptions.push($sub);
        });
        $sub
    }

    method unsubscribe(Subscription $s) {
        $!subscriptions_lock.run({
            @!subscriptions.=grep(* !=== $s);
        });
    }

    method subscriptions() {
        # Shallow clone to provide safe snapshot.
        my @subs;
        $!subscriptions_lock.run({ @subs = @!subscriptions });
        @subs
    }

    method Channel() {
        my $c = Channel.new();
        self.subscribe(
            -> \val { $c.send(val) },
            { $c.close },
            -> $ex { $c.fail($ex) });
        $c
    }

    method list() {
        # Use a Channel to handle any asynchrony.
        my $c = self.Channel;
        (1..*).map(sub ($) {
            select(
                $c        => -> \val { return val },
                $c.closed => -> $p { $p.result; last }
            )
        })
    }

    method do(&side_effect) { SubscribableOperations.do(self, &side_effect) }
    method grep(&filter)    { SubscribableOperations.grep(self, &filter) }
    method map(&mapper)     { SubscribableOperations.map(self, &mapper) }
    method merge($s)        { SubscribableOperations.merge(self, $s) }
    method zip($s, *@with)  { SubscribableOperations.zip(self, $s, |@with) }
}

# The on meta-combinator provides a mechanism for implementing thread-safe
# combinators on Subscribables. It subscribes to a bunch of sources, but will
# only let one of the specified callbacks to handle their next/last/fail run
# at a time. A little bit actor-like.
my class X::Subscribable::On::BadSetup is Exception {
    method message() {
        "on requires a callable that returns a list of pairs with Subscribable keys"
    }
}
my class X::Subscribable::On::NoNext is Exception {
    method message() {
        "on requires that next be specified for each subscribable"
    }
}
sub on(&setup) {
    my class OnSubscribable does Subscribable {
        has &!setup;
        
        submethod BUILD(:&!setup) { }

        method !add_source($source, $lock, :&next, :&last is copy, :&fail is copy) {
            unless defined &next {
                X::Subscribable::On::NoNext.new.throw;
            }
            unless defined &last {
                &last = { self.last }
            }
            unless defined &fail {
                &fail = -> $ex { self.fail($ex) }
            }
            $source.subscribe(
                -> \val {
                    $lock.run({ next(val) });
                    CATCH { self.fail($_) }
                },
                {
                    $lock.run({ last() });
                    CATCH { self.fail($_) }
                },
                -> $ex {
                    $lock.run({ fail($ex) });
                    CATCH { self.fail($_) }
                }
            );
        }
        
        method subscribe(|c) {
            my $sub = self.Subscribable::subscribe(|c);
            my @subscriptions = &!setup(self);
            my $lock = Lock.new;
            for @subscriptions -> $ssn {
                unless $ssn ~~ Pair && $ssn.key ~~ Subscribable {
                    X::Subscribable::On::BadSetup.new.throw;
                }
                given $ssn.value {
                    when EnumMap {
                        self!add_source($ssn.key, $lock, |$ssn.value);
                    }
                    when Callable {
                        self!add_source($ssn.key, $lock, next => $ssn.value);
                    }
                    default {
                        X::Subscribable::On::BadSetup.new.throw;
                    }
                }
            }
            $sub
        }

        method next(\msg) {
            for self.subscriptions {
                .next().(msg)
            }
            Nil;
        }

        method last() {
            for self.subscriptions {
                if .last -> $l { $l() }
            }
            Nil;
        }

        method fail($ex) {
            for self.subscriptions {
                if .fail -> $t { $t($ex) }
            }
            Nil;
        }
    }

    OnSubscribable.new(:&setup)
}

# Operations we can do on Subscribables. Note, many of them need to compose
# the Subscribable role into classes they create along the way, so they must
# be declared outside of Subscribable.
my class SubscribableOperations is repr('Uninstantiable') {
    # Private versions of the methods to relay events to subscribers, used in
    # implementing various operations.
    my role PrivatePublishing {
        method !next(\msg) {
            for self.subscriptions {
                .next().(msg)
            }
            Nil;
        }

        method !last() {
            for self.subscriptions {
                if .last { .last().() }
            }
            Nil;
        }

        method !fail($ex) {
            for self.subscriptions {
                if .fail { .fail().($ex) }
            }
            Nil;
        }
    }
    
    method do($a, &side_effect) {
        on -> $res {
            $a => sub (\val) { side_effect(val); $res.next(val) }
        }
    }
    
    method grep(Subscribable $a, &filter) {
        my class GrepSubscribable does Subscribable does PrivatePublishing {
            has $!source;
            has &!filter;
            
            submethod BUILD(:$!source, :&!filter) { }
            
            method subscribe(|c) {
                my $sub = self.Subscribable::subscribe(|c);
                my $ssn = $!source.subscribe(
                    -> \val {
                        if (&!filter(val)) { self!next(val) }
                    },
                    { self!last(); },
                    -> $ex { self!fail($ex) }
                );
                $sub
            }
        }
        GrepSubscribable.new(:source($a), :&filter)
    }
    
    method map(Subscribable $a, &mapper) {
        my class MapSubscribable does Subscribable does PrivatePublishing {
            has $!source;
            has &!mapper;
            
            submethod BUILD(:$!source, :&!mapper) { }
            
            method subscribe(|c) {
                my $sub = self.Subscribable::subscribe(|c);
                my $ssn = $!source.subscribe(
                    -> \val {
                        self!next(&!mapper(val))
                    },
                    { self!last(); },
                    -> $ex { self!fail($ex) }
                );
                $sub
            }
        }
        MapSubscribable.new(:source($a), :&mapper)
    }
    
    method merge(Subscribable $a, Subscribable $b) {
        my $lasts = 0;
        on -> $res {
            $a => {
                next => sub ($val) { $res.next($val) },
                last => {
                    $res.last() if ++$lasts == 2;
                }
            },
            $b => {
                next => sub ($val) { $res.next($val) },
                last => {
                    $res.last() if ++$lasts == 2;
                }
            }
        }
    }
    
    method zip(Subscribable $a, Subscribable $b, &with = &infix:<,>) {
        my @as;
        my @bs;
        on -> $res {
            $a => sub ($val) {
                @as.push($val);
                if @as && @bs {
                    $res.next(with(@as.shift, @bs.shift));
                }
            },
            $b => sub ($val) {
                @bs.push($val);
                if @as && @bs {
                    $res.next(with(@as.shift, @bs.shift));
                }
            }
        }
    }
}

# Something that can be subscribed to, and enables messages to be published to
# all subscribers. Note that there are no replay semantics; anybody who will
# subscribe too late will miss things.
my class Publisher does Subscribable {
    method next(\msg) {
        for self.subscriptions {
            .next().(msg)
        }
        Nil;
    }

    method last() {
        for self.subscriptions {
            if .last -> $l { $l() }
        }
        Nil;
    }

    method fail($ex) {
        for self.subscriptions {
            if .fail -> $t { $t($ex) }
        }
        Nil;
    }
}

# Various generators and combinators are provided by Publish.
my class Publish {
    method for(*@values, :$scheduler = $*SCHEDULER) {
        my class ForSubscribable does Subscribable {
            has @!values;
            has $!scheduler;

            submethod BUILD(:@!values, :$!scheduler) {}

            method subscribe(|c) {
                my $sub = self.Subscribable::subscribe(|c);
                $!scheduler.schedule_with_catch(
                    {
                        for @!values -> \val {
                            $sub.next().(val);
                        }
                        if $sub.last -> $l { $l() }
                    },
                    -> $ex { if $sub.fail -> $t { $t($ex) } }
                );
                $sub
            }
        }
        ForSubscribable.new(:@values, :$scheduler)
    }

    method interval($interval, $delay = 0, :$scheduler = $*SCHEDULER) {
        my class IntervalSubscribable does Subscribable {
            has $!scheduler;
            has $!interval;
            has $!delay;

            submethod BUILD(:$!scheduler, :$!interval, :$!delay) {}

            method subscribe(|c) {
                my $sub = self.Subscribable::subscribe(|c);
                $!scheduler.schedule_every(
                    {
                        state $i = 0;
                        $sub.next().($i++);
                    },
                    $!interval, $!delay
                );
                $sub
            }
        }
        IntervalSubscribable.new(:$interval, :$delay, :$scheduler)
    }
}

# A KeyReducer provides a thread-safe way to compose a hash from multiple
# sources.
my class X::KeyReducer::ResultObtained is Exception {
    method message() { "Cannot contribute to a KeyReducer after the result has been obtained" }
}
my class KeyReducer {
    has $!initializer;
    has $!reducer;
    has %!result;
    has Mu $!lock;
    has $!exception;
    has $!obtained;
    
    method new($initializer, $reducer) {
        self.bless(:$initializer, :$reducer)
    }
    
    my Mu $interop;
    my Mu $ReentrantLock;
    submethod BUILD(:$!initializer, :$!reducer) {
        unless nqp::isconcrete($interop) {
            $interop := nqp::jvmbootinterop();
            $ReentrantLock := $interop.typeForName('java.util.concurrent.locks.ReentrantLock');
        }
        $!lock := $ReentrantLock.'constructor/new/()V'();
        $!obtained = False;
    }
    
    proto method contribute(|) { * }
    multi method contribute(KeyReducer:D: %h) {
        $!lock.lock();
        if $!exception {
            $!lock.unlock();
            return False;
        }
        if $!obtained {
            $!lock.unlock();
            X::KeyReducer::ResultObtained.new.throw
        }
        try {
            for %h.kv -> $k, $v {
                %!result{$k} = %!result.exists($k)
                    ?? $!reducer(%!result{$k}, $v)
                    !! $!initializer($v)
            }
            CATCH { default { $!exception := $_ } }
        }
        $!lock.unlock();
        True
    }
    multi method contribute(KeyReducer:D: Pair $p) {
        $!lock.lock();
        if $!exception {
            $!lock.unlock();
            return False;
        }
        if $!obtained {
            $!lock.unlock();
            X::KeyReducer::ResultObtained.new.throw
        }
        try {
            %!result{$p.key} = %!result.exists($p.key)
                    ?? $!reducer(%!result{$p.key}, $p.value)
                    !! $!initializer($p.value);
            CATCH { default { $!exception := $_ } }
        }
        $!lock.unlock();
        True
    }
    
    method snapshot(KeyReducer:D:) {
        $!lock.lock();
        if $!exception {
            $!lock.unlock();
            $!exception.throw;
        }
        my %snapshot = %!result;
        $!lock.unlock();
        %snapshot
    }
    
    method result(KeyReducer:D:) {
        $!lock.lock();
        $!obtained = True;
        $!lock.unlock();
        $!exception ?? $!exception.throw !! %!result
    }
}

# Very basic asynchronous I/O support for files. Work in progress. Things that
# would nomally return something scalar-ish produce a Promise. Things that
# would normally return a (lazy) list produce a Channel.
my class IO::Async::File {
    has $!PIO;
    has $.chomp = Bool::True;
    has $.path;
    
    proto method open(|) { * }
    multi method open($path? is copy, :$r, :$w, :$a, :$bin, :$chomp = Bool::True,
            :enc(:$encoding) = 'utf8') {
        $path //= $!path;
        my $mode = $w ?? 'w' !! ($a ?? 'wa' !! 'r' );
        nqp::bindattr(self, IO::Async::File, '$!PIO',
             nqp::openasync(nqp::unbox_s($path.Str), nqp::unbox_s($mode))
        );
        $!path = $path;
        $!chomp = $chomp;
        nqp::setencoding($!PIO, $bin ?? 'binary' !! NORMALIZE_ENCODING($encoding));
        self;
    }

    method close() {
        nqp::closefh($!PIO);
        Bool::True;
    }
    
    method opened() {
        nqp::p6bool(nqp::istrue($!PIO));
    }
    
    method slurp(:$bin, :enc($encoding)) {
        self.open(:r, :$bin) unless self.opened;
        self.encoding($encoding) if $encoding.defined;

        if $bin {
            die "Asynchronous binary file reading NYI"
        }
        else {
            my $p = Promise.new;
            nqp::slurpasync($!PIO, Str,
                -> $str { $p.keep($str); self.close(); },
                -> $msg { $p.break($msg); try self.close(); });
            $p
        }
    }
    
    method lines(:enc($encoding)) {
        self.open(:r) unless self.opened;
        self.encoding($encoding) if $encoding.defined;

        my $c := Channel.new;
        nqp::linesasync($!PIO, Str, $.chomp ?? 1 !! 0,
            nqp::getattr($c, Channel, '$!queue'),
            -> { $c.finish(); self.close() },
            -> $msg { $c.fail($msg); try self.close(); });
        $c
    }
}

# Waits for a promise to be kept or a channel to be able to receive a value
# and, once it can, unwraps or returns the result. This should be made more
# efficient by using continuations to suspend any task running in the thread
# pool that blocks; for now, this cheat gets the basic idea in place.
proto sub await(|) { * }
multi sub await(Promise $p) {
    $p.result
}
multi sub await(*@awaitables) {
    @awaitables.eager.map(&await)
}
multi sub await(Channel $c) {
    $c.receive
}

# Takes a list of pairs, mapping a Channel or Promise to code. Invokes the
# code block of whichever Channel receives first whichever Promise is kept
# or broken first. Evaluates to the result of that code block.
# If none of the channels have a value or none of the promises have a result,
# then the default block is ran. If there is no default block, select() blocks
# until one channel or promise is ready.
# If more than one channel/promise is ready, select() picks one at random
proto sub select(|) { * }
multi sub select(*@selectors, :$default) {
    multi is-ready(Promise $p) {
        if $p.has_result {
            return (True, $p)
        }
        return (False, False)
    }

    multi is-ready(Channel $c) {
        my $selected is default(Nil) = $c.poll;
        unless $selected === Nil {
            return (True, $selected)
        }
        return (False, False)
    }
    multi is-ready(Any $c) {
        die "Cannot use select on a " ~ .^name;
    }

    my $choice;
    loop {
        my @ready;
        my @waiting;
        for @selectors -> $s {
            die "select expects to be passed a list of pairs" unless $s ~~ Pair;
            my $arg = is-ready($s.key);
            if $arg[0] {
                @ready.push: $s.value => $arg[1]
            } else {
                @waiting.push: $s
            }
        }
        if @ready {
            $choice = @ready.pick;
            last;
        } elsif $default {
            $choice = $default;
            last;
        }
        else {
            Thread.yield;
        }
    }
    nqp::istype($choice, Pair)
        ?? $choice.key.($choice.value)
        !! $choice.()
}
