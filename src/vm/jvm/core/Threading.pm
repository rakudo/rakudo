# This file contains early work on threading support for Rakudo on the JVM.
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
    has $.app_lifetime;
   
    submethod BUILD(:&code!, :$!app_lifetime) {
        my $interop   := nqp::jvmbootinterop();
        my \JVMThread := $interop.typeForName('java.lang.Thread');
        $!jvm_thread  := JVMThread."constructor/new/(Ljava/lang/Runnable;)V"(
            $interop.proxy('java.lang.Runnable', nqp::hash('run', nqp::decont(&code))));
        $!jvm_thread.setDaemon(1) if $!app_lifetime;
    }
    
    method start(&code, :$app_lifetime) {
        Thread.new(:&code, :$app_lifetime).run()
    }
    
    method run(Thread:D:) {
        $!jvm_thread.start();
        self
    }
    
    method join(Thread:D:) {
        $!jvm_thread.'method/join/()V'();
        self
    }
}

# A promise represents a piece of asynchronous work, which may be in progress,
# completed or even yet to start. Typically, a promise is created using the
# C<async> function.
my enum PromiseStatus (:Planned(0), :Running(1), :Kept(2), :Broken(3));
my class X::Promise::Code is Exception {
    has $.attempted;
    method message() { "Can not $!attempted a code-based promise" }
}
my class X::Promise::Combinator is Exception {
    has $.combinator;
    method message() { "Can only use $!combinator to combine other Promise objects" }
}
my class X::Promise::CauseOnlyValidOnBroken is Exception {
    method message() { "Can only call cause on a broken promise" }
}
my class Promise {
    has $.scheduler;
    has $.status;
    has &!code;
    has $!result;
    has @!thens;
    has Mu $!ready_semaphore;
    has Mu $!then_lock;
    
    submethod BUILD(:$!scheduler = $*SCHEDULER, :&!code, :$scheduled = True) {
        my $interop       := nqp::jvmbootinterop();
        my \Semaphore     := $interop.typeForName('java.util.concurrent.Semaphore');
        my \ReentrantLock := $interop.typeForName('java.util.concurrent.locks.ReentrantLock');
        $!status = Planned;
        $!ready_semaphore := Semaphore.'constructor/new/(I)V'(-1);
        $!then_lock := ReentrantLock.'constructor/new/()V'();
        self!schedule() if &!code && $scheduled;
    }
    
    method !schedule() {
        $!scheduler.schedule({
            $!status = Running;
            self!keep(&!code());
            CATCH {
                default {
                    self!break($_);
                }
            }
        })
    }
    
    method keep(Promise:D: $result) {
        X::Promise::Code.new(attempted => 'keep').throw if &!code;
        self!keep($result)
    }
    
    method !keep($!result) {
        $!status = Kept;
        $!ready_semaphore.'method/release/(I)V'(32768);
        self!schedule_thens();
        $!result
    }
    
    method break(Promise:D: $result) {
        X::Promise::Code.new(attempted => 'break').throw if &!code;
        self!break($result ~~ Exception ?? $result !! X::AdHoc.new(payload => $result))
    }
    
    method !break($!result) {
        $!status = Broken;
        $!ready_semaphore.'method/release/(I)V'(32768);
        self!schedule_thens();
    }
    
    method !schedule_then($fulfilled) {
        my $orig_code = &!code;
        &!code = { $orig_code($fulfilled) }
        self!schedule();
    }
    
    method !schedule_thens() {
        $!then_lock.lock();
        while @!thens {
            @!thens.shift()!schedule_then(self)
        }
        $!then_lock.unlock();
    }
    
    method result(Promise:D:) {
        # One important missing optimization here is that if the promise is
        # not yet started, then the work can be done immediately by the
        # thing that is blocking on it.
        if $!status == none(Broken, Kept) {
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
    
    method cause(Promise:D:) {
        if $!status == Broken {
            $!result
        } else {
            X::Promise::CauseOnlyValidOnBroken.new.throw
        }
    }
    
    method then(Promise:D: &code) {
        $!then_lock.lock();
        if $!status == any(Broken, Kept) {
            # Already have the result, schedule immediately.
            $!then_lock.unlock();
            Promise.new(:$!scheduler, :code({ code(self) }))
        }
        else {
            # Create a (currently unscheduled) promise and add it to
            # the list.
            my $then_promise = Promise.new(:$!scheduler, :code(&code), :!scheduled);
            @!thens.push($then_promise);
            $!then_lock.unlock();
            $then_promise
        }
    }
    
    my Mu $timer;
    method sleep(Promise:U: $seconds) {
        once {
            my Mu \Timer := nqp::jvmbootinterop().typeForName('java.util.Timer');
            $timer       := Timer.'constructor/new/(Z)V'(True);
            Nil;
        }
        my $p = Promise.new;
        $timer.'method/schedule/(Ljava/util/TimerTask;J)V'(
            nqp::jvmbootinterop().proxy(
                'java.util.TimerTask',
                nqp::hash('run', -> { $p.keep(True) })),
            ($seconds * 1000).Int);
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
        for @promises {
            .then({
                if .status == Kept {
                    if $c.'decrementAndGet'() == 0 {
                        $p.keep(Nil)
                    }
                }
                else {
                    $p.break(.cause)
                }
            })
        }
        $p
    }
}

# A channel provides a thread-safe way to send a series of values from some
# producer(s) to some consumer(s).
my class X::Channel::SendOnCompleted is Exception {
    method message() { "Cannot send a message on a completed channel" }
}
my class X::Channel::ReceiveOnCompleted is Exception {
    method message() { "Cannot receive a message on a completed channel" }
}
my class Channel {
    # The queue of events moving through the channel.
    has Mu $!queue;
    
    # Promise that is triggered on channel completion.
    has $!completion;
    
    # Flag for if the channel is completed.
    has $!completed;
    
    # Magical objects for various ways a channel can end.
    my class CHANNEL_FINISH { }
    my class CHANNEL_FAIL   { has $.error }
    
    my Mu $interop;
    submethod BUILD() {
        $interop := nqp::jvmbootinterop() unless nqp::isconcrete($interop);
        my \LinkedBlockingQueue := $interop.typeForName('java.util.concurrent.LinkedBlockingQueue');
        $!queue := LinkedBlockingQueue.'constructor/new/()V'();
        $!completion = Promise.new;
    }
    
    method send(Channel:D: \item) {
        X::Channel::SendOnCompleted.new.throw if $!completed;
        $!queue.add($interop.sixmodelToJavaObject(nqp::decont(item)))
    }
    
    method receive(Channel:D:) {
        my \msg := $interop.javaObjectToSixmodel($!queue.take());
        if nqp::istype(msg, CHANNEL_FINISH) {
            $!completion.keep(Nil);
            X::Channel::ReceiveOnCompleted.new.throw
        }
        elsif nqp::istype(msg, CHANNEL_FAIL) {
            $!completion.break(msg.error);
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
            if nqp::istype(msg, CHANNEL_FINISH) {
                $!completion.keep(Nil);
                Nil
            }
            elsif nqp::istype(msg, CHANNEL_FAIL) {
                $!completion.break(msg.error);
                Nil
            }
            else {
                msg
            }
        }
    }
    
    method finish() {
        $!completed = 1;
        $!queue.add($interop.sixmodelToJavaObject(CHANNEL_FINISH))
    }
    
    method fail($error) {
        $!completed = 1;
        $!queue.add($interop.sixmodelToJavaObject(CHANNEL_FAIL.new(:$error)))
    }
    
    method completion() {
        $!completion
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
        self.bless(*, :$initializer, :$reducer)
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

# The ThreadPoolScheduler is a straightforward scheduler that maintains a
# pool of threads and schedules work items in the order they are added
# using them.
my class ThreadPoolScheduler {
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
    
    # Adds a new thread to the pool, respecting the maximum.
    method !maybe_new_thread() {
        if $!thread_start_semaphore.'method/tryAcquire/(I)Z'(1) {
            my $interop := nqp::jvmbootinterop();
            $!started_any = 1;
            Thread.start(:app_lifetime, {
                loop {
                    my Mu $task := $interop.javaObjectToSixmodel($!queue.take());
                    $task();
                    $!outstanding.decrementAndGet();
                }
            });
        }
    }
    
    submethod BUILD(:$!initial_threads = 0, :$!max_threads = 4) {
        die "Initial thread pool threads must be less than or equal to maximim threads"
            if $!initial_threads > $!max_threads;
    }
    
    method schedule(&code) {
        my $interop := nqp::jvmbootinterop();
        unless $!started_any {
            # Things we will use from the JVM.
            my \LinkedBlockingQueue := $interop.typeForName('java.util.concurrent.LinkedBlockingQueue');
            my \Semaphore := $interop.typeForName('java.util.concurrent.Semaphore');
            my \AtomicInteger := $interop.typeForName('java.util.concurrent.atomic.AtomicInteger');
            $!queue := LinkedBlockingQueue.'constructor/new/()V'();
            $!thread_start_semaphore := Semaphore.'constructor/new/(I)V'($!max_threads.Int);
            $!outstanding := AtomicInteger.'constructor/new/()V'();
            self!maybe_new_thread() for 1..$!initial_threads;
        }
        self!maybe_new_thread()
            if !$!started_any || $!outstanding.incrementAndGet() > 0;
        $!queue.add($interop.sixmodelToJavaObject(&code));
    }
    
    method outstanding() {
        $!outstanding.get()
    }
}

# This thread pool scheduler will be the default one.
$PROCESS::SCHEDULER = ThreadPoolScheduler.new();

# Schedules a piece of asynchronous work using the current scheduler, and
# returns a promise that represents it.
sub async(&code) {
    Promise.new(:scheduler($*SCHEDULER), :&code);
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
# or broken first. Evaluates to the result of that code block. If none of
# the channels have a value or none of the promises have a result, blocks
# until one does.
proto sub select(|) { * }
multi sub select(*@selectors) {
    # XXX Crappy spinning implementation; do something better soon.
    my $found;
    my $arg;
    until $found {
        for @selectors -> $s {
            die "select expects to be passed a list of pairs" unless $s ~~ Pair;
            given $s.key {
                when Promise {
                    if .has_result {
                        $found := $s.value();
                        $arg   := $_;
                    }
                }
                when Channel {
                    my \selected := .poll;
                    unless selected === Nil {
                        $found := $s.value();
                        $arg   := selected;
                    }
                }
                default {
                    die "Cannot use select on a " ~ .^name;
                }
            }
        }
    }
    $found($arg)
}
