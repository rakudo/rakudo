# This file contains early work on threading support for Rakudo on the JVM.
# The implementation is clearly VM specific, however the aim is to iterate
# towards a backend-independent API.

# Thread represents an OS-level thread. While it could be used directly, it
# is not the preferred way to work in Perl 6. It's a building block for the
# interesting things.
my class Thread {
    # Things we will use from the JVM.
    my $interop   := nqp::jvmbootinterop();
    my \JVMThread := $interop.typeForName('java.lang.Thread');
    
    # This thread's underlying JVM thread object.
    has Mu $!jvm_thread;
    
    # Is the thread's lifetime bounded by that of the application, such
    # that when it exits, so does the thread?
    has $.app_lifetime;
   
    submethod BUILD(:&code!, :$!app_lifetime) {
        $!jvm_thread := JVMThread."constructor/new/(Ljava/lang/Runnable;)V"(
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
my enum PromiseStatus (:Planned(0), :Running(1), :Completed(2), :Failed(3));
my class Promise {
    # Things we will use from the JVM.
    my $interop   := nqp::jvmbootinterop();
    my \Semaphore := $interop.typeForName('java.util.concurrent.Semaphore');
    
    has $.scheduler;
    has $.status;
    has $!result;
    has Mu $!ready_semaphore;
    
    submethod BUILD(:$!scheduler!, :&code!) {
        $!status = Planned;
        $!ready_semaphore := Semaphore.'constructor/new/(I)V'(-1);
        $!scheduler.schedule({
            $!status = Running;
            $!result = code();
            $!status = Completed;
            $!ready_semaphore.'method/release/(I)V'(32768);
            CATCH {
                default {
                    $!result = $_;
                    $!status = Failed;
                    $!ready_semaphore.'method/release/(I)V'(32768);
                }
            }
        })
    }
    
    method result() {
        # One important missing optimization here is that if the promise is
        # not yet started, then the work can be done immediately by the
        # thing that is blocking on it.
        if $!status == none(Failed, Completed) {
            $!ready_semaphore.'method/acquire/()V'();
        }
        if $!status == Completed {
            $!result
        }
        elsif $!status == Failed {
            $!result.rethrow
        }
    }
}

# The ThreadPoolScheduler is a straightforward scheduler that maintains a
# pool of threads and schedules work items in the order they are added
# using them.
my class ThreadPoolScheduler {
    # Things we will use from the JVM.
    my $interop := nqp::jvmbootinterop();
    my \LinkedBlockingQueue := $interop.typeForName('java.util.concurrent.LinkedBlockingQueue');
    my \Semaphore := $interop.typeForName('java.util.concurrent.Semaphore');
    my \AtomicInteger := $interop.typeForName('java.util.concurrent.atomic.AtomicInteger');
    
    # A concurrent work queue that blocks any worker threads that poll it
    # when empty until some work arrives.
    has Mu $!queue;
    
    # Semaphore to ensure we don't start more than the maximum number of
    # threads allowed.
    has Mu $!thread_start_semaphore;
    
    # Atomic integer roughly tracking outstanding work, used for rough
    # management of the pool size.
    has Mu $!outstanding;
    
    # Adds a new thread to the pool, respecting the maximum.
    method !maybe_new_thread() {
        if $!thread_start_semaphore.'method/tryAcquire/(I)Z'(1) {
            Thread.start(:app_lifetime, {
                loop {
                    my Mu $task := $interop.javaObjectToSixmodel($!queue.take());
                    $task();
                    $!outstanding.decrementAndGet();
                }
            });
        }
    }
    
    submethod BUILD(:$initial_threads = 1, :$max_threads = 4) {
        die "Must have at least one thread pool thread"
            if $initial_threads < 1;
        die "Initial thread pool threads must be less than or equal to maximim threads"
            if $initial_threads > $max_threads;
        $!queue := LinkedBlockingQueue.'constructor/new/()V'();
        $!thread_start_semaphore := Semaphore.'constructor/new/(I)V'($max_threads.Int);
        $!outstanding := AtomicInteger.'constructor/new/()V'();
        self!maybe_new_thread() for 1..$initial_threads;
    }
    
    method schedule(&code) {
        self!maybe_new_thread()
            if $!outstanding.incrementAndGet() > 1;
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
