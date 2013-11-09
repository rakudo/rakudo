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
            Thread.start(:app_lifetime, {
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
    
    proto method cue(|) { * }
    multi method cue(&code) {
        self!initialize unless $!started_any;
        my $outstanding = $!outstanding.incrementAndGet();
        self!maybe_new_thread()
            if !$!started_any || $outstanding > 1;
        $!queue.add(nqp::jvmbootinterop().sixmodelToJavaObject(&code));
    }
    multi method cue(&code, :$in!) {
        self!cue_in(&code, $in, Any);
    }
    multi method cue(&code, :$every!, :$in = 0) {
        self!cue_in(&code, $in, $every);
    }
    multi method cue(&code, :$at!, :$every, :$in) {
        die "Cannot specify :at and :in at the same time" if $in.defined;
        self!cue_in(&code, $at - now, $every);
    }

    method outstanding() {
        $!outstanding.get()
    }

    method !cue_in(&code, $in, $every) {
        self!initialize() unless $!started_any;
        if $every {
            $!timer.'method/scheduleAtFixedRate/(Ljava/util/TimerTask;JJ)V'(
              nqp::jvmbootinterop().proxy(
                'java.util.TimerTask', nqp::hash('run', -> { code() })),
              ($in * 1000).Int,
              ($every * 1000).Int);
        }
        else {
            $!timer.'method/scheduleAtFixedRate/(Ljava/util/TimerTask;JJ)V'(
              nqp::jvmbootinterop().proxy(
                'java.util.TimerTask', nqp::hash('run', -> { code() })),
              ($in * 1000).Int);
        }
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
