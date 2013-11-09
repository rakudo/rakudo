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
    has Mu $!loads;
    
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
                    $!loads.decrementAndGet();
                }
            });
        }
    }
    
    submethod BUILD(:$!initial_threads = 0, :$!max_threads = 16) {
        die "Initial thread pool threads must be less than or equal to maximum threads"
            if $!initial_threads > $!max_threads;
    }
    
    method cue(&code, :$at, :$in, :$every ) {
        die "Cannot specify :at and :in at the same time"
          if $at.defined and $in.defined;
        my $delay = $at ?? $at - now !! $in // 0;
        self!initialize unless $!started_any;

        # need repeating
        if $every {
            $!timer.'method/scheduleAtFixedRate/(Ljava/util/TimerTask;JJ)V'(
              nqp::jvmbootinterop().proxy(
                'java.util.TimerTask', nqp::hash('run', -> { code() })),
              ($delay * 1000).Int,
              ($every * 1000).Int);
        }

        # only after waiting a bit
        elsif $delay {
            $!timer.'method/schedule/(Ljava/util/TimerTask;J)V'(
              nqp::jvmbootinterop().proxy(
                'java.util.TimerTask', nqp::hash('run', -> { code() })),
              ($delay * 1000).Int);
        }

        # just cue the code
        else {
            my $loads = $!loads.incrementAndGet();
            self!maybe_new_thread()
                if !$!started_any || $loads > 1;
            $!queue.add(nqp::jvmbootinterop().sixmodelToJavaObject(&code));
        }
    }

    method loads() {
        $!loads.get()
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
        $!loads                  := AtomicInteger.'constructor/new/()V'();
        $!timer                  := Timer.'constructor/new/(Z)V'(True);
        self!maybe_new_thread() for 1..$!initial_threads;
    }
}

# This thread pool scheduler will be the default one.
$PROCESS::SCHEDULER = ThreadPoolScheduler.new();
