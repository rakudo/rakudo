# The ThreadPoolScheduler is a straightforward scheduler that maintains a
# pool of threads and schedules work items in the order they are added
# using them.

my class ThreadPoolScheduler does Scheduler {
    # A concurrent work queue that blocks any worker threads that poll it
    # when empty until some work arrives.
    my class Queue is repr('ConcBlockingQueue') { }
    has $!queue;
    
    # Semaphore to ensure we don't start more than the maximum number of
    # threads allowed.
    has $!thread_start_semaphore;
    
    # Number of outstanding work items, used for rough management of the
    # pool size.
    has int $!loads;

    # Number of threads started so far.
    has int $!threads_started;

    # Lock protecting updates to the above 2 fields.
    has $!counts_lock;

    # If we've got incoming I/O events we need a thread to handle.
    has int $!need_io_thread;
    
    # Initial and maximum threads.
    has Int $.initial_threads;
    has Int $.max_threads;
    
    # Have we started any threads yet?
    has int $!started_any;

    # Adds a new thread to the pool, respecting the maximum.
    method !maybe_new_thread() {
        if $!thread_start_semaphore.try_acquire() {
            $!started_any = 1;
            $!counts_lock.protect: { $!threads_started = $!threads_started + 1 };
            Thread.start(:app_lifetime, {
                loop {
                    my Mu $task := nqp::shift($!queue);
                    $!counts_lock.protect: { $!loads = $!loads + 1 };
                    try {
                        if nqp::islist($task) {
                            my Mu $code := nqp::shift($task);
                            $code(|nqp::p6parcel($task, Any));
                        }
                        else {
                            $task();
                        }
                        CATCH {
                            default {
                                self.handle_uncaught($_)
                            }
                        }
                    }
                    $!counts_lock.protect: { $!loads = $!loads - 1 };
                }
            });
        }
    }
    
    submethod BUILD(
        Int :$!initial_threads = 0,
        Int :$!max_threads = (%*ENV<RAKUDO_MAX_THREADS> // 16).Int,
    ) {
        die "Initial thread pool threads ($!initial_threads) must be less than or equal to maximum threads ($!max_threads)"
            if $!initial_threads > $!max_threads;
    }

    method queue() {
        self!initialize unless $!started_any;
        self!maybe_new_thread();
        $!need_io_thread = 1;
        $!queue
    }

    method cue(&code, :$at, :$in, :$every, :$times = 1, :&catch ) {
        my class TimerCancellation is repr('AsyncTask') { }
        die "Cannot specify :at and :in at the same time"
          if $at.defined and $in.defined;
        die "Cannot specify :every and :times at the same time"
          if $every.defined and $times > 1;
        my $delay = $at ?? $at - now !! $in // 0;
        self!initialize unless $!started_any;

        # need repeating
        if $every {
            my $handle := nqp::timer($!queue,
                &catch
                  ?? -> { code(); CATCH { default { catch($_) } } }
                  !! &code,
                ($delay * 1000).Int, ($every * 1000).Int,
                TimerCancellation);
            self!maybe_new_thread() if !$!started_any;
            return Cancellation.new(async_handles => [$handle]);
        }

        # only after waiting a bit or more than once
        elsif $delay or $times > 1 {
            my $todo := &catch
                ?? -> { code(); CATCH { default { catch($_) } } }
                !! &code;
            my @async_handles;
            for 1 .. $times {
                @async_handles.push(nqp::timer($!queue, $todo,
                    ($delay * 1000).Int, 0, TimerCancellation));
                $delay = 0;
            }
            self!maybe_new_thread() if !$!started_any;
            return Cancellation.new(:@async_handles);
        }

        # just cue the code
        else {
            my &run := &catch 
               ?? -> { code(); CATCH { default { catch($_) } } }
               !! &code;
            self!maybe_new_thread() if $!loads + $!need_io_thread <= $!threads_started;
            nqp::push($!queue, &run);
            return Nil;
        }
    }

    method loads() {
        return 0 unless $!started_any;
        $!loads
    }

    method !initialize() {
        $!queue                  := nqp::create(Queue);
        $!thread_start_semaphore := Semaphore.new($!max_threads.Int);
        $!counts_lock             := nqp::create(Lock);
        self!maybe_new_thread() for 1..$!initial_threads;
    }
}

# This thread pool scheduler will be the default one.
$PROCESS::SCHEDULER = ThreadPoolScheduler.new();

# vim: ft=perl6 expandtab sw=4
