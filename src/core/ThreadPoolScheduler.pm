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
                            my \args = nqp::p6bindattrinvres(nqp::create(List), List, '$!reified', $task);
                            $code(|args);
                        }
                        else {
                            $task();
                        }
                        CONTROL {
                            default {
                                my Mu $vm-ex := nqp::getattr(nqp::decont($_), Exception, '$!ex');
                                nqp::getcomp('perl6').handle-control($vm-ex);
                            }
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
        Int :$!max_threads = (%*ENV<RAKUDO_MAX_THREADS> // 16).Int
        --> Nil
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

    method cue(&code, :$at, :$in, :$every, :$times = 1, :&stop is copy, :&catch ) {
        my class TimerCancellation is repr('AsyncTask') { }
        die "Cannot specify :at and :in at the same time"
          if $at.defined and $in.defined;
        die "Cannot specify :every, :times and :stop at the same time"
          if $every.defined and $times > 1 and &stop;
        my $delay = $at ?? $at - now !! $in // 0;
        self!initialize unless $!started_any;

        # need repeating
        if $every {

            # generate a stopper if needed
            if $times > 1 {
                my $todo = $times;
                &stop = sub { $todo ?? !$todo-- !! True }
            }

            # we have a stopper
            if &stop {
                my $handle;
                my $cancellation;
                sub cancellation() {
                    $cancellation //=
                      Cancellation.new(async_handles => [$handle]);
                }
                $handle := nqp::timer($!queue,
                    &catch
                      ?? -> {
                          stop()
                            ?? cancellation().cancel
                            !! code();
                          CATCH { default { catch($_) } };
                      }
                      !! -> {
                          stop()
                            ?? cancellation().cancel
                            !! code();
                      },
                    to-millis($delay), to-millis($every),
                    TimerCancellation);
                self!maybe_new_thread();
                return cancellation()
            }

            # no stopper
            else {
                my $handle := nqp::timer($!queue,
                    &catch
                      ?? -> { code(); CATCH { default { catch($_) } } }
                      !! &code,
                    to-millis($delay), to-millis($every),
                    TimerCancellation);
                self!maybe_new_thread();
                return Cancellation.new(async_handles => [$handle]);
            }
        }

        # only after waiting a bit or more than once
        elsif $delay or $times > 1 {
            my $todo := &catch
                ?? -> { code(); CATCH { default { catch($_) } } }
                !! &code;
            my @async_handles;
            for 1 .. $times {
                @async_handles.push(nqp::timer($!queue, $todo,
                    to-millis($delay), 0, TimerCancellation));
                $delay = 0;
            }
            self!maybe_new_thread();
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

    multi to-millis(Int $value) {
        1000 * $value
    }
    multi to-millis(Numeric $value) {
        my $proposed = (1000 * $value).Int;
        if $value && $proposed == 0 {
            warn "Minimum timer resolution is 1ms; using that instead of {1000 * $value}ms";
            $proposed = 1;
        }
        $proposed
    }
    multi to-millis($value) {
        to-millis(+$value)
    }

    method !initialize() {
        $!queue                  := nqp::create(Queue);
        $!thread_start_semaphore := Semaphore.new($!max_threads.Int);
        $!counts_lock             := nqp::create(Lock);
        self!maybe_new_thread() for 1..$!initial_threads;
    }
}

# This thread pool scheduler will be the default one.
Rakudo::Internals.REGISTER-DYNAMIC: '$*SCHEDULER', {
    PROCESS::<$SCHEDULER> = ThreadPoolScheduler.new();
}

# vim: ft=perl6 expandtab sw=4
