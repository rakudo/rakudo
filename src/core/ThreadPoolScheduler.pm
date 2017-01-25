# The ThreadPoolScheduler is a straightforward scheduler that maintains a
# pool of threads and schedules work items in the order they are added
# using them.

my class ThreadPoolScheduler does Scheduler {
    constant THREAD_POOL_PROMPT = Mu.new;

    class ThreadPoolAwaiter does Awaiter {
        has $!queue;

        submethod BUILD(:$queue!) {
            $!queue := nqp::decont($queue);
        }

        method await(Awaitable:D $a) {
            my $handle := $a.get-await-handle;
            if $handle.already {
                $handle.success
                    ?? $handle.result
                    !! $handle.cause.rethrow
            }
            else {
                my $success;
                my $result;
                nqp::continuationcontrol(0, THREAD_POOL_PROMPT, -> Mu \c {
                    $handle.subscribe-awaiter(-> \success, \result {
                        $success := success;
                        $result := result;
                        nqp::push($!queue, { nqp::continuationinvoke(c, nqp::null()) });
                        Nil
                    });
                });
                $success
                    ?? $result
                    !! $result.rethrow
            }
        }

        method await-all(Iterable:D \i) {
            # Collect results that are already available, and handles where the
            # results are not yet available together with the matching insertion
            # indices.
            my \results = nqp::list();
            my \handles = nqp::list();
            my \indices = nqp::list_i();
            my int $insert = 0;
            for i -> $awaitable {
                unless nqp::istype($awaitable, Awaitable) {
                    die "Can only specify Awaitable objects to await (got a $awaitable.^name())";
                }
                unless nqp::isconcrete($awaitable) {
                    die "Must specify a defined Awaitable to await (got an undefined $awaitable.^name())";
                }

                my $handle := $awaitable.get-await-handle;
                if $handle.already {
                    $handle.success
                        ?? nqp::bindpos(results, $insert, $handle.result)
                        !! $handle.cause.rethrow
                }
                else {
                    nqp::push(handles, $handle);
                    nqp::push_i(indices, $insert);
                }

                $insert++;
            }

            # See if we have anything that we really need to suspend for. If
            # so, we need to take great care that the continuation taking is
            # complete before we try to resume it (completions can happen on
            # different threads, and so concurrent with us subscribing, not
            # to mention concurrent with each other wanting to resume). We
            # use a lock to take care of this, holding the lock until the
            # continuation has been taken.
            my int $num-handles = nqp::elems(handles);
            if $num-handles {
                my $continuation;
                my $exception;
                my $l = Lock.new;
                $l.lock;
                {
                    my int $remaining = $num-handles;
                    loop (my int $i = 0; $i < $num-handles; $i++) {
                        my $handle := nqp::atpos(handles, $i);
                        my int $insert = nqp::atpos_i(indices, $i);
                        $handle.subscribe-awaiter(-> \success, \result {
                            my int $resume;
                            $l.protect: {
                                if success && $remaining {
                                    nqp::bindpos(results, $insert, result);
                                    --$remaining;
                                    $resume = 1 unless $remaining;
                                }
                                elsif !nqp::isconcrete($exception) {
                                    $exception := result;
                                    $remaining = 0;
                                    $resume = 1;
                                }
                            }
                            if $resume {
                                nqp::push($!queue, {
                                    nqp::continuationinvoke($continuation, nqp::null())
                                });
                            }
                        });
                    }
                    CATCH {
                        # Unlock if we fail here, and let the exception
                        # propagate outwards.
                        $l.unlock();
                    }
                }
                nqp::continuationcontrol(0, THREAD_POOL_PROMPT, -> Mu \c {
                    $continuation := c;
                    $l.unlock;
                });

                # If we got an exception, throw it.
                $exception.rethrow if nqp::isconcrete($exception);
            }

            nqp::p6bindattrinvres(nqp::create(List), List, '$!reified', results);
        }
    }

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
                my $*AWAITER := ThreadPoolAwaiter.new(:$!queue);
                loop {
                    my Mu $task := nqp::shift($!queue);
                    $!counts_lock.protect: { $!loads = $!loads + 1 };
                    nqp::continuationreset(THREAD_POOL_PROMPT, {
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
                    });
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
            $delay = to-millis($delay) if $delay;
            @async_handles.push(
              nqp::timer($!queue, $todo, $delay, 0, TimerCancellation)
            ) for 1 .. $times;
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
