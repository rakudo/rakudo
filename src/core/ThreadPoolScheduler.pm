my class ThreadPoolScheduler does Scheduler {
    # A concurrent, blocking-on-receive queue.
    my class Queue is repr('ConcBlockingQueue') {
        method elems() { nqp::elems(self) }
    }

    # Scheduler debug, controlled by an environment variable.
    my $scheduler-debug = so %*ENV<RAKUDO_SCHEDULER_DEBUG>;
    sub scheduler-debug($message) {
        if $scheduler-debug {
            note "[SCHEDULER] $message";
        }
    }

    # Infrastructure for non-blocking `await` for code running on the
    # scheduler.
    my constant THREAD_POOL_PROMPT = Mu.new;
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

    # There are three kinds of worker:
    # * General worker threads all pull from the main queue. If they have no
    #   work, they may steal from timer threads.
    # * Timer worker threads are intended to handle time-based events. They
    #   pull events from the time-sensitive queue, and they will not do any
    #   work stealing so as to be ready and available for timer events. The
    #   time-sensitive queue will only be returned when a queue is requested
    #   with the :hint-time-sensitive named argument. Only one timer worker
    #   will be created on the first request for such a queue; the supervisor
    #   will then monitor the time-sensitive queue length and add more if
    #   needed.
    # * Affinity worker threads each have their own queue. They are used when
    #   a queue is requested and :hint-affinity is passed. These are useful
    #   for things like Proc::Async and IO::Socket::Async, where events will
    #   be processed using a Supply, which is serial, and so there's no point
    #   at all in contending over the data. Work will not be stolen from an
    #   affinity worker thread.
    my role Worker {
        has $.thread;
        has $!scheduler;

        # Completed is the number of tasks completed since the last time the
        # supervisor checked in.
        has atomicint $.completed;

        # Working is 1 if the worker is currently busy, 0 if not.
        has int $.working;

        # Resets the completed to zero.
        method take-completed() {
            my atomicint $taken;
            cas $!completed, -> atomicint $current { $taken = $current; 0 }
            $taken
        }

        method !run-one(\task) {
            $!working = 1;
            nqp::continuationreset(THREAD_POOL_PROMPT, {
                if nqp::istype(task, List) {
                    my Mu $code := nqp::shift(nqp::getattr(task, List, '$!reified'));
                    $code(|task);
                }
                else {
                    task.();
                }
                CONTROL {
                    default {
                        my Mu $vm-ex := nqp::getattr(nqp::decont($_), Exception, '$!ex');
                        nqp::getcomp('perl6').handle-control($vm-ex);
                    }
                }
                CATCH {
                    default {
                        $!scheduler.handle_uncaught($_)
                    }
                }
            });
            $!working = 0;
            $!completed⚛++;
        }
    }
    my class GeneralWorker does Worker {
        has Queue $!queue;

        submethod BUILD(Queue :$queue!, :$!scheduler!) {
            $!queue := $queue;
            $!thread = Thread.start(:app_lifetime, {
                my $*AWAITER := ThreadPoolAwaiter.new(:$!queue);
                loop {
                    self!run-one(nqp::shift($queue));
                }
            });
        }
    }
    my class TimerWorker does Worker {
        has Queue $!queue;

        submethod BUILD(Queue :$queue!, :$!scheduler!) {
            $!queue := $queue;
            $!thread = Thread.start(:app_lifetime, {
                my $*AWAITER := ThreadPoolAwaiter.new(:$!queue);
                loop {
                    self!run-one(nqp::shift($queue));
                }
            });
        }
    }
    my class AffinityWorker does Worker {
        has Queue $.queue;

        submethod BUILD(:$!scheduler!) {
            my $queue := $!queue := Queue.CREATE;
            $!thread = Thread.start(:app_lifetime, {
                my $*AWAITER := ThreadPoolAwaiter.new(:$!queue);
                loop {
                    self!run-one(nqp::shift($queue));
                }
            });
        }
    }

    # Initial and maximum threads allowed.
    has Int $.initial_threads;
    has Int $.max_threads;

    # All of the worker and queue state below is guarded by this lock.
    has Lock $!state-lock .= new;

    # The general queue and timer queue, if created.
    has Queue $!general-queue;
    has Queue $!timer-queue;

    # The current lists of workers. Immutable lists; new ones are produced
    # upon changes.
    has List $!general-workers = ();
    has List $!timer-workers = ();
    has List $!affinity-workers = ();

    # The supervisor thread, if started.
    has Thread $!supervisor;

    method !general-queue() {
        unless $!general-queue.DEFINITE {
            $!state-lock.protect: {
                unless $!general-queue.DEFINITE {
                    # We don't have any workers yet, so start one.
                    $!general-queue := nqp::create(Queue);
                    $!general-workers = (GeneralWorker.new(
                        queue => $!general-queue,
                        scheduler => self
                    ),);
                    scheduler-debug "Created initial general worker thread";
                    self!maybe-start-supervisor();
                }
            }
        }
        $!general-queue
    }

    method !timer-queue() {
        unless $!timer-queue.DEFINITE {
            $!state-lock.protect: {
                unless $!timer-queue.DEFINITE {
                    # We don't have any workers yet, so start one.
                    $!timer-queue := nqp::create(Queue);
                    $!timer-workers = (TimerWorker.new(
                        queue => $!timer-queue,
                        scheduler => self
                    ),);
                    scheduler-debug "Created initial timer worker thread";
                    self!maybe-start-supervisor();
                }
            }
        }
        $!timer-queue
    }

    constant @affinity-add-thresholds = 1, 5, 10, 20, 50, 100;
    method !affinity-queue() {
        # If there's no affinity workers, start one.
        my $cur-affinity-workers := $!affinity-workers;
        if $cur-affinity-workers.elems == 0 {
            $!state-lock.protect: {
                if $!affinity-workers.elems == 0 {
                    # We don't have any affinity workers yet, so start one
                    # and return its queue.
                    $!affinity-workers := (AffinityWorker.new(
                        scheduler => self
                    ),);
                    scheduler-debug "Created initial affinity worker thread";
                    self!maybe-start-supervisor();
                    return $!affinity-workers[0].queue;
                }
            }
            $cur-affinity-workers := $!affinity-workers; # lost race for first
        }

        # Otherwise, see which has the least load (this is inherently racey
        # and approximate, but enough to help us avoid a busy worker). If we
        # find an empty queue, return it immediately.
        my $most-free-worker;
        $cur-affinity-workers.map: -> $cand {
            if $most-free-worker.DEFINITE {
                my $queue = $cand.queue;
                return $queue if $queue.elems == 0;
                if $cand.elems < $most-free-worker.queue.elems {
                    $most-free-worker := $cand;
                }
            }
            else {
                $most-free-worker := $cand;
            }
        }

        # Otherwise, check if the queue beats the threshold to add another
        # worker thread.
        my $chosen-queue = $most-free-worker.queue;
        my $queue-elems = $chosen-queue.elems;
        my $threshold = @affinity-add-thresholds[
            ($cur-affinity-workers.elems max @affinity-add-thresholds) - 1
        ];
        if $chosen-queue.elems > $threshold {
            # Add another one, unless another thread did too.
            $!state-lock.protect: {
                if self!total-workers() >= $!max_threads {
                    scheduler-debug "Will not add extra affinity worker; hit $!max_threads thread limit";
                    return $chosen-queue;
                }
                if $cur-affinity-workers.elems != $!affinity-workers.elems {
                    return $chosen-queue;
                }
                my $new-worker = AffinityWorker.new(scheduler => self);
                $!affinity-workers = (|$!affinity-workers, $new-worker);
                scheduler-debug "Added an affinity worker thread";
                $new-worker.queue
            }
        }
        else {
            $chosen-queue
        }
    }

    # The supervisor sits in a loop, mostly sleeping. Each time it wakes up,
    # it takes stock of the current situation and decides whether or not to
    # add threads.
    my constant SUPERVISION_INTERVAL = 0.005;
    method !maybe-start-supervisor() {
        unless $!supervisor.DEFINITE {
            $!supervisor = Thread.start(:app_lifetime, {
                sub add-general-worker() {
                    $!state-lock.protect: {
                        $!general-workers := (|$!general-workers, GeneralWorker.new(
                            queue => $!general-queue,
                            scheduler => self
                        ));
                    }
                    scheduler-debug "Added a general worker thread";
                }
                sub add-timer-worker() {
                    $!state-lock.protect: {
                        $!timer-workers := (|$!timer-workers, TimerWorker.new(
                            queue => $!timer-queue,
                            scheduler => self
                        ));
                    }
                    scheduler-debug "Added a timer worker thread";
                }

                scheduler-debug "Supervisor started";
                my num $last-rusage-time = nqp::time_n;
                my int $last-usage = self!getrusage-total();
                my num @last-utils;
                my int $cpu-cores = nqp::cpucores();
                scheduler-debug "Supervisor thinks there are $cpu-cores CPU cores";
                loop {
                    # Wait until the next time we should check how things
                    # are.
                    sleep SUPERVISION_INTERVAL;

                    # Work out the delta of CPU usage since last supervison
                    # and the time period that measurement spans.
                    my num $now = nqp::time_n;
                    my num $rusage-period = $now - $last-rusage-time;
                    $last-rusage-time = $now;
                    my int $current-usage = self!getrusage-total();
                    my int $usage-delta = $current-usage - $last-usage;
                    $last-usage = $current-usage;

                    # Scale this by the time between rusage calls and turn it
                    # into a per-core utilization percentage.
                    my num $normalized-delta = $usage-delta / $rusage-period;
                    my num $per-core = $normalized-delta / $cpu-cores;
                    my num $per-core-util = 100 * ($per-core / 1000000);

                    # Since those values are noisy, average the last 5 to get
                    # a smoothed value.
                    @last-utils.shift if @last-utils == 5;
                    push @last-utils, $per-core-util;
                    my $smooth-per-core-util = [+](@last-utils) / @last-utils;
                    scheduler-debug "Per-core utilization (approx): $smooth-per-core-util%";

                    if $!general-queue.DEFINITE {
                        self!tweak-workers: $!general-queue, $!general-workers,
                            &add-general-worker, $cpu-cores, $smooth-per-core-util;
                    }
                    if $!timer-queue.DEFINITE {
                        self!tweak-workers: $!timer-queue, $!timer-workers,
                            &add-timer-worker, $cpu-cores, $smooth-per-core-util;
                    }
                    CATCH {
                        default {
                            scheduler-debug .gist;
                        }
                    }
                }
            });
        }
    }

    method !getrusage-total() {
        my \rusage = nqp::getrusage();
        nqp::atpos_i(rusage, nqp::const::RUSAGE_UTIME_SEC) * 1000000 +
            nqp::atpos_i(rusage, nqp::const::RUSAGE_UTIME_MSEC) +
            nqp::atpos_i(rusage, nqp::const::RUSAGE_STIME_SEC) * 1000000 +
            nqp::atpos_i(rusage, nqp::const::RUSAGE_STIME_MSEC)
    }

    method !tweak-workers(\queue, \worker-list, &add-worker, $cores, $per-core-util) {
        # If there's nothing in the queue, nothing could need an extra worker.
        return if queue.elems == 0;

        # Go through the worker list. If something is not working, then there
        # is at lesat one worker free to process things in the queue, so we
        # don't need to add one.
        my int $total-completed;
        worker-list.map: {
            return unless .working;
            $total-completed += .take-completed;
        }

        # If we didn't complete anything, then consider adding more threads.
        my int $total-workers = self!total-workers();
        if $total-completed == 0 {
            if $total-workers < $!max_threads {
                # There's something in the queue and we haven't completed it.
                # If we are still below the CPU core count, just add a woker.
                if $total-workers < $cores {
                    add-worker();
                }

                # Otherwise, consider utilization. If it's very little then a
                # further thread may be needed for deadlock breaking.
                elsif $per-core-util < 2 {
                    scheduler-debug "Heuristic deadlock situation detected";
                    add-worker();
                }
            }
            else {
                scheduler-debug "Will not add extra worker; hit $!max_threads thread limit";
            }
        }
    }

    method !total-workers() {
        $!general-workers.elems + $!timer-workers.elems + $!affinity-workers.elems
    }

    submethod BUILD(
        Int :$!initial_threads = 0,
        Int :$!max_threads = (%*ENV<RAKUDO_MAX_THREADS> // 64).Int
        --> Nil
    ) {
        die "Initial thread pool threads ($!initial_threads) must be less than or equal to maximum threads ($!max_threads)"
            if $!initial_threads > $!max_threads;
        if $!initial_threads > 0 {
            # We've been asked to make some initial threads; we interpret this
            # as general workers.
            self!general-queue(); # Starts one worker
            if $!initial_threads > 1 {
                $!general-workers := (|$!general-workers, |(
                    GeneralWorker.new(
                        queue => $!general-queue,
                        scheduler => self
                    ) xx $!initial_threads - 1
                ));
            }
        }
    }

    method queue(Bool :$hint-time-sensitive, :$hint-affinity) {
        if $hint-affinity {
            self!affinity-queue()
        }
        elsif $hint-time-sensitive {
            self!timer-queue()
        }
        else {
            self!general-queue()
        }
    }

    my class TimerCancellation is repr('AsyncTask') { }
    method cue(&code, :$at, :$in, :$every, :$times = 1, :&stop is copy, :&catch ) {
        die "Cannot specify :at and :in at the same time"
          if $at.defined and $in.defined;
        die "Cannot specify :every, :times and :stop at the same time"
          if $every.defined and $times > 1 and &stop;
        my $delay = $at ?? $at - now !! $in // 0;

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
                $handle := nqp::timer(self!timer-queue(),
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
                return cancellation()
            }

            # no stopper
            else {
                my $handle := nqp::timer(self!timer-queue(),
                    &catch
                      ?? -> { code(); CATCH { default { catch($_) } } }
                      !! &code,
                    to-millis($delay), to-millis($every),
                    TimerCancellation);
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
              nqp::timer(self!timer-queue(), $todo, $delay, 0, TimerCancellation)
            ) for 1 .. $times;
            return Cancellation.new(:@async_handles);
        }

        # just cue the code
        else {
            my &run := &catch
               ?? -> { code(); CATCH { default { catch($_) } } }
               !! &code;
            nqp::push(self!general-queue(), &run);
            return Nil;
        }
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

    method loads() {
        [+] ($!general-queue ?? $!general-queue.elems !! 0),
            ($!timer-queue ?? $!timer-queue.elems !! 0),
            |($!affinity-workers.map(*.queue.elems))
    }
}

# This thread pool scheduler will be the default one.
PROCESS::<$SCHEDULER> = ThreadPoolScheduler.new();

# vim: ft=perl6 expandtab sw=4
