my class ThreadPoolScheduler does Scheduler {
    # A concurrent, blocking-on-receive queue.
    my class Queue is repr('ConcBlockingQueue') {
        method elems() is raw { nqp::elems(self) }
    }

    # Initialize $*PID here, as we need it for the debug message
    # anyway *and* it appears to have a positive effect on stability
    # specifically wrt GH #1202.
    PROCESS::<$PID> := nqp::p6box_i(my int $pid = nqp::getpid);

    # Scheduler defaults controlled by environment variables
    my $ENV := nqp::getattr(%*ENV,Map,'$!storage');
    my int $scheduler-debug;
    $scheduler-debug = 1
      if nqp::atkey($ENV,'RAKUDO_SCHEDULER_DEBUG');
    my int $scheduler-debug-status;
    $scheduler-debug-status = 1
      if nqp::atkey($ENV,'RAKUDO_SCHEDULER_DEBUG_STATUS');

    sub scheduler-debug($message --> Nil) {
        if $scheduler-debug {
            note "[SCHEDULER $pid] $message";
        }
    }

    # Infrastructure for non-blocking `await` for code running on the
    # scheduler.
    my constant THREAD_POOL_PROMPT = Mu.new;
    my class ContinuationWrapper {
        has $.cont;
        method new(Mu \cont) {
            nqp::p6bindattrinvres(nqp::create(self), ContinuationWrapper, '$!cont', cont)
        }
    }
    class ThreadPoolAwaiter does Awaiter {
        has $!queue;

        submethod BUILD(:$queue!) {
            $!queue := nqp::decont($queue);
        }

        sub holding-locks() {
            nqp::hllbool(nqp::threadlockcount(nqp::currentthread()))
        }

        method await(Awaitable:D $a) {
            holding-locks() || !nqp::isnull(nqp::getlexdyn('$*RAKUDO-AWAIT-BLOCKING'))
                ?? Awaiter::Blocking.await($a)
                !! self!do-await($a)
        }

        method !do-await(Awaitable:D $a) {
            my $handle := $a.get-await-handle;
            if $handle.already {
                $handle.success
                    ?? $handle.result
                    !! $handle.cause.rethrow
            }
            else {
                my $success;
                my $result;
                nqp::continuationcontrol(1, THREAD_POOL_PROMPT, -> Mu \c {
                    $handle.subscribe-awaiter(-> \success, \result {
                        $success := success;
                        $result := result;
                        nqp::push($!queue, ContinuationWrapper.new(c));
                        Nil
                    });
                });
                $success
                    ?? $result
                    !! $result.rethrow
            }
        }

        method await-all(Iterable:D \i) {
            holding-locks() || !nqp::isnull(nqp::getlexdyn('$*RAKUDO-AWAIT-BLOCKING'))
                ?? Awaiter::Blocking.await-all(i)
                !! self!do-await-all(i)
        }

        method !do-await-all(Iterable:D \i) {
            # Collect results that are already available, and handles where the
            # results are not yet available together with the matching insertion
            # indices.
            my \results = nqp::list();
            my \handles = nqp::list();
            my \indices = nqp::list_i();
            my int $insert = 0;
            my $saw-slip = False;
            for i -> $awaitable {
                unless nqp::istype($awaitable, Awaitable) {
                    die "Can only specify Awaitable objects to await (got a $awaitable.^name())";
                }
                unless nqp::isconcrete($awaitable) {
                    die "Must specify a defined Awaitable to await (got an undefined $awaitable.^name())";
                }

                my $handle := $awaitable.get-await-handle;
                if $handle.already {
                    if $handle.success {
                        my \result = $handle.result;
                        nqp::bindpos(results, $insert, result);
                        $saw-slip = True if nqp::istype(result, Slip);
                    }
                    else {
                        $handle.cause.rethrow
                    }
                }
                else {
                    nqp::push(handles, $handle);
                    nqp::push_i(indices, $insert);
                }

                ++$insert;
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
                    loop (my int $i = 0; $i < $num-handles; ++$i) {
                        my $handle := nqp::atpos(handles, $i);
                        my int $insert = nqp::atpos_i(indices, $i);
                        $handle.subscribe-awaiter(-> \success, \result {
                            my int $resume;
                            $l.protect: {
                                if success && $remaining {
                                    nqp::bindpos(results, $insert, result);
                                    $saw-slip = True if nqp::istype(result, Slip);
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
                                    $l.lock; # lock gets released as soon as $continuation is initialized
                                    $l.unlock; # no need to hold the lock while running the continuation - no one else is gonna take it
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
                nqp::continuationcontrol(1, THREAD_POOL_PROMPT, -> Mu \c {
                    $continuation := c;
                    $l.unlock;
                });

                # If we got an exception, throw it.
                $exception.rethrow if nqp::isconcrete($exception);
            }

            my \result-list = nqp::p6bindattrinvres(nqp::create(List), List, '$!reified', results);
            $saw-slip ?? result-list.map(-> \val { val }).List !! result-list
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
#?if moar
        has atomicint $.completed;
#?endif
#?if !moar
        has int $.completed;
#?endif

        # Total number of tasks completed since creation.
        has int $.total;

        # Working is 1 if the worker is currently busy, 0 if not.
        has int $.working;

        # Number of times take-completed has returned zero in a row.
        has int $.times-nothing-completed;

        # Resets the completed to zero and updates the total.
        method take-completed() {
#?if moar
            my atomicint $taken;
            cas $!completed, -> atomicint $current { $taken = $current; 0 }
#?endif
#?if !moar
            my int $taken = $!completed;
            $!completed = 0;
#?endif
            if $taken == 0 {
                ++$!times-nothing-completed;
            }
            else {
                $!times-nothing-completed = 0;
            }
            $taken
        }

        method !run-one(\task --> Nil) {
            $!working = 1;
            if nqp::istype(task, ContinuationWrapper) {
                nqp::continuationinvoke(task.cont, nqp::null());
            }
            else {
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
                            nqp::getcomp('Raku').handle-control($vm-ex);
                        }
                    }
                    CATCH {
                        default {
                            $!scheduler.handle_uncaught($_)
                        }
                    }
                });
            }
            $!working = 0;
#?if moar
            ++⚛$!completed;
#?endif
#?if !moar
            ++$!completed;
#?endif
            ++$!total;
        }
    }
    my class GeneralWorker does Worker {
        has Queue $!queue;

        submethod BUILD(Queue :$queue!, :$!scheduler!) {
            $!queue := $queue;
            $!thread = Thread.start(:app_lifetime, :name<GeneralWorker>, {
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
            $!thread = Thread.start(:app_lifetime, :name<TimerWorker>, {
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
            $!thread = Thread.start(:app_lifetime, :name<AffinityWorker>, {
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
    has Lock $!state-lock = Lock.new;

    # The general queue and timer queue, if created.
    has Queue $!general-queue;
    has Queue $!timer-queue;

    # The current lists of workers. Immutable lists; new ones are produced
    # upon changes.
    has $!general-workers;
    has $!timer-workers;
    has $!affinity-workers;

    # The supervisor thread, if started.
    has Thread $!supervisor;

    method !general-queue() {
        nqp::if(
          nqp::isconcrete($!general-queue),
          $!general-queue,
          nqp::stmts(
            $!state-lock.protect( {
                nqp::unless(
                  nqp::isconcrete($!general-queue),
                  nqp::stmts(
                    # We don't have any workers yet, so start one.
                    ($!general-queue := nqp::create(Queue)),
                    ($!general-workers := first-worker(
                      GeneralWorker.new(
                        queue => $!general-queue,
                        scheduler => self
                      )
                    )),
                    scheduler-debug("Created initial general worker thread"),
                    self!maybe-start-supervisor
                  )
                )
            } ),
            $!general-queue
          )
        )
    }

    method !timer-queue() {
        nqp::if(
          nqp::isconcrete($!timer-queue),
          $!timer-queue,
          nqp::stmts(
            $!state-lock.protect( {
                nqp::unless(
                  nqp::isconcrete($!timer-queue),
                  nqp::stmts(
                    # We don't have any workers yet, so start one.
                    ($!timer-queue := nqp::create(Queue)),
                    ($!timer-workers := first-worker(
                      TimerWorker.new(
                        queue => $!timer-queue,
                        scheduler => self
                      )
                    )),
                    scheduler-debug("Created initial timer worker thread"),
                    self!maybe-start-supervisor
                  )
                )
            } ),
            $!timer-queue
          )
        )
    }

    # set up affinity threshold information
    my $affinity-add-thresholds := nqp::list_i(0, 1, 5, 10, 20, 50, 100);
    my int $affinity-max-index =
      nqp::sub_i(nqp::elems($affinity-add-thresholds),1);
    my $affinity-max-threshold =
      nqp::atpos_i($affinity-add-thresholds,$affinity-max-index );

    method !affinity-queue() {
        nqp::stmts(
          # If there are no affinity workers, start one.
          nqp::unless(
            nqp::elems(my $cur-affinity-workers := $!affinity-workers),
            nqp::stmts(
              $!state-lock.protect( {
                  nqp::unless(
                    nqp::elems($!affinity-workers),
                    nqp::stmts(
                      # We don't have any affinity workers yet, so start one
                      # and return its queue.
                      ($!affinity-workers := first-worker(
                        AffinityWorker.new(
                          scheduler => self
                        )
                      )),
                      scheduler-debug("Created initial affinity worker thread"),
                      self!maybe-start-supervisor,
                      (return nqp::atpos($!affinity-workers,0).queue)
                    )
                  )
              } ),
              ($cur-affinity-workers := $!affinity-workers) # lost race
            )
          ),

          # Otherwise, see which has the least load (this is inherently racey
          # and approximate, but enough to help us avoid a busy worker). If we
          # find an empty queue, return it immediately.
          (my int $i = -1),
          nqp::while(
            nqp::islt_i(
              ($i = nqp::add_i($i,1)),
              nqp::elems($cur-affinity-workers)
            ),
            nqp::if(
              nqp::isconcrete(my $most-free-worker),
              nqp::stmts(
                (my $cand := nqp::atpos($cur-affinity-workers,$i)),
                nqp::unless(
                  nqp::elems(my $queue := $cand.queue),
                  nqp::unless(
                    $cand.working,
                    (return $queue),
                  ),
                ),
                nqp::if(
                  nqp::islt_i(
                    nqp::elems($queue),
                    nqp::elems($most-free-worker.queue)
                  ),
                  $most-free-worker := $cand
                )
              ),
              ($most-free-worker := nqp::atpos($cur-affinity-workers,$i))
            )
          ),

          # Otherwise, check if the queue beats the threshold to add another
          # worker thread.
          nqp::if(
            nqp::isle_i(
              nqp::elems(my $chosen-queue := $most-free-worker.queue),
              nqp::if(
                nqp::islt_i(
                  nqp::elems($cur-affinity-workers),
                  $affinity-max-index
                ),
                nqp::atpos_i(
                  $affinity-add-thresholds,
                  nqp::elems($cur-affinity-workers)
                ),
                $affinity-max-threshold,
              )
            ),
            # found one that is empty enough
            $chosen-queue,
            # need to add another one, unless another thread did already
            $!state-lock.protect( {
                nqp::stmts(
                  nqp::if(
                    nqp::isgt_i(
                      nqp::elems($!general-workers)
                        + nqp::elems($!timer-workers)
                        + nqp::elems($!affinity-workers),
                      $!max_threads
                    ),
                    # alas, no way to add more threads
                    nqp::stmts(
                      scheduler-debug("Will not add extra affinity worker; hit $!max_threads thread limit"),
                      (return $chosen-queue)
                    )
                  ),
                  nqp::if(
                    nqp::isne_i(
                      nqp::elems($cur-affinity-workers),
                      nqp::elems($!affinity-workers)
                    ),
                    # different load found, take this one
                    (return $chosen-queue)
                  ),

                  # ok ok, add new worker
                  ($!affinity-workers := push-worker(
                    $!affinity-workers,
                    (my $new-worker := AffinityWorker.new(scheduler => self))
                  )),
                  scheduler-debug("Added an affinity worker thread"),
                  $new-worker.queue
                )
            } )
          )
        )
    }

    # Initializing a worker list with a worker, is straightforward and devoid
    # of concurrency issues, as we're already in protected code when we do this.
    sub first-worker(\first) is raw {
        my $workers := nqp::create(IterationBuffer);
        nqp::push($workers,first);
        $workers
    }

    # Since the worker lists can be changed during copying, we need to
    # just take whatever we can get and assume that it may be gone by
    # the time we get to it.
    sub push-worker(\workers, \to-push) is raw {
        my $new-workers := nqp::clone(workers);
        nqp::push($new-workers,to-push);
        $new-workers
    }

    # The supervisor sits in a loop, mostly sleeping. Each time it wakes up,
    # it takes stock of the current situation and decides whether or not to
    # add threads.
    my constant SUPERVISION_INTERVAL  = 1e-2;
    my constant NUM_SAMPLES           = 5;
    my constant NUM_SAMPLES_NUM       = 5e0;
    my constant EXHAUSTED_RETRY_AFTER = 100;
    method !maybe-start-supervisor(--> Nil) {
        unless $!supervisor.DEFINITE {
            $!supervisor = Thread.start(:app_lifetime, :name<Supervisor>, {
                sub add-general-worker(--> Nil) {
                    $!state-lock.protect: {
                        $!general-workers := push-worker(
                          $!general-workers,
                          GeneralWorker.new(
                            queue => $!general-queue,
                            scheduler => self
                          )
                        );

                    }
                    scheduler-debug "Added a general worker thread";
                }
                sub add-timer-worker(--> Nil) {
                    $!state-lock.protect: {
                        $!timer-workers := push-worker(
                          $!timer-workers,
                          TimerWorker.new(
                            queue => $!timer-queue,
                            scheduler => self
                          )
                        );
                    }
                    scheduler-debug "Added a timer worker thread";
                }

                scheduler-debug "Supervisor started";
                my int $last-rusage-time = nqp::time;
                my int @rusage;
                nqp::getrusage(@rusage);
                my int $last-usage =
                  nqp::mul_i(
                    nqp::atpos_i(@rusage,nqp::const::RUSAGE_UTIME_SEC),
                    1000000
                  ) + nqp::atpos_i(@rusage,nqp::const::RUSAGE_UTIME_MSEC)
                    + nqp::mul_i(
                        nqp::atpos_i(@rusage,nqp::const::RUSAGE_STIME_SEC),
                        1000000
                      )
                    + nqp::atpos_i(@rusage, nqp::const::RUSAGE_STIME_MSEC);

                my num @last-utils = 0e0 xx NUM_SAMPLES;
                my int $cpu-cores = max(nqp::cpucores() - 1,1);

                # These definitions used to live inside the supervisor loop.
                # Moving them out of the loop does not improve CPU usage
                # noticably, but does seem to save about 3M of memory for
                # every 10 seconds of runtime.  Whether this is an actual
                # leak, or just less churn on garbage collection, remains
                # unclear until we have profiling options that also work
                # when multiple threads are running.
                my int $exhausted;
                my int $now;
                my int $rusage-period;
                my int $current-usage;
                my int $usage-delta;
                my num $normalized-delta;
                my num $per-core;
                my num $per-core-util;
                my num $smooth-per-core-util = 0e0;

                scheduler-debug "Supervisor thinks there are $cpu-cores CPU cores";
                loop {
                    # Wait until the next time we should check how things
                    # are.
                    nqp::sleep(SUPERVISION_INTERVAL);

                    # Work out the delta of CPU usage since last supervision
                    # and the time period that measurement spans.
                    $now = nqp::time;
                    $rusage-period = $now - $last-rusage-time;
                    $last-rusage-time = $now;
                    nqp::getrusage(@rusage);
                    $current-usage =
                      nqp::mul_i(
                        nqp::atpos_i(@rusage,nqp::const::RUSAGE_UTIME_SEC),
                        1000000
                      ) + nqp::atpos_i(@rusage,nqp::const::RUSAGE_UTIME_MSEC)
                        + nqp::mul_i(
                            nqp::atpos_i(@rusage,nqp::const::RUSAGE_STIME_SEC),
                            1000000
                          )
                        + nqp::atpos_i(@rusage,nqp::const::RUSAGE_STIME_MSEC);
                    $usage-delta = $current-usage - $last-usage;
                    $last-usage = $current-usage;

                    # Scale this by the time between rusage calls and turn it
                    # into a per-core utilization percentage.
                    $normalized-delta = nqp::div_n($usage-delta, $rusage-period);
                    $per-core = nqp::div_n($normalized-delta, $cpu-cores);
                    # used to have a "100 *" in the front, but for speed
                    # and mostly memory usage reasons it got constant-folded
                    # into the 1000000 instead.
                    $per-core-util = nqp::div_n($per-core, (10000e0 * NUM_SAMPLES_NUM));

                    # Since those values are noisy, average the last
                    # NUM_SAMPLES values to get a smoothed value.
                    $smooth-per-core-util -= nqp::shift_n(@last-utils);
                    $smooth-per-core-util += $per-core-util;
                    nqp::push_n(@last-utils,$per-core-util);
                    note "[SCHEDULER $pid] Per-core utilization (approx): $smooth-per-core-util%"
                      if $scheduler-debug-status;

                    # exhausted the system allotment of low level threads
                    if $exhausted {
                        $exhausted = 0  # for next run of supervisor
                          if ++$exhausted > EXHAUSTED_RETRY_AFTER;
                    }

                    # we can still add threads if necessary
                    else {
                        self!tweak-workers($!general-queue, $!general-workers,
                          &add-general-worker, $cpu-cores, $smooth-per-core-util)
                          if $!general-queue.DEFINITE && nqp::elems($!general-queue);

                        self!tweak-workers($!timer-queue, $!timer-workers,
                          &add-timer-worker, $cpu-cores, $smooth-per-core-util)
                          if $!timer-queue.DEFINITE && nqp::elems($!timer-queue);

                    }

                    # always need to prod affinity workers
                    if nqp::isconcrete($!affinity-workers)
                      && nqp::elems($!affinity-workers) -> int $count {
                        my $worker;
                        my $item;
                        loop (my int $idx = 0; $idx < $count; $idx++) {
                            $worker := nqp::atpos($!affinity-workers, $idx);
                            if $worker.working {
                                $worker.take-completed;

                                # If an affinity worker completed nothing for some time,
                                # steal an item from its queue, moving it to general queue.
                                # This resolves deadlocks in certain cases.
                                if $worker.times-nothing-completed > 10 {
                                    scheduler-debug "Stealing queue from affinity worker";
                                    $item := nqp::queuepoll($worker.queue);
                                    nqp::push(self!general-queue, $item)
                                      unless nqp::isnull($item);
                                }
                            }
                        }
                    }

                    CATCH {
                        when X::Exhausted {
                            $exhausted = 1;
                            scheduler-debug .message;
                            scheduler-debug "Refraining from trying to start new threads";
                        }
                        default {
                            scheduler-debug .gist;
                        }
                    }
                }
            });
        }
    }

    # Tweak workers for non-empty queues
    method !tweak-workers(\queue, \worker-list, &add-worker, $cores, $per-core-util) {

        # Go through the worker list. If something is not working, then there
        # is at least one worker free to process things in the queue, so we
        # don't need to add one.
        my int $total-completed;
        my int $total-times-nothing-completed;
        my int $i = -1;
        nqp::while(
          ++$i < nqp::elems(worker-list),
          nqp::if(
            (my $worker := nqp::atpos(worker-list,$i)).working,
            nqp::stmts(
              ($total-completed += $worker.take-completed),
              ($total-times-nothing-completed += $worker.times-nothing-completed)
            ),
            return
          )
        );

        sub heuristic-check-for-deadlock(--> Nil) {
            my int $average-times-nothing-completed
            = $total-times-nothing-completed div (nqp::elems(worker-list) || 1);
            if $average-times-nothing-completed > 20 {
                scheduler-debug "Heuristic queue progress deadlock situation detected";
                add-worker();
            }
        }

        # Consider adding more worker threads when:
        # 1. We didn't complete anything since the last supervision. This is
        #    likely because some long-running tasks are holding onto the
        #    workers. We should at least think about adding more (and the
        #    code below will try to determine if that's beneficial).
        # 2. The number of tasks in the queue is greater than the number of
        #    workers. Such a situation suggests we are under-resourced, and
        #    so liable to fall behind. Consider it like checkout lanes at a
        #    supermarket: if 20 people are queueing and there are only 2 open
        #    checkout lanes, it makes sense to open more, but if there are 20
        #    people waiting and 30 open checkout lanes, there's little to be
        #    won by opening another one at this point.
        my int $total-workers = nqp::elems($!general-workers)
          + nqp::elems($!timer-workers)
          + nqp::elems($!affinity-workers);
        if $total-completed == 0 || nqp::elems(queue) > nqp::elems(worker-list) {
            if $total-workers < $!max_threads {
                # There's something in the queue and we haven't completed it.
                # If we are still below the CPU core count, just add a worker.
                if $total-workers < $cores {
                    add-worker();
                }

                # Otherwise, consider utilization. If it's very little then a
                # further thread may be needed for deadlock breaking.
                elsif $per-core-util < 2 {
                    scheduler-debug "Heuristic low utilization deadlock situation detected";
                    add-worker();
                }

                # Another form of deadlock can happen when one kind of queue
                # is being processed but another is not. In that case, the
                # number of iterations since nothing was completed by any
                # worker will grow.
                else {
                    heuristic-check-for-deadlock
                }
            }
            else {
                scheduler-debug "Will not add extra worker; hit $!max_threads thread limit [branch with 0 total completed]";
            }
        }
        elsif $total-times-nothing-completed > 20*$cores {
            if $total-workers < $!max_threads {
                heuristic-check-for-deadlock
            }
            else {
                scheduler-debug "Will not add extra worker; hit $!max_threads thread limit [branch with some total completed]";
            }
        }
    }

    submethod BUILD(
        Int :$!initial_threads = 0,
#?if jvm
        Int :$!max_threads = (%*ENV<RAKUDO_MAX_THREADS> // 64).Int
#?endif
#?if !jvm
        Int :$!max_threads = nqp::ifnull(nqp::atkey($ENV,'RAKUDO_MAX_THREADS'),64)
#?endif
        --> Nil
    ) {
        die "Initial thread pool threads ($!initial_threads) must be less than or equal to maximum threads ($!max_threads)"
            if $!initial_threads > $!max_threads;

        $!general-workers  := nqp::create(IterationBuffer);
        $!timer-workers    := nqp::create(IterationBuffer);
        $!affinity-workers := nqp::create(IterationBuffer);

        if $!initial_threads > 0 {
            # We've been asked to make some initial threads; we interpret this
            # as general workers.
            $!general-queue   := nqp::create(Queue);
            nqp::push(
              $!general-workers,
              GeneralWorker.new(
                queue => $!general-queue,
                scheduler => self
              )
            ) for ^$!initial_threads;
            scheduler-debug "Created scheduler with $!initial_threads initial general workers";
            self!maybe-start-supervisor();
        }
        else {
            scheduler-debug "Created scheduler without initial general workers";
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

    # Checks if the value given is Inf, -Inf, or NaN. If NaN, this throws.
    # if Inf, this returns Nil for ThreadPoolScheduler.cue to return an empty
    # Cancellation for. If -Inf, returns 0. Otherwise, returns the value.
    sub validate-seconds(Numeric() $value --> Numeric) {
        nqp::unless(
          nqp::istype($value, Num),
          $value,
          nqp::if(
            nqp::iseq_n($value,nqp::inf()),
            Nil,
            nqp::if(
              nqp::iseq_n($value,nqp::neginf()),
              0,
              nqp::if(
                nqp::isnanorinf($value),
                X::Scheduler::CueInNaNSeconds.new().throw(),
                $value
              )
            )
          )
        )
    }
    sub to-millis(Numeric $value --> int) {
        nqp::unless(
          nqp::isconcrete(nqp::decont($value)),
          nqp::stmts(
            warn("Minimum timer resolution is 1ms; using that instead of Inf"),
            1
          ),
          nqp::if(
            nqp::isgt_i((my int $proposed = (1000 * $value).Int),0),
            $proposed,
            nqp::stmts(
              warn("Minimum timer resolution is 1ms; using that instead of {1000 * $value}ms"),
              1
            )
          ),
        )
    }
    sub to-millis-allow-zero(Numeric $value --> int) {
        nqp::unless(
          nqp::isconcrete(nqp::decont($value)),
          nqp::stmts(
            warn("Minimum timer resolution is 0ms; using that instead of Inf"),
            0
          ),
          nqp::if(
            nqp::isgt_i((my int $proposed = (1000 * $value).Int),0),
            $proposed
            # not true == 0 == what we need
          )
        )
    }
    sub wrap-catch(&code, &catch) {
        -> { code(); CATCH { default { catch($_) } } }
    }

    my class TimerCancellation is repr('AsyncTask') { }

    method !CUE_DELAY_TIMES(&code, int $delay, int $times, %args) {
        nqp::stmts(
          (my &run := nqp::if(                   # set up what we need to run
            nqp::isnull(my $catch :=
              nqp::atkey(nqp::getattr(%args,Map,'$!storage'),"catch")),
            &code,
            wrap-catch(&code, $catch) # wrap any catch handler around code
          )),
          Cancellation.new( async_handles => nqp::if(
            $times,
            nqp::stmts(                          # need to run more than once
              (my @async_handles),
              (my int $i = -1),
              nqp::while(
                nqp::islt_i(($i = nqp::add_i($i,1)),$times),
                @async_handles.push(
                  nqp::timer(self!timer-queue,&run,$delay,0,TimerCancellation)
                )
              ),
              @async_handles
            ),
            [                                    # only needs to run once
              nqp::timer(self!timer-queue,&run,$delay,0,TimerCancellation)
            ]
          ))
        )
    }

    proto method cue(|) {*}
    multi method cue(&code, :$every!, :$times = 1, *%_) {
        # these need to exist in this scope
        my $handle;
        my $cancellation;
        sub cancellation() {
            $cancellation //= Cancellation.new(async_handles => [$handle])
        }

        nqp::if(
          nqp::isconcrete(
            nqp::atkey((my $args := nqp::getattr(%_,Map,'$!storage')),"stop")
          ) && nqp::isconcrete($times) && $times > 1,
          die("Cannot specify :every, :times and :stop at the same time"),
          nqp::if(
            nqp::isconcrete(nqp::atkey($args,"at"))
              && nqp::isconcrete(nqp::atkey($args,"in")),
            die("Cannot specify :at and :in at the same time"),
            nqp::stmts(
              (my $interval := validate-seconds($every)), # ensure the interval given is sane
              (my $delay-in-seconds := validate-seconds(  # ensure the delay or time given is sane
                nqp::if(
                  nqp::isnull(my $at := nqp::atkey($args,"at")),
                  nqp::ifnull(nqp::atkey($args,"in"),0),
                  $at - now
                )
              ));
              nqp::unless(
                nqp::isconcrete($delay-in-seconds),
                (return Cancellation.new(async_handles => []))
              ),
              (my int $delay = to-millis-allow-zero($delay-in-seconds)),
              (my &run := nqp::if(                         # set up what should run
                nqp::isnull(my $catch := nqp::atkey($args,"catch")),
                &code,
                wrap-catch(&code, $catch)                  # wrap any catch handler around code
              )),
              nqp::stmts(
                (my int $interval-is-nil = nqp::not_i(nqp::isconcrete($interval))),
                (my $stopper := nqp::if(
                  ($interval-is-nil || nqp::isgt_i($times,1)),
                  nqp::stmts(                              # create our own stopper
                    (my int $todo = nqp::add_i(nqp::if($interval-is-nil,1,$times),1)),
                    sub { nqp::not_i($todo = nqp::sub_i($todo,1)) }
                  ),
                  nqp::atkey($args,"stop")
                )),
                nqp::if(
                  $interval-is-nil,
                  nqp::stmts(
                    (warn "Inf was passed via :every; running the given block only once, immediately"),
                    run(),
                    (return Cancellation.new(async_handles => []))
                  )
                ),
                nqp::if(
                  nqp::isconcrete($stopper),
                  nqp::stmts(                              # we have a stopper
                    ($handle := nqp::timer(
                      self!timer-queue,
                      -> { nqp::if($stopper(),cancellation().cancel,run()) },
                      $delay, to-millis($interval), TimerCancellation
                    )),
                    (return cancellation())
                  ),
                  nqp::stmts(                              # we have no stopper
                    ($handle := nqp::timer(
                      self!timer-queue,
                      &run,
                      $delay, to-millis($interval), TimerCancellation
                    )),
                    (return cancellation())
                  )
                )
              )
            )
          )
        )
    }
    multi method cue(&code, :$times!, *%_) {
        nqp::stmts(
          (my $args := nqp::getattr(%_,Map,'$!storage')),
          nqp::if(
            nqp::isconcrete(my $at := nqp::atkey($args,"at"))
              && nqp::isconcrete(my $in := nqp::atkey($args,"in")),
            die("Cannot specify :at and :in at the same time"),
            nqp::stmts(
              nqp::if(
                nqp::isconcrete($at)
                  && nqp::not_i(nqp::isconcrete($at := validate-seconds($at))),
                (return Cancellation.new(async_handles => [])),
                nqp::if(
                  nqp::isconcrete($in)
                    && nqp::not_i(nqp::isconcrete($in := validate-seconds($in))),
                  (return Cancellation.new(async_handles => []))
                )
              ),
              self!CUE_DELAY_TIMES(
                &code,
                to-millis(nqp::ifnull(
                  $in,
                  nqp::if(nqp::isnull($at), .001, $at - now)
                )),
                $times,
                %_
              )
            )
          )
        )
    }
    multi method cue(&code, :$at!, *%_) {
        nqp::isconcrete($at)
          && nqp::isconcrete(nqp::atkey(nqp::getattr(%_,Map,'$!storage'),"in"))
          ?? die("Cannot specify :at and :in at the same time")
          !! nqp::isconcrete(my $time := validate-seconds($at))
            ?? self!CUE_DELAY_TIMES(
                 &code, to-millis-allow-zero($time - now), 0, %_
               )
            !! Cancellation.new(async_handles => [])
    }
    multi method cue(&code, :$in!, *%_) {
        nqp::isconcrete(my $delay := validate-seconds($in))
          ?? (self!CUE_DELAY_TIMES(&code, to-millis-allow-zero($delay), 0, %_))
          !! Cancellation.new(async_handles => [])
    }
    multi method cue(&code, :&catch! --> Nil) {
        nqp::push(self!general-queue, wrap-catch(&code, &catch))
    }
    multi method cue(&code --> Nil) {
        nqp::push(self!general-queue,&code)
    }

    method loads() is raw {
        my int $loads = 0;
        $loads = $loads + nqp::elems($!general-queue) if $!general-queue;
        $loads = $loads + nqp::elems($!timer-queue)   if $!timer-queue;

        my int $i = -1;
        nqp::while(
          ++$i < nqp::elems($!affinity-workers),
          $loads = $loads + nqp::elems(nqp::atpos($!affinity-workers,$i).queue)
        );

        $loads
    }

    # Constants indexing into the data array
    my constant SUPERVISOR =  0;
    my constant GW         =  1;
    my constant GTQ        =  2;
    my constant GTC        =  3;
    my constant TW         =  4;
    my constant TTQ        =  5;
    my constant TTC        =  6;
    my constant AW         =  7;
    my constant ATQ        =  8;
    my constant ATC        =  9;
    my constant COLUMNS    = 10;

    # calculate number of tasks completed for a worker list
    sub completed(\workers) is raw {
        my int $elems = nqp::elems(workers);
        my int $completed;
        my int $i = -1;
        nqp::while(
          nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
          nqp::stmts(
            (my $w := nqp::atpos(workers,$i)),
            ($completed = nqp::add_i(
              $completed,
              nqp::getattr_i($w,$w.WHAT,'$!total')
            ))
          )
        );
        $completed
    }

    proto method usage(|) {*}
    multi method usage(ThreadPoolScheduler:U:) is raw {
        nqp::setelems(nqp::list_i,COLUMNS)
    }
    multi method usage(ThreadPoolScheduler:D:) is raw {
        my $data := nqp::setelems(nqp::list_i,COLUMNS);

        nqp::bindpos_i($data,SUPERVISOR,1) if $!supervisor;

        if $!general-workers -> \workers {
            nqp::bindpos_i($data,GW,nqp::elems(workers));
            nqp::bindpos_i($data,GTQ,nqp::elems($!general-queue))
              if $!general-queue;
            nqp::bindpos_i($data,GTC,completed(workers));
        }

        if $!timer-workers -> \workers {
            nqp::bindpos_i($data,TW,nqp::elems(workers));
            nqp::bindpos_i($data,TTQ,nqp::elems($!timer-queue))
              if $!timer-queue;
            nqp::bindpos_i($data,TTC,completed(workers));
        }

        if $!affinity-workers -> \workers {
            my int $elems =
              nqp::bindpos_i($data,AW,nqp::elems(workers));
            my int $completed;
            my int $queued;
            my int $i = -1;
            nqp::while(
              nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
              nqp::stmts(
                (my $w := nqp::atpos(workers,$i)),
                ($completed = nqp::add_i(
                  $completed,
                  nqp::getattr_i($w,$w.WHAT,'$!total')
                )),
                ($queued = nqp::add_i(
                  $queued,
                  nqp::elems(nqp::getattr($w,$w.WHAT,'$!queue'))
                ))
              )
            );
            nqp::bindpos_i($data,ATQ,$queued);
            nqp::bindpos_i($data,ATC,$completed);
        }

        # the final thing
        $data
    }
}

# vim: expandtab shiftwidth=4
