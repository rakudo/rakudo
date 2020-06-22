# Scheduler that always does things immediately, on the current thread.

my class CurrentThreadScheduler does Scheduler {

    method handle_uncaught($exception) {
        $exception.throw
    }

    method cue(&code, :$at, :$in, :$every, :$times = 1, :&catch is copy ) {
        die "Cannot specify :at and :in at the same time"
          if $at.defined and $in.defined;
        die "Cannot specify :every and :times at the same time"
          if $every.defined and $times > 1;
        die "Cannot specify :every in {self.^name}"
          if $every;

        my $delay := nqp::decont($at ?? $at - now !! $in);
        nqp::if(
            nqp::istype($delay, Num),
            nqp::if(
                nqp::iseq_n($delay, nqp::inf()),
                (return class { method cancel() {} }),
                nqp::if(
                    nqp::iseq_n($delay, nqp::neginf()),
                    ($delay := 0),
                    nqp::if(
                        nqp::isnanorinf($delay),
                        X::Scheduler::CueInNaNSeconds.new().throw()
                    )
                )
            )
        );

        sleep $delay if $delay;
        &catch //=
          (self && self.uncaught_handler) // -> $ex { self.handle_uncaught($ex) };

        for 1 .. $times {
            code();
            CATCH { default { catch($_) } };
        }
        class { method cancel() {} }
    }

    method loads(--> 0) { }
}

# vim: expandtab shiftwidth=4
