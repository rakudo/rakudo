# Scheduler that always does things immediately, on the current thread.

my class CurrentThreadScheduler does Scheduler {
    method handle_uncaught($exception) {
        $exception.throw
    }

    method cue(&code, :$at, :$in, :$every ) {
        die "Cannot specify :at and :in at the same time"
          if $at.defined and $in.defined;

        my $delay = $at ?? $at - now !! $in;
        sleep $delay if $delay;

        if $every {
            loop {
                code();
                sleep $every;
            }
        }
        else {
            code();
        }
    }

    method loads() { 0 }
}
