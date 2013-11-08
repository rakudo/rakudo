# Scheduler that always does things immediately, on the current thread.

my class CurrentThreadScheduler does Scheduler {
    method handle_uncaught($exception) {
        $exception.throw
    }

    proto method cue(|) { * }
    multi method cue(&code) {
        code()
    }
    multi method cue(&code, :$in!) {
        sleep $in;
        code();
    }
    multi method cue (&code, :$every!, :$in ) {
        sleep $in if $in.defined;
        loop {
            code();
            sleep $every;
        }
    }
    multi method cue (&code, :$at!, :$every, :$in ) {
        die "Cannot specify :at and :in at the same time" if $in.defined;
        sleep $at - now;
        loop {
            code();
            last unless $every.defined;
            sleep $every;
        }
    }

    method outstanding() { 0 }
}
