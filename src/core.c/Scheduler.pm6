# Schedulers do this role. It mostly serves as an interface for the things
# that schedulers must do, as well as a way to factor out some common "sugar"
# and infrastructure.

my class X::Scheduler::CueInNaNSeconds is Exception {
    method message(--> Str) {
        'Cannot pass NaN as a number of seconds to Scheduler.cue'
    }
}

my role Scheduler {
    has &.uncaught_handler is rw;

    method handle_uncaught($exception) {
        my $ch = &!uncaught_handler;
        if $ch {
            $ch($exception);
        }
        else {
            # No default handler, so terminate the application.
            note "Unhandled exception in code scheduled on thread " ~ $*THREAD.id;
            if Rakudo::Internals.LL-EXCEPTION {
                note $exception.message;
                note $exception.backtrace.full;
            }
            else {
                note $exception.gist;
            }
            exit(1);
        }
    }

    method cue { ... }

    method loads() { ... }
}

# vim: expandtab shiftwidth=4
