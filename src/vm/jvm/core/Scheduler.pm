# Schedulers do this role. It mostly serves as an interface for the things
# that schedulers must do, as well as a way to factor out some common "sugar"
# and infrastructure.
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
            note $exception.gist;
            exit(1);
        }
    }

    method schedule(&code) { ... }

    method schedule_with_catch(&code, &catch) {
        self.schedule({
            code();
            CATCH { default { catch($_) } }
        })
    }
    
    method schedule_in(&code, $delay) { ... }
    method schedule_every(&code, $interval, $delay = 0) { ... }
    method outstanding() { ... }
}
