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
            my Mu $ex := nqp::decont($exception);
            $ex := nqp::decont(X::AdHoc.new(payload => $exception.gist))
              unless nqp::istype($ex, Exception) && nqp::isconcrete($ex);
            my Mu $vm-ex := nqp::getattr($ex, Exception, '$!ex');
            unless nqp::isconcrete($vm-ex) {
                $vm-ex := nqp::newexception();
                nqp::setpayload($vm-ex, $ex);
            }
            nqp::getcomp('Raku').handle-exception($vm-ex,
              "Unhandled exception in code scheduled on thread " ~ $*THREAD.id);
        }
    }

    method cue { ... }

    method loads() { ... }
}

# vim: expandtab shiftwidth=4
