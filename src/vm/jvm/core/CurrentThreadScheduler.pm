# Scheduler that always does things immediately, on the current thread.
my class CurrentThreadScheduler does Scheduler {
    method handle_uncaught($exception) {
        $exception.throw
    }

    method schedule(&code) {
        code()
    }

    method schedule_in(&code, $delay) {
        sleep $delay;
        code();
    }

    method schedule_every(&code, $interval, $delay = 0) {
        sleep $delay;
        loop {
            code();
            sleep $interval;
        }
    }

    method outstanding() {
        0
    }
}
