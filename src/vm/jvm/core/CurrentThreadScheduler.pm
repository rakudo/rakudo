# Scheduler that always does things immediately, on the current thread.

my class CurrentThreadScheduler does Scheduler {
    method handle_uncaught($exception) {
        $exception.throw
    }

    method cue(&code, :$at, :$in, :$every, :&catch ) {
        die "Cannot specify :at and :in at the same time"
          if $at.defined and $in.defined;

        my $delay = $at ?? $at - now !! $in;
        sleep $delay if $delay;

        if &catch {
            if $every {
                loop { {  # extra block needed because of #120498
                        code();
                        sleep $every;
                        CATCH { default { catch($_) } };
                } }
            }
            else {
                code();
                CATCH { default { catch($_) } };
            }
        }
        elsif $every {
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
