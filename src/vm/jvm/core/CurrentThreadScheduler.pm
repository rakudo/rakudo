# Scheduler that always does things immediately, on the current thread.

my class CurrentThreadScheduler does Scheduler {
    method handle_uncaught($exception) {
        $exception.throw
    }

    method cue(&code, :$at, :$in, :$every, :&catch is copy ) {
        die "Cannot specify :at and :in at the same time"
          if $at.defined and $in.defined;
        die "Cannot specify :every in {self.HOW.name(self)}"
          if $every;

        my $delay = $at ?? $at - now !! $in;
        sleep $delay if $delay;
        &catch //=
          self.uncaught_handler // -> $ex { self.handle_uncaught($ex) };

        code();
        CATCH { default { catch($_) } };
    }

    method loads() { 0 }
}
