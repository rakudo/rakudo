# The JavaScriptScheduler is a scheduler that works around the lack
# of threads in javascript

my class JavaScriptScheduler does Scheduler {
    use nqp;

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

        die "Cannot specify :at in {self.^name}"
          if $at;
        die "Cannot specify :in in {self.^name}"
          if $in;

        my $delay = $at ?? $at - now !! $in;
        #sleep $delay if $delay;
        &catch //=
          self.uncaught_handler // -> $ex { self.handle_uncaught($ex) };

        for 1 .. $times {
            code();
            CATCH { default { catch($_) } };
        }
        class { method cancel() {} }
    }

    method queue() {
      nqp::null();
    }

    method loads(--> 0) { }
}

# This thread pool scheduler will be the default one.
PROCESS::<$SCHEDULER> = JavaScriptScheduler.new();

# vim: ft=perl6 expandtab sw=4
