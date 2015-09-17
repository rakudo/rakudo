# Operations we can do on Supplies. Note, many of them need to compose
# the Supply role into classes they create along the way, so they must
# be declared outside of Supply.

my class X::Supply::Migrate::Needs is Exception {
    method message() {
        ".migrate needs Supplies to be emitted"
    }
}

my class SupplyOperations {

    # Private versions of the methods to relay events to subscribers, used in
    # implementing various operations.
    my role PrivatePublishing {
        method !emit(\msg) {
            for self.tappers {
                .emit().(msg)
            }
            Nil;
        }

        method !done() {
            for self.tappers {
                if .done { .done().() }
            }
            Nil;
        }

        method !quit($ex) {
            for self.tappers {
                if .quit { .quit().($ex) }
            }
            Nil;
        }
    }

    method on-demand(&producer, :&closing, :$scheduler = CurrentThreadScheduler) {
        class :: does Supply {
            has &!producer;
            has &!closing;
            has $!scheduler;

            submethod BUILD(:&!producer!, :&!closing!, :$!scheduler!) {}

            method live { False }
            method tap(|c) {
                my $closed = False;
                my $close_action;
                my $sub = self.Supply::tap(|c, closing => {
                    $closed = True;
                    &!closing && &!closing();
                });
                $!scheduler.cue(
                    {
                        my $s = Supply.new;
                        $s.tap(
                            -> \val { $sub.emit().(val) },
                            done => { if !$closed && $sub.done -> $t { $t() } },
                            quit => -> $ex { if !$closed && $sub.quit -> $t { $t($ex) } }
                        );
                        &!producer($s)
                    },
                    :catch(-> $ex { if !$closed && $sub.quit -> $t { $t($ex) } })
                );
                $sub
            }
        }.new(:&producer, :&closing, :$scheduler);
    }

    method from-list(*@values, :$scheduler = CurrentThreadScheduler) {
        class :: does Supply {
            has @!values;
            has $!scheduler;

            submethod BUILD(:@!values, :$!scheduler) {}

            method live { False }
            method tap(|c) {
                my $closed = False;
                my $sub = self.Supply::tap(|c, closing => { $closed = True });
                $!scheduler.cue(
                    {
                        for @!values -> \val {
                            last if $closed;
                            $sub.emit().(val);
                        }
                        if !$closed && $sub.done -> $l { $l() }
                    },
                    :catch(-> $ex { if !$closed && $sub.quit -> $t { $t($ex) } })
                );
                $sub
            }
        }.new(:@values, :$scheduler);
    }

    method interval($interval, $delay = 0, :$scheduler = $*SCHEDULER) {
        class :: does Supply {
            has $!scheduler;
            has $!interval;
            has $!delay;

            submethod BUILD(:$!scheduler, :$!interval, :$!delay) {}

            method live { False }
            method tap(|c) {
                my $cancellation;
                my $sub = self.Supply::tap(|c, closing => { $cancellation.cancel() });
                $cancellation = $!scheduler.cue(
                    {
                        state $i = 0;
                        $sub.emit().($i++);
                    },
                    :every($!interval), :in($!delay)
                );
                $sub
            }
        }.new(:$interval, :$delay, :$scheduler);
    }

    method flat(Supply $source) {
        class :: does Supply does PrivatePublishing {
            has $!source;

            submethod BUILD(:$!source) { }

            method live { $!source.live }
            method tap(|c) {
                my $source_tap;
                my $tap = self.Supply::tap(|c, closing => {$source_tap.close});
                $source_tap = $!source.tap( -> \val {
                      $tap.emit().(val.flat)
                  },
                  done => { if $tap.done { $tap.done().() } },
                  quit => -> $ex { if $tap.quit { $tap.quit().($ex) } });
                $tap
            }
        }.new(:$source);
    }

    method grep(Supply $source, Mu $test) {
        class :: does Supply does PrivatePublishing {
            has $!source;
            has Mu $!test;

            submethod BUILD(:$!source, :$!test) { }

            method live { $!source.live }
            method tap(|c) {
                my $source_tap;
                my $tap = self.Supply::tap(|c, closing => {$source_tap.close});
                $source_tap = $!source.tap( $!test.DEFINITE
                  ?? nqp::istype($!test,Callable)
                    ?? nqp::istype($!test,Regex)
                       ?? -> \val { $tap.emit().(val) if val.match($!test) }
                       !! -> \val { $tap.emit().(val) if $!test(val) }
                    !! -> \val { $tap.emit().(val) if val ~~ $!test }
                  !! -> \val { $tap.emit().(val) if val ~~ $!test },
                  done => { if $tap.done { $tap.done().() } },
                  quit => -> $ex { if $tap.quit { $tap.quit().($ex) } }
                );
                $tap
            }
        }.new(:$source, :$test);
    }

    method map(Supply $source, &mapper) {
        class :: does Supply does PrivatePublishing {
            has $!source;
            has &!mapper;

            submethod BUILD(:$!source, :&!mapper) { }

            method live { $!source.live }
            method tap(|c) {
                my $source_tap;
                my $tap = self.Supply::tap(|c, closing => {$source_tap.close});
                $source_tap = $!source.tap( -> \val {
                      $tap.emit().(&!mapper(val))
                  },
                  done => { if $tap.done { $tap.done().() } },
                  quit => -> $ex { if $tap.quit { $tap.quit().($ex) } });
                $tap
            }
        }.new(:$source, :&mapper);
    }

    method schedule-on(Supply $source, Scheduler $scheduler) {
        class :: does Supply does PrivatePublishing {
            has $!source;
            has $!scheduler;

            submethod BUILD(:$!source, :$!scheduler) { }

            method live { $!source.live }
            method tap(|c) {
                my $source_tap;
                my $tap = self.Supply::tap(|c, closing => {$source_tap.close});
                $source_tap = $!source.tap( -> \val {
                      $!scheduler.cue: { $tap.emit().(val) }
                  },
                  done => { $!scheduler.cue: { if $tap.done { $tap.done().() } } },
                  quit => -> $ex { if $tap.quit { $tap.quit().($ex) } });
                $tap
            }
        }.new(:$source, :$scheduler);
    }

    method start(Supply $s, &startee) {
        self.map($s, -> \value {
            class :: does Supply does PrivatePublishing {
                has $!value;
                has &!startee;

                submethod BUILD(:$!value, :&!startee) { }

                method live { $s.live }
                method tap(|c) {
                    my $sub = self.Supply::tap(|c);
                    Promise.start({ &!startee($!value) }).then({
                        if .status == Kept {
                            self!emit(.result);
                            self!done();
                        }
                        else {
                            self!quit(.cause);
                        }
                    });
                    $sub
                }
            }.new(:value(value), :&startee);
        });
    }

    method stable(Supply $source, $time, :$scheduler = $*SCHEDULER) {

        return $source if !$time;  # nothing to do

        class :: does Supply does PrivatePublishing {
            has $!source;
            has $!time;
            has $!scheduler;
            has $!lock;
            has $!last_cancellation;

            submethod BUILD(:$!source, :$!time, :$!scheduler) {
                $!lock = Lock.new;
            }

            method live { $!source.live }
            method tap(|c) {
                my $source_tap;
                my $tap = self.Supply::tap(|c, closing => {$source_tap.close});
                $source_tap = $!source.tap(
                    -> \val {
                        $!lock.protect({
                            if $!last_cancellation {
                                $!last_cancellation.cancel;
                            }
                            $!last_cancellation = $!scheduler.cue(
                                :in($time),
                                {
                                    $!lock.protect({
                                        $!last_cancellation = Nil;
                                    });
                                    $tap.emit().(val);
                                });
                        });
                    },
                    done => { if $tap.done { $tap.done().() } },
                    quit => -> $ex { if $tap.quit { $tap.quit().($ex) } });
                $tap
            }
        }.new(:$source, :$time, :$scheduler);
    }

    method delayed(Supply $source, $time, :$scheduler = $*SCHEDULER) {

        return $source if !$time;  # nothing to do

        class :: does Supply does PrivatePublishing {
            has $!source;
            has $!time;
            has $!scheduler;

            submethod BUILD(:$!source, :$!time, :$!scheduler) { }

            method live { $!source.live }
            method tap(|c) {
                my $source_tap;
                my $tap = self.Supply::tap(|c, closing => {$source_tap.close});
                $source_tap = $!source.tap(
                    -> \val {
                        $!scheduler.cue( { $tap.emit().(val) }, :in($time) );
                    },
                    done => {
                        $!scheduler.cue( { if $tap.done { $tap.done().() } }, :in($time) );
                    },
                    quit => -> $ex {
                        $!scheduler.cue( { if $tap.quit { $tap.quit().($ex) } }, :in($time) );
                    } );
                $tap
            }
        }.new(:$source, :$time, :$scheduler);
    }

    method migrate(Supply $source) {
        class :: does Supply does PrivatePublishing {
            has $!source;
            has $!current;
            has $!lock;

            submethod BUILD(:$!source) {
                $!lock = Lock.new;
            }

            method live { $!source.live }
            method tap(|c) {
                my $source_tap;
                my $tap = self.Supply::tap(|c, closing => {$source_tap.close});
                $source_tap = $!source.tap(
                    -> \inner_supply {
                        X::Supply::Migrate::Needs.new.throw
                          unless nqp::istype(inner_supply,Supply);
                        $!lock.protect({
                            $!current.close() if $!current;
                            $!current = inner_supply.tap(-> \val {
                                $tap.emit().(val);
                            });
                        });
                    },
                    done => { if $tap.done { $tap.done().() } },
                    quit => -> $ex { if $tap.quit { $tap.quit().($ex) } });
                $tap
            }
        }.new(:$source);
    }

    method classify(Supply $source, &mapper, :$multi ) {
        class :: does Supply does PrivatePublishing {
            has $!source;
            has %!mapping;

            submethod BUILD(:$!source) { }
            submethod find_supply ($key) {
                %!mapping{ $key.WHICH } //= do {
                    my $s = Supply.new;
                    self!emit($key => $s);
                    $s;
                };
            }

            method !classify-done() {
                %!mapping.values>>.done;
                self!done;
            }

            method live { $!source.live }
            method tap(|c) {
                my $source_tap;
                my $tap = self.Supply::tap(|c, closing => {$source_tap.close});
                $source_tap = $!source.tap( $multi
                  ?? -> \val {
                      for @(mapper(val)) -> $key {
                          self.find_supply($key).emit(val);
                      }
                  }
                  !! -> \val {
                      self.find_supply( mapper(val) ).emit(val);
                  },
                  done => { self!classify-done(); },
                  quit => -> $ex { self!quit($ex) });
                $tap
            }
        }.new(:$source);
    }
}

# vim: ft=perl6 expandtab sw=4
