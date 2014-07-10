# Operations we can do on Supplies. Note, many of them need to compose
# the Supply role into classes they create along the way, so they must
# be declared outside of Supply.

my class X::Supply::Migrate::Needs is Exception {
    method message() {
        ".migrate needs Supplies to be more'd"
    }
}

my class SupplyOperations is repr('Uninstantiable') {

    # Private versions of the methods to relay events to subscribers, used in
    # implementing various operations.
    my role PrivatePublishing {
        method !more(\msg) {
            for self.tappers {
                .more().(msg)
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
    
    method on_demand(&producer, :&closing, :$scheduler = CurrentThreadScheduler) {
        my class OnDemandSupply does Supply {
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
                            -> \val { $sub.more().(val) },
                            done => { if !$closed && $sub.done -> $t { $t() } },
                            quit => -> $ex { if !$closed && $sub.quit -> $t { $t($ex) } }
                        );
                        &!producer($s)
                    },
                    :catch(-> $ex { if !$closed && $sub.quit -> $t { $t($ex) } })
                );
                $sub
            }
        }
        OnDemandSupply.new(:&producer, :&closing, :$scheduler)
    }

    method for(*@values, :$scheduler = CurrentThreadScheduler) {
        my class ForSupply does Supply {
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
                            $sub.more().(val);
                        }
                        if !$closed && $sub.done -> $l { $l() }
                    },
                    :catch(-> $ex { if !$closed && $sub.quit -> $t { $t($ex) } })
                );
                $sub
            }
        }
        ForSupply.new(:@values, :$scheduler)
    }

    method interval($interval, $delay = 0, :$scheduler = $*SCHEDULER) {
        my class IntervalSupply does Supply {
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
                        $sub.more().($i++);
                    },
                    :every($!interval), :in($!delay)
                );
                $sub
            }
        }
        IntervalSupply.new(:$interval, :$delay, :$scheduler)
    }
    
    method flat(Supply $source) {
        my class FlatSupply does Supply does PrivatePublishing {
            has $!source;
            
            submethod BUILD(:$!source) { }
            
            method live { $source.live }
            method tap(|c) {
                my $source_tap;
                my $tap = self.Supply::tap(|c, closing => {$source_tap.close});
                $source_tap = $!source.tap( -> \val {
                      $tap.more().(val.flat)
                  },
                  done => { if $tap.done { $tap.done().() } },
                  quit => -> $ex { if $tap.quit { $tap.quit().($ex) } });
                $tap
            }
        }
        FlatSupply.new(:$source)
    }

    method grep(Supply $source, Mu $test) {
        my class GrepSupply does Supply does PrivatePublishing {
            has $!source;
            has Mu $!test;
            
            submethod BUILD(:$!source, :$!test) { }
            
            method live { $source.live }
            method tap(|c) {
                my $source_tap;
                my $tap = self.Supply::tap(|c, closing => {$source_tap.close});
                $source_tap = $!source.tap( $!test.DEFINITE
                  ?? $!test ~~ Callable
                    ?? $!test ~~ Regex
                       ?? -> \val { $tap.more().(val) if val.match($!test) }
                       !! -> \val { $tap.more().(val) if $!test(val) }
                    !! -> \val { $tap.more().(val) if val ~~ $!test }
                  !! -> \val { $tap.more().(val) if val ~~ $!test },
                  done => { if $tap.done { $tap.done().() } },
                  quit => -> $ex { if $tap.quit { $tap.quit().($ex) } }
                );
                $tap
            }
        }
        GrepSupply.new(:$source, :$test)
    }

    method map(Supply $source, &mapper) {
        my class MapSupply does Supply does PrivatePublishing {
            has $!source;
            has &!mapper;
            
            submethod BUILD(:$!source, :&!mapper) { }
            
            method live { $source.live }
            method tap(|c) {
                my $source_tap;
                my $tap = self.Supply::tap(|c, closing => {$source_tap.close});
                $source_tap = $!source.tap( -> \val {
                      $tap.more().(&!mapper(val))
                  },
                  done => { if $tap.done { $tap.done().() } },
                  quit => -> $ex { if $tap.quit { $tap.quit().($ex) } });
                $tap
            }
        }
        MapSupply.new(:$source, :&mapper)
    }

    method schedule_on(Supply $source, Scheduler $scheduler) {
        my class ScheduleSupply does Supply does PrivatePublishing {
            has $!source;
            has $!scheduler;
            
            submethod BUILD(:$!source, :$!scheduler) { }
            
            method live { $source.live }
            method tap(|c) {
                my $source_tap;
                my $tap = self.Supply::tap(|c, closing => {$source_tap.close});
                $source_tap = $!source.tap( -> \val {
                      $!scheduler.cue: { $tap.more().(val) }
                  },
                  done => { $!scheduler.cue: { if $tap.done { $tap.done().() } } },
                  quit => -> $ex { if $tap.quit { $tap.quit().($ex) } });
                $tap
            }
        }
        ScheduleSupply.new(:$source, :$scheduler)
    }
    
    method start(Supply $s, &startee) {
        my class StartSupply does Supply does PrivatePublishing {
            has $!value;
            has &!startee;
            
            submethod BUILD(:$!value, :&!startee) { }
            
            method live { $s.live }
            method tap(|c) {
                my $sub = self.Supply::tap(|c);
                Promise.start({ &!startee($!value) }).then({
                    if .status == Kept {
                        self!more(.result);
                        self!done();
                    }
                    else {
                        self!quit(.cause);
                    }
                });
                $sub
            }
        }
        self.map($s, -> \value {
            StartSupply.new(:value(value), :&startee)
        })
    }

    method stable(Supply $source, $time, :$scheduler = $*SCHEDULER) {

        return $source if !$time;  # nothing to do

        my class StableSupply does Supply does PrivatePublishing {
            has $!source;
            has $!time;
            has $!scheduler;
            has $!lock;
            has $!last_cancellation;
            
            submethod BUILD(:$!source, :$!time, :$!scheduler) {
                $!lock = Lock.new;
            }
            
            method live { $source.live }
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
                                    $tap.more().(val);
                                });
                        });
                    },
                    done => { if $tap.done { $tap.done().() } },
                    quit => -> $ex { if $tap.quit { $tap.quit().($ex) } });
                $tap
            }
        }
        StableSupply.new(:$source, :$time, :$scheduler);
    }

    method delay(Supply $source, $time, :$scheduler = $*SCHEDULER) {

        return $source if !$time;  # nothing to do

        my class DelaySupply does Supply does PrivatePublishing {
            has $!source;
            has $!time;
            has $!scheduler;
            
            submethod BUILD(:$!source, :$!time, :$!scheduler) { }
            
            method live { $source.live }
            method tap(|c) {
                my $source_tap;
                my $tap = self.Supply::tap(|c, closing => {$source_tap.close});
                $source_tap = $!source.tap(
                    -> \val {
                        $!scheduler.cue( { $tap.more().(val) }, :in($time) );
                    },
                    done => {
                        $!scheduler.cue( { if $tap.done { $tap.done().() } }, :in($time) );
                    },
                    quit => -> $ex {
                        $!scheduler.cue( { if $tap.quit { $tap.quit().($ex) } }, :in($time) );
                    } );
                $tap
            }
        }
        DelaySupply.new(:$source, :$time, :$scheduler);
    }

    method migrate(Supply $source) {
        my class MigrateSupply does Supply does PrivatePublishing {
            has $!source;
            has $!current;
            has $!lock;
            
            submethod BUILD(:$!source) {
                $!lock = Lock.new;
            }
            
            method live { $source.live }
            method tap(|c) {
                my $source_tap;
                my $tap = self.Supply::tap(|c, closing => {$source_tap.close});
                $source_tap = $!source.tap(
                    -> \inner_supply {
                        X::Supply::Migrate::Needs.new.throw
                          unless inner_supply ~~ Supply;
                        $!lock.protect({
                            $!current.close() if $!current;
                            $!current = inner_supply.tap(-> \val {
                                $tap.more().(val);
                            });
                        });
                    },
                    done => { if $tap.done { $tap.done().() } },
                    quit => -> $ex { if $tap.quit { $tap.quit().($ex) } });
                $tap
            }
        }
        MigrateSupply.new(:$source)
    }

    method classify(Supply $source, &mapper, :$multi ) {
        my class ClassifySupply does Supply does PrivatePublishing {
            has $!source;
            has %!mapping;
            
            submethod BUILD(:$!source) { }
            submethod find_supply ($key) {
                %!mapping{ $key.WHICH } //= do {
                    my $s = Supply.new;
                    self!more($key => $s);
                    $s;
                };
            }
            
            method live { $source.live }
            method tap(|c) {
                my $source_tap;
                my $tap = self.Supply::tap(|c, closing => {$source_tap.close});
                $source_tap = $!source.tap( $multi
                  ?? -> \val {
                      for @(mapper(val)) -> $key {
                          self.find_supply($key).more(val);
                      }
                  }
                  !! -> \val {
                      self.find_supply( mapper(val) ).more(val);
                  },
                  done => { self!done(); },
                  quit => -> $ex { self!quit($ex) });
                $tap
            }
        }
        ClassifySupply.new(:$source)
    }
}

# vim: ft=perl6 expandtab sw=4
