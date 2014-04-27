# Operations we can do on Supplies. Note, many of them need to compose
# the Supply role into classes they create along the way, so they must
# be declared outside of Supply.

my class SupplyOperations is repr('Uninstantiable') {
    my @secret;

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
    
    method for(*@values, :$scheduler = $*SCHEDULER) {
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
    
    method flat(Supply $s) {
        my class FlatSupply does Supply does PrivatePublishing {
            has $!source;
            
            submethod BUILD(:$!source) { }
            
            method live { $s.live }
            method tap(|c) {
                my $source_tap;
                my $sub = self.Supply::tap(|c, closing => { $source_tap.close() });
                $source_tap = $!source.tap( -> \val {
                      self!more(val.flat)
                  },
                  done => { self!done(); },
                  quit => -> $ex { self!quit($ex) });
                $sub
            }
        }
        FlatSupply.new(:source($s))
    }

    method do($s, &side_effect) {
        on -> $res {
            $s => sub (\val) { side_effect(val); $res.more(val) }
        }
    }

    method grep(Supply $s, &filter) {
        my class GrepSupply does Supply does PrivatePublishing {
            has $!source;
            has &!filter;
            
            submethod BUILD(:$!source, :&!filter) { }
            
            method live { $s.live }
            method tap(|c) {
                my $source_tap;
                my $sub = self.Supply::tap(|c, closing => { $source_tap.close() });
                $source_tap = $!source.tap( -> \val {
                      if (&!filter(val)) { self!more(val) }
                  },
                  done => { self!done(); },
                  quit => -> $ex { self!quit($ex) }
                );
                $sub
            }
        }
        GrepSupply.new(:source($s), :&filter)
    }

    method uniq(Supply $s, :&as, :&with, :$expires) {
        on -> $res {
            $s => do {
                if $expires {
                    if &with and &with !=== &[===] {
                        my @seen;  # really Mu, but doesn't work in settings
                        my Mu $target;
                        &as
                          ?? -> \val {
                              my $now := now;
                              $target = &as(val);
                              my $index =
                                @seen.first-index({&with($target,$_[0])});
                              if $index.defined {
                                  if $now > @seen[$index][1] {  # expired
                                      @seen[$index][1] = $now+$expires;
                                      $res.more(val);
                                  }
                              }
                              else {
                                  @seen.push: [$target, $now+$expires];
                                  $res.more(val);
                              }
                          }
                          !! -> \val {
                              my $now := now;
                              my $index =
                                @seen.first-index({&with(val,$_[0])});
                              if $index.defined {
                                  if $now > @seen[$index][1] {  # expired
                                      @seen[$index][1] = $now+$expires;
                                      $res.more(val);
                                  }
                              }
                              else {
                                  @seen.push: [val, $now+$expires];
                                  $res.more(val);
                              }
                          };
                    }
                    else {
                        my $seen := nqp::hash();
                        my str $target;
                        &as
                          ?? -> \val {
                              my $now := now;
                              $target = nqp::unbox_s(&as(val).WHICH);
                              if !nqp::existskey($seen,$target) ||
                                $now > nqp::atkey($seen,$target) { #expired
                                  $res.more(val);
                                  nqp::bindkey($seen,$target,$now+$expires);
                              }
                          }
                          !! -> \val {
                              my $now := now;
                              $target = nqp::unbox_s(val.WHICH);
                              if !nqp::existskey($seen,$target) ||
                                $now > nqp::atkey($seen,$target) { #expired
                                  $res.more(val);
                                  nqp::bindkey($seen,$target,$now+$expires);
                              }
                          };
                    }
                }
                else { # !$!expires
                    if &with and &with !=== &[===] {
                        my @seen;  # really Mu, but doesn't work in settings
                        my Mu $target;
                        &as
                          ?? -> \val {
                              $target = &as(val);
                              if @seen.first({ &with($target,$_) } ) =:= Nil {
                                  @seen.push($target);
                                  $res.more(val);
                              }
                          }
                          !! -> \val {
                              if @seen.first({ &with(val,$_) } ) =:= Nil {
                                  @seen.push(val);
                                  $res.more(val);
                              }
                          };
                    }
                    else {
                        my $seen := nqp::hash();
                        my str $target;
                        &as
                          ?? -> \val {
                              $target = nqp::unbox_s(&as(val).WHICH);
                              unless nqp::existskey($seen, $target) {
                                  nqp::bindkey($seen, $target, 1);
                                  $res.more(val);
                              }
                          }
                          !! -> \val {
                              $target = nqp::unbox_s(val.WHICH);
                              unless nqp::existskey($seen, $target) {
                                  nqp::bindkey($seen, $target, 1);
                                  $res.more(val);
                              }
                          };
                    }
                }
            }
        }
    }

    method squish(Supply $s, :&as, :&with is copy) {
        &with //= &[===];
        on -> $res {
            $s => do {
                my Mu $last = @secret;
                my Mu $target;
                &as
                  ?? -> \val {
                      $target = &as(val);
                      unless &with($target,$last) {
                          $last = $target;
                          $res.more(val);
                      }
                  }
                  !! -> \val {
                      unless &with(val,$last) {
                          $last = val;
                          $res.more(val);
                      }
                  };
            }
        }
    }
    
    method map(Supply $s, &mapper) {
        my class MapSupply does Supply does PrivatePublishing {
            has $!source;
            has &!mapper;
            
            submethod BUILD(:$!source, :&!mapper) { }
            
            method live { $s.live }
            method tap(|c) {
                my $source_tap;
                my $sub = self.Supply::tap(|c, closing => { $source_tap.close() });
                $source_tap = $!source.tap( -> \val {
                      self!more(&!mapper(val))
                  },
                  done => { self!done(); },
                  quit => -> $ex { self!quit($ex) });
                $sub
            }
        }
        MapSupply.new(:source($s), :&mapper)
    }

    method rotor(Supply $s, $elems is copy, $overlap is copy ) {

        $elems   //= 2;
        $overlap //= 1;
        return $s if $elems == 1 and $overlap == 0;  # nothing to do

        on -> $res {
            $s => do {
                my @batched;
                sub flush {
                    $res.more( [@batched] );
                    @batched.splice( 0, +@batched - $overlap );
                }

                {
                    more => -> \val {
                        @batched.push: val;
                        flush if @batched.elems == $elems;
                    },
                    done => {
                        flush if @batched;
                        $res.done;
                    }
                }
            }
        }
    }

    method batch(Supply $s, :$elems, :$seconds ) {

        return $s if (!$elems or $elems == 1) and !$seconds;  # nothing to do

        on -> $res {
            $s => do {
                my @batched;
                my $last_time;
                sub flush {
                    $res.more([@batched]);
                    @batched = ();
                }

                {
                    more => do {
                        if $seconds {
                            $last_time = time div $seconds;

                            $elems # and $seconds
                              ??  -> \val {
                                  my $this_time = time div $seconds;
                                  if $this_time != $last_time {
                                      flush if @batched;
                                      $last_time = $this_time;
                                      @batched.push: val;
                                  }
                                  else {
                                      @batched.push: val;
                                      flush if @batched.elems == $elems;
                                  }
                              }
                              !! -> \val {
                                  my $this_time = time div $seconds;
                                  if $this_time != $last_time {
                                      flush if @batched;
                                      $last_time = $this_time;
                                  }
                                  @batched.push: val;
                              }
                        }
                        else { # just $elems
                            -> \val {
                                @batched.push: val;
                                flush if @batched.elems == $elems;
                            }
                        }
                    },
                    done => {
                        flush if @batched;
                        $res.done;
                    }
                }
            }
        }
    }

    method schedule_on(Supply $s, Scheduler $scheduler) {
        my class ScheduleSupply does Supply does PrivatePublishing {
            has $!source;
            has $!scheduler;
            
            submethod BUILD(:$!source, :$!scheduler) { }
            
            method live { $s.live }
            method tap(|c) {
                my $source_tap;
                my $sub = self.Supply::tap(|c, closing => { $source_tap.close() });
                $source_tap = $!source.tap( -> \val {
                      $!scheduler.cue: { self!more(val) }
                  },
                  done => { $!scheduler.cue: { self!done(); } },
                  quit => -> $ex { $!scheduler.cue: { self!quit($ex) } });
                $sub
            }
        }
        ScheduleSupply.new(:source($s), :$scheduler)
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

    method stable(Supply $s, $time, :$scheduler = $*SCHEDULER) {
        my class StableSupply does Supply does PrivatePublishing {
            has $!source;
            has $!time;
            has $!scheduler;
            has $!lock;
            has $!last_cancellation;
            
            submethod BUILD(:$!source, :$!time, :$!scheduler) {
                $!lock = Lock.new;
            }
            
            method live { $s.live }
            method tap(|c) {
                my $source_tap;
                my $sub = self.Supply::tap(|c, closing => { $source_tap.close() });
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
                                    self!more(val);
                                });
                        });
                    },
                    done => { self!done(); },
                    quit => -> $ex { self!quit($ex) });
                $sub
            }
        }
        StableSupply.new(:source($s), :$time, :$scheduler);
    }

    method migrate(Supply $s) {
        my class MigrateSupply does Supply does PrivatePublishing {
            has $!source;
            has $!current;
            has $!lock;
            
            submethod BUILD(:$!source) {
                $!lock = Lock.new;
            }
            
            method live { $s.live }
            method tap(|c) {
                my $source_tap;
                my $sub = self.Supply::tap(|c, closing => { $source_tap.close() });
                $source_tap = $!source.tap(
                    -> \inner_supply {
                        $!lock.protect({
                            $!current.close() if $!current;
                            $!current = inner_supply.tap(-> \val {
                                self!more(val);
                            });
                        });
                    },
                    done => { self!done(); },
                    quit => -> $ex { self!quit($ex) });
                $sub
            }
        }
        MigrateSupply.new(:source($s))
    }

    method merge(*@s) {

        @s.shift unless @s[0].DEFINITE;  # lose if used as class method
        return Supply unless +@s;        # nothing to be done
        return @s[0]  if +@s == 1;       # nothing to be done

        my $dones = 0;
        on -> $res {
            @s => {
                more => -> \val { $res.more(val) },
                done => { $res.done() if ++$dones == +@s }
            },
        }
    }
    
    method zip(*@s, :&with is copy) {

        @s.shift unless @s[0].DEFINITE;  # lose if used as class method
        return Supply unless +@s;        # nothing to be done
        return @s[0]  if +@s == 1;       # nothing to be done

        my &infix:<op> = &with // &[,]; # hack for [[&with]] parse failure
        my @values = ( [] xx +@s );
        on -> $res {
            @s => -> $val, $index {
                @values[$index].push($val);
                if all(@values) {
                    $res.more( [op] @values>>.shift );
                }
            }
        }
    }
}
