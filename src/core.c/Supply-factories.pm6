    ## Supply factories
    ##

    my class OnDemand does Tappable {
        has &!producer;
        has &!closing;
        has $!scheduler;

        submethod BUILD(:&!producer!, :&!closing!, :$!scheduler! --> Nil) {}

        method tap(&emit, &done, &quit, &tap) {
            my int $closed = 0;
            my $t = Tap.new: {
                if &!closing {
                    &!closing() unless $closed++;
                }
            }
            tap($t);
            my $p = Supplier.new;
            $p.Supply.tap(&emit,
                done => {
                    done();
                    $t.close();
                },
                quit => -> \ex {
                    quit(ex);
                    $t.close();
                });
            $!scheduler.cue({ &!producer($p) },
                catch => -> \ex { $p.quit(ex) });
            $t
        }

        method live(--> False) { }
        method sane(--> False) { }
        method serial(--> False) { }
    }
    method on-demand(Supply:U: &producer, :&closing, :$scheduler = CurrentThreadScheduler) {
        Supply.new(OnDemand.new(:&producer, :&closing, :$scheduler)).sanitize
    }

    method from-list(Supply:U: +@values, :$scheduler = CurrentThreadScheduler) {
        self.on-demand(-> $p {
            $p.emit($_) for @values;
            $p.done();
        }, :$scheduler);
    }

    my class Interval does Tappable {
        has $!scheduler;
        has $!interval;
        has $!delay;

        submethod BUILD(:$!scheduler, :$!interval, :$!delay --> Nil) { }

        method tap(&emit, &, &, &tap) {
            my $i = 0;
            my $lock = Lock::Async.new;
            $lock.protect: {
                my $cancellation = $!scheduler.cue(
                    {
                        $lock.protect: { emit $i++ };
                        CATCH { $cancellation.cancel if $cancellation }
                    },
                    :every($!interval), :in($!delay)
                );
                my $t = Tap.new({ $cancellation.cancel });
                tap($t);
                $t
            }
        }

        method live(--> False) { }
        method sane(--> True) { }
        method serial(--> True) { }
    }
    method interval(Supply:U: $interval, $delay = 0, :$scheduler = $*SCHEDULER) {
        Supply.new(Interval.new(:$interval, :$delay, :$scheduler));
    }

    ##
    ## Simple operations are those that operate on a single Supply, carry its
    ## liveness, and are always serial. We implement the directly as they are
    ## common and fairly "hot path".
    ##

    my role SimpleOpTappable does Tappable {
        has $!source;
        method live() { $!source.live }
        method sane(--> True) { }
        method serial(--> True) { }
        method !cleanup(int $cleaned-up is rw, $source-tap) {
            if $source-tap && !$cleaned-up  {
                $cleaned-up = 1;
                $source-tap.close;
            }
        }
    }

    my class Serialize does SimpleOpTappable {
        submethod BUILD(:$!source! --> Nil) { }

        method tap(&emit, &done, &quit, &tap) {
            my $lock = Lock::Async.new;
            my int $cleaned-up = 0;
            my $source-tap;
            my $t;
            $!source.tap(
                tap => {
                    $source-tap = $_;
                    $t = Tap.new({ self!cleanup($cleaned-up, $source-tap) });
                    tap($t);
                },
                -> \value{
                    $lock.protect-or-queue-on-recursion: { emit(value); }
                },
                done => -> {
                    $lock.protect-or-queue-on-recursion: {
                        done();
                        self!cleanup($cleaned-up, $source-tap);
                    }
                },
                quit => -> $ex {
                    $lock.protect-or-queue-on-recursion: {
                        quit($ex);
                        self!cleanup($cleaned-up, $source-tap);
                    }
                });
            $t
        }
    }
    method serialize(Supply:D:) {
        $!tappable.serial ?? self !! Supply.new(Serialize.new(source => self))
    }

    my class Sanitize does SimpleOpTappable {
        submethod BUILD(:$!source! --> Nil) { }

        method tap(&emit, &done, &quit, &tap) {
            my int $cleaned-up = 0;
            my int $finished = 0;
            my $source-tap;
            my $t;
            $!source.tap(
                tap => {
                    $source-tap = $_;
                    $t = Tap.new({ self!cleanup($cleaned-up, $source-tap) });
                    tap($t);
                },
                -> \value{
                    emit(value) unless $finished;
                },
                done => -> {
                    unless $finished {
                        $finished = 1;
                        done();
                        self!cleanup($cleaned-up, $source-tap);
                    }
                },
                quit => -> $ex {
                    unless $finished {
                        $finished = 1;
                        quit($ex);
                        self!cleanup($cleaned-up, $source-tap);
                    }
                });
            $t
        }
    }
    method sanitize() {
        $!tappable.sane ?? self !! Supply.new(Sanitize.new(source => self.serialize))
    }

    my class OnClose does SimpleOpTappable {
        has &!on-close;

        submethod BUILD(:$!source!, :&!on-close! --> Nil) { }

        method tap(&emit, &done, &quit, &tap) {
            my int $cleaned-up = 0;
            my $t;
            $!source.tap: &emit, :&done, :&quit, tap => -> $source-tap {
                $t = Tap.new({
                    &!on-close();
                    self!cleanup($cleaned-up, $source-tap)
                });
                tap($t);
            }
            $t
        }
    }
    method on-close(Supply:D: &on-close) {
        return Supply.new(OnClose.new(source => self, :&on-close))
    }

    my class MapSupply does SimpleOpTappable {
        has &!mapper;

        submethod BUILD(:$!source!, :&!mapper! --> Nil) { }

        method tap(&emit, &done, &quit, &tap) {
            my int $cleaned-up = 0;
            my $source-tap;
            my $t;
            $!source.tap(
                tap => {
                    $source-tap = $_;
                    $t = Tap.new({ self!cleanup($cleaned-up, $source-tap) });
                    tap($t);
                },
                -> \value {
                    my \result = try &!mapper(value);
                    if $! {
                        quit($!);
                        self!cleanup($cleaned-up, $source-tap);
                    }
                    else {
                        emit(result)
                    }
                },
                done => -> {
                    done();
                    self!cleanup($cleaned-up, $source-tap);
                },
                quit => -> $ex {
                    quit($ex);
                    self!cleanup($cleaned-up, $source-tap);
                });
            $t
        }
    }
    method map(Supply:D: &mapper) {
        Supply.new(MapSupply.new(source => self.sanitize, :&mapper))
    }

    my class Grep does SimpleOpTappable {
        has Mu $!test;

        submethod BUILD(:$!source!, Mu :$!test! --> Nil) { }

        method tap(&emit, &done, &quit, &tap) {
            my int $cleaned-up = 0;
            my $source-tap;
            my $t;
            $!source.tap(
                tap => {
                    $source-tap = $_;
                    $t = Tap.new({ self!cleanup($cleaned-up, $source-tap) });
                    tap($t);
                },
                -> \value {
                    if try $!test.ACCEPTS(value) {
                        emit(value);
                    }
                    elsif $! {
                        quit($!);
                        self!cleanup($cleaned-up, $source-tap);
                    }
                },
                done => -> {
                    done();
                    self!cleanup($cleaned-up, $source-tap);
                },
                quit => -> $ex {
                    quit($ex);
                    self!cleanup($cleaned-up, $source-tap);
                });
            $t
        }
    }
    method grep(Supply:D: Mu $test) {
        Supply.new(Grep.new(source => self.sanitize, :$test))
    }

    my class ScheduleOn does SimpleOpTappable {
        has $!scheduler;

        submethod BUILD(:$!source!, :$!scheduler! --> Nil) { }

        method tap(&emit, &done, &quit, &tap) {
            my int $cleaned-up = 0;
            my $source-tap;
            my $t;
            $!source.tap(
                tap => {
                    $source-tap = $_;
                    $t = Tap.new({ self!cleanup($cleaned-up, $source-tap) });
                    tap($t);
                },
                -> \value {
                    $!scheduler.cue: { emit(value) }
                },
                done => -> {
                    $!scheduler.cue: { done(); self!cleanup($cleaned-up, $source-tap); }
                },
                quit => -> $ex {
                    $!scheduler.cue: { quit($ex); self!cleanup($cleaned-up, $source-tap); }
                });
            $t
        }
    }
    method schedule-on(Supply:D: Scheduler $scheduler) {
        Supply.new(ScheduleOn.new(source => self.sanitize, :$scheduler))
    }

    my class Start does SimpleOpTappable {
        has $!value;
        has &!startee;

        submethod BUILD(:$!value, :&!startee --> Nil) { }

        method tap(&emit, &done, &quit, &tap) {
            my int $closed = 0;
            my $t = Tap.new({ $closed = 1 });
            tap($t);
            Promise.start({ &!startee($!value) }).then({
                unless $closed {
                    if .status == Kept {
                        emit(.result);
                        done();
                    }
                    else {
                        quit(.cause);
                    }
                }
            });
            $t
        }
    }
    method start(Supply:D: &startee) {
        self.map: -> \value {
            Supply.new(Start.new(:value(value), :&startee))
        }
    }

    my class Stable does SimpleOpTappable {
        has $!time;
        has $!scheduler;

        submethod BUILD(:$!source!, :$!time!, :$!scheduler! --> Nil) { }

        method tap(&emit, &done, &quit, &tap) {
            my int $cleaned-up = 0;
            my $lock = Lock::Async.new;
            my $last_cancellation;
            my $source-tap;
            my $t;
            $!source.tap(
                tap => {
                    $source-tap = $_;
                    $t = Tap.new({ self!cleanup($cleaned-up, $source-tap) });
                    tap($t);
                },
                -> \value {
                    $lock.protect: {
                        if $last_cancellation {
                            $last_cancellation.cancel;
                        }
                        $last_cancellation = $!scheduler.cue(
                            :in($!time),
                            {
                                $lock.protect: { $last_cancellation = Nil; }
                                try {
                                    emit(value);
                                    CATCH {
                                        default {
                                            quit($_);
                                            self!cleanup($cleaned-up, $source-tap);
                                        }
                                    }
                                }
                            });
                    }
                },
                done => -> {
                    done();
                    self!cleanup($cleaned-up, $source-tap);
                },
                quit => -> $ex {
                    quit($ex);
                    self!cleanup($cleaned-up, $source-tap);
                });
            $t
        }
    }
    method stable(Supply:D: $time, :$scheduler = $*SCHEDULER) {
        return self unless $time;
        Supply.new(Stable.new(source => self.sanitize, :$time, :$scheduler))
    }

    my class Delayed does SimpleOpTappable {
        has $!time;
        has $!scheduler;

        submethod BUILD(:$!source!, :$!time, :$!scheduler! --> Nil) { }

        method tap(&emit, &done, &quit, &tap) {
            my int $cleaned-up = 0;
            my $source-tap;
            my $t;
            $!source.tap(
                tap => {
                    $source-tap = $_;
                    $t = Tap.new({ self!cleanup($cleaned-up, $source-tap) });
                    tap($t);
                },
                -> \value {
                    $!scheduler.cue: { emit(value) }, :in($!time)
                },
                done => -> {
                    $!scheduler.cue:
                        { done(); self!cleanup($cleaned-up, $source-tap); },
                        :in($!time)
                },
                quit => -> $ex {
                    $!scheduler.cue:
                        { quit($ex); self!cleanup($cleaned-up, $source-tap); },
                        :in($!time)
                });
            $t
        }
    }
    method delayed(Supply:D: $time, :$scheduler = $*SCHEDULER) {
        $time
          ?? Supply.new(Delayed.new(source => self.sanitize,:$time,:$scheduler))
          !! self    # nothing to do
    }

    ##
    ## A bunch of the more complex combinators, implemented as supply blocks
    ##

    method do(Supply:D: &side-effect) {
        supply {
            whenever self -> \value {
                side-effect(value);
                emit(value);
            }
        }
    }

    method flat(Supply:D:) {
        supply {
            whenever self -> \inner {
                whenever inner -> \value {
                    emit value;
                }
            }
        }
    }

    method merge(*@s) {
        @s.unshift(self) if self.DEFINITE;  # add if instance method
        return supply { } unless +@s;       # nothing to be done

        X::Supply::Combinator.new(
           combinator => 'merge'
        ).throw unless Rakudo::Internals.ALL_DEFINED_TYPE(@s,Supply);

        return @s[0].sanitize  if +@s == 1; # nothing to be done

        supply {
            for @s {
                whenever $_ -> \value { emit(value) }
            }
        }
    }

    method reduce(Supply:D: &with) {
        supply {
            my $first := True;
            my $reduced := Nil;
            whenever self -> \value {
                if $first {
                    $reduced := value;
                    $first := False;
                }
                else {
                    $reduced := with($reduced, value);
                }
                LAST {
                    emit $reduced;
                }
            }
        }
    }

    method produce(Supply:D: &with) {
        supply {
            my $first := True;
            my $reduced := Nil;
            whenever self -> \value {
                if $first {
                    $reduced := value;
                    $first := False;
                }
                else {
                    $reduced := with($reduced, value);
                }
                emit $reduced;
            }
        }
    }

    method migrate(Supply:D:) {
        supply {
            my $current;
            whenever self -> \inner {
                X::Supply::Migrate::Needs.new.throw
                    unless nqp::istype(inner, Supply);
                $current.close if $current;
                $current = do whenever inner -> \value {
                    emit(value);
                }
            }
        }
    }

    proto method classify(|) {*}
    multi method classify(Supply:D: &mapper )  {
        self!classify(&mapper);
    }
    multi method classify(Supply:D: %mapper )  {
        self!classify({ %mapper{$^a} });
    }
    multi method classify(Supply:D: @mapper )  {
        self!classify({ @mapper[$^a] });
    }

    proto method categorize (|) {*}
    multi method categorize(Supply:D: &mapper )  {
        self!classify(&mapper, :multi);
    }
    multi method categorize(Supply:D: %mapper )  {
        self!classify({ %mapper{$^a} }, :multi);
    }
    multi method categorize(Supply:D: @mapper )  {
        self!classify({ @mapper[$^a] }, :multi);
    }

    method !classify(&mapper, :$multi) {
        supply {
            my %mapping;

            sub find-target($key) {
                %mapping{ $key.WHICH } //= do {
                    my $p = Supplier::Preserving.new;
                    emit($key => $p.Supply);
                    $p
                };
            }

            whenever self -> \value {
                if $multi {
                    for @(mapper(value)) -> $key {
                        find-target($key).emit(value);
                    }
                }
                else {
                    find-target(mapper(value)).emit(value);
                }
                LAST {
                    %mapping.values>>.done;
                }
            }
        }
    }

    # split the supply on the given string
    multi method split(Supply:D: Str(Cool) $the-needle) {
        supply {
            my str $needle = $the-needle;

            my str $str;
            whenever self -> str $val {
                my $matches := nqp::split($needle,nqp::concat($str,$val));
                $str = nqp::pop($matches);  # keep last for next batch

                my $iterator := nqp::iterator($matches);
                nqp::while(
                  $iterator,
                  emit nqp::p6box_s(nqp::shift($iterator))
                );

                LAST {
                    emit nqp::p6box_s($str);
                }
            }
        }
    }

    # split the supply on the given string
    multi method split(Supply:D: Str(Cool) $the-needle, :$skip-empty!) {
        $skip-empty
          ?? supply {                            # skip empties
                 my str $needle = $the-needle;

                 my str $str;
                 my str $emittee;
                 whenever self -> str $val {
                     my $matches := nqp::split($needle,nqp::concat($str,$val));
                     $str = nqp::pop($matches);  # keep last for next batch

                     my $iterator := nqp::iterator($matches);
                     nqp::while(
                       $iterator,
                       nqp::if(
                         nqp::chars($emittee = nqp::shift($iterator)),
                         emit nqp::p6box_s($emittee)
                       )
                     );

                     LAST {
                         emit nqp::p6box_s($str) if nqp::chars($str);
                     }
                 }
             }
          !! self.split($the-needle)             # don't skip empties
    }

    # split the supply on the given string for a limit
    multi method split(Supply:D: Str(Cool) $the-needle, Whatever) {
        self.split($the-needle, |%_)
    }
    multi method split(Supply:D: Str(Cool) $the-needle, Numeric(Cool) $the-limit) {
        $the-limit == Inf
          ?? self.split($the-needle, |%_)         # there's no limit
          !! $the-limit <= 0                      # there *is* a limit
            ?? supply { }                         # but it won't pass anything
            !! supply {                           # need to pass stuff
                   my str $needle = $the-needle;
                   my int $limit  = $the-limit.Int;
                   my int $emitted;

                   my str $str;
                   whenever self -> str $val {
                       my $matches := nqp::split($needle,nqp::concat($str,$val));
                       $str = nqp::pop($matches);  # keep last for next batch

                       my $iterator := nqp::iterator($matches);
                       nqp::while(
                         $iterator,
                         nqp::if(
                           nqp::isle_i(++$emitted,$limit),
                           emit(nqp::p6box_s(nqp::shift($iterator))),
                           done
                         )
                       );

                       LAST {
                           emit nqp::p6box_s($str) if $emitted < $limit;
                       }
                   }
               }
    }

    # split the supply on the given string for a limit while skipping empty
    multi method split(Supply:D:
      Str(Cool) $the-needle,
      Numeric(Cool) $the-limit,
      :$skip-empty!
    ) {
        $the-limit == Inf
          ?? self.split(                         # there's no limit
               $the-needle, :$skip-empty, |%_
             )
          !! $the-limit <= 0                     # there *is* a limit
            ?? supply { }                        # nothing will pass
            !! $skip-empty                       # will skip empties
              ?? supply {
                     my str $needle = $the-needle;
                     my int $limit  = $the-limit.Int;

                     my str $emittee;
                     my int $emitted;
                     my str $str;
                     whenever self -> str $val {
                         my $matches := nqp::split($needle,nqp::concat($str,$val));
                         $str = nqp::pop($matches);  # keep last for next batch

                         my $iterator := nqp::iterator($matches);
                         nqp::while(
                           $iterator,
                           nqp::if(
                             nqp::chars($emittee = nqp::shift($iterator)),
                             nqp::if(
                               nqp::isle_i(++$emitted,$limit),
                               emit(nqp::p6box_s($emittee)),
                               done
                             )
                           )
                         );

                         LAST {
                             emit nqp::p6box_s($str)
                               if nqp::chars($str) && $emitted < $limit;
                         }
                     }
                 }
              !! self.split(                     # won't skip empties
                   $the-needle, $the-limit, |%_
                 )
    }

# vim: ft=perl6 expandtab sw=4
