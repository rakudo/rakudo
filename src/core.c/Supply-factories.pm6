# continued from src/core.c/Supply.pm6

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
        Supply.new(OnClose.new(source => self, :&on-close))
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

    method first(Supply:D: :$end, |c) {
        c.list
          ?? $end
            ?? self.grep(|c).tail
            !! self.grep(|c).head
          !! $end
            ?? self.tail
            !! self.head
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
        $time
          ?? Supply.new(
               Stable.new(source => self.sanitize, :$time, :$scheduler)
             )
          !! self
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
            my $reduced := nqp::null;
            whenever self -> \value {
                $reduced := nqp::isnull($reduced)
                  ?? value
                  !! with($reduced, value);
                LAST {
                    emit nqp::ifnull($reduced,Nil);
                }
            }
        }
    }

    method produce(Supply:D: &with) {
        supply {
            my $reduced := nqp::null;
            whenever self -> \value {
                emit $reduced := nqp::isnull($reduced)
                  ?? value
                  !! with($reduced, value);
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

    # comb the supply for characters
    proto method comb(|) {*}
    multi method comb(Supply:D:) {
        supply {
            whenever self -> str $str {
                for ^nqp::chars($str) -> int $i {
                    emit nqp::box_s(nqp::substr($str,$i,1),Str);
                }
            }
        }
    }

    # comb the supply for N characters at a time
    multi method comb(Supply:D: Int:D $the-batch) {
        $the-batch <= 1
          ?? self.comb
          !! supply {
                 my str $str;
                 my int $batch = $the-batch;
                 whenever self -> str $val {
                     $str = nqp::concat($str,$val);

                     my int $i;
                     my int $times = nqp::chars($str) div $batch;
                     nqp::while(
                       $times--,
                       nqp::stmts(
                         emit(nqp::box_s(nqp::substr($str,$i,$batch),Str)),
                         ($i = $i + $batch)
                       )
                     );
                     $str = nqp::substr($str,$i);

                     LAST { emit $str if nqp::chars($str) }
                 }
             }
    }

    # comb the supply for a Str needle
    multi method comb(Supply:D: Str:D $the-needle) {
        $the-needle
          ?? supply {
                 my str $str;
                 my str $needle = $the-needle;
                 my int $len = nqp::chars($needle);
                 whenever self -> str $val {
                     $str = nqp::concat($str,$val);

                     my int $i;
                     my int $pos;
                     nqp::while(
                       nqp::isgt_i(($i = nqp::index($str,$needle,$pos)),-1),
                       nqp::stmts(
                         emit($the-needle),
                         ($pos = $i + $len)
                       )
                     );
                     $str = nqp::substr($str,$pos);
                 }
             }
          !! self.comb
    }

    # Specifying :match forces a collect of all strings first
    multi method comb(Supply:D: Regex:D $matcher, :$match!, |c) {
        $match
          ?? supply {
                 my $parts := nqp::list_s;
                 whenever self -> str $val {
                     nqp::push_s($parts,$val);
                     LAST {
                         emit $_
                           for nqp::join('',$parts).comb($matcher, :match, |c);
                     }
                 }
             }
          !! self.comb($matcher, |c)
    }

    # comb the supply for a Regex needle
    multi method comb(Supply:D: Regex:D $matcher) {
        supply {
            my str $str;
            whenever self -> str $val {
                $str = nqp::concat($str,$val);
                my @matches = $str.comb($matcher, :match);
                emit .Str for @matches;
                $str = nqp::substr($str,@matches.tail.pos) if @matches;
            }
        }
    }

    # comb the supply for a Str needle for a max number of time
    multi method comb(Supply:D: \the-thing, \the-limit) {
        self.comb(the-thing).head(the-limit)
    }

    # split the supply on the needle and adverbs
    multi method split(Supply:D: \needle) {
        supply {
            my $str = "";  # prevent warning on first batch
            my @matches;
            whenever self -> \value {
                done unless @matches = ($str ~ value).split(needle, |%_);
                $str = @matches.pop.Str;  # keep last for next batch

                emit .Str for @matches;

                LAST { emit $str }
            }
        }
    }

    # split the supply on the needle, limit and adverbs
    multi method split(Supply:D: \needle, \the-limit) {
        self.split(needle, |%_).head(the-limit)
    }

    # encode chunks with the given encoding
    method encode(Supply:D: $encoding = "utf8") {
        supply {
            whenever self -> \val {
                emit val.encode($encoding);
            }
        }
    }

    # decode chunks with the given encoding
    method decode(Supply:D: $encoding = "utf8") {
        supply {
            my str $str;
            whenever self -> \val {
                my str $decoded = nqp::concat($str,val.decode($encoding));
                if nqp::chars($decoded) > 1 {
                    emit nqp::box_s(
                      nqp::substr($decoded,0,nqp::chars($decoded) - 1),
                      Str
                    );
                    $str = nqp::substr($decoded,nqp::chars($decoded) - 1);
                }
                else  {
                    $str = $decoded;
                }

                LAST { emit nqp::box_s($str,Str) if nqp::chars($str) }
            }
        }
    }

# continued in src/core.c/Supply-coercers.pm6

# vim: expandtab shiftwidth=4
