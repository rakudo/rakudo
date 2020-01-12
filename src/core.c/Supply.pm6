# When we tap a Supply, we get back a Tap object. We close the tap in order
# to turn off the flow of values.
my class Tap {
    has &!on-close;

    submethod BUILD(:&!on-close --> Nil) { } # for subclasses of Tap

    multi method new(Tap: --> Tap:D) {
        nqp::create(self)
    }
    multi method new(Tap: &on-close --> Tap:D) {
        nqp::if(
          nqp::eqaddr(self.WHAT,Tap),
          nqp::p6bindattrinvres(                 # we're a real Tap, fast path
            nqp::create(self),Tap,'&!on-close',&on-close
          ),
          self.bless(:&on-close)                 # subclass, use slow path
        )
    }

    method close(--> True) {
        nqp::if(
          nqp::isconcrete(&!on-close),
          nqp::if(
            nqp::istype((my \close-result := &!on-close()),Promise),
            (await close-result)
          )
        )
    }
}

# The asynchronous dual of the Iterator role; goes inside of a Supply, which
# is the asynchronous dual of the Seq class. So just as a Seq wraps around an
# Iterator so we don't expose all the internal iterator types to the world, a
# Supply wraps about a Tappable so we don't expose all of those. (It may
# surprise you that it's a Tappable, not a Tap, given Seq wraps an Iterator,
# not an Iterable. Guess that's part of the duality too. Ask your local
# category theorist. :-))
my role Tappable {
    method tap(&emit, &done, &quit, &tap) { ... }
    method live() { ... }    # Taps into a live data source
    method serial() { ... }  # Promises no concurrent emits
    method sane() { ... }    # Matches emit* [done|quit]? grammar
}

# A few Supply-related exception types.
my class X::Supply::Combinator is Exception {
    has $.combinator;
    method message() { "Can only use $!combinator to combine defined Supply objects" }
}
my class X::Supply::Migrate::Needs is Exception {
    method message() {
        ".migrate needs Supplies to be emitted"
    }
}
my class X::Supply::New is Exception {
    method message() {
        "Cannot directly create a Supply. You might want:\n" ~
        " - To use a Supplier in order to get a live supply\n" ~
        " - To use Supply.on-demand to create an on-demand supply\n" ~
        " - To create a Supply using a supply block"
    }
}


# A Supply is like an asynchronous Seq. All the methods that you can do on
# a Supply go in here.
my class Supplier { ... }
my class Supplier::Preserving { ... }
my class Supply does Awaitable {
    has Tappable $!tappable;

    proto method new(|) {*}
    multi method new(Supply:) {
        X::Supply::New.new.throw
    }
    multi method new(Supply: Tappable $tappable) {
        nqp::if(
          nqp::eqaddr(self.WHAT,Supply),
          nqp::p6bindattrinvres(                 # we're a real Supply, fast path
            nqp::create(self),Supply,'$!tappable',$tappable
          ),
          self.bless(:$tappable)                 # subclass, use slow path
        )
    }
    submethod BUILD(Tappable :$!tappable! --> Nil) { }  # for subclasses

    method Capture(Supply:D:) { self.List.Capture }

    method live(Supply:D:) { $!tappable.live }
    method serial(Supply:D:) { $!tappable.serial }
    method Tappable(--> Tappable) { $!tappable }

    my \DISCARD = -> $ {};
    my \NOP = -> {};
    my \DEATH = -> $ex { $ex.throw };
    method tap(Supply:D: &emit = DISCARD, :&done = NOP, :&quit = DEATH, :&tap = DISCARD) {
        $!tappable.tap(&emit, &done, &quit, &tap)
    }

    method act(Supply:D: &actor, *%others) {
        self.sanitize.tap(&actor, |%others)
    }

    ##
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
          ?? supply {
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
          !! self.split($the-needle)
    }

    ##
    ## Coercions
    ##

    multi method Supply(Supply:D:) { self }

    method Channel(Supply:D:) {
        my $c = Channel.new();
        self.sanitize.tap:
            -> \val { $c.send(val) },
            done => { $c.close },
            quit => -> $ex { $c.fail($ex) };
        $c
    }

    my class ConcQueue is repr('ConcBlockingQueue') { }
    multi method list(Supply:D:) {
        self.Seq.list
    }
    method Seq(Supply:D:) {
        gather {
            my Mu \queue = nqp::create(ConcQueue);
            my $exception;
            self.tap(
                -> \val { nqp::push(queue, val) },
                done => -> { nqp::push(queue, ConcQueue) }, # type obj as sentinel
                quit => -> \ex { $exception := ex; nqp::push(queue, ConcQueue) });
            loop {
                my \got = nqp::shift(queue);
                if got =:= ConcQueue {
                    $exception.DEFINITE
                        ?? $exception.throw
                        !! last
                }
                else {
                    take got;
                }
            }
        }
    }

    method Promise(Supply:D:) {
        my $p = Promise.new;
        my $v = $p.vow;
        my $final := Nil;
        my $t = self.tap:
            -> \val { $final := val },
            done => { $v.keep($final) },
            quit => -> \ex { $v.break(ex) };
        $p
    }

    method wait(Supply:D:) { await self.Promise }

    my class SupplyAwaitableHandle does Awaitable::Handle {
        has $!supply;

        method not-ready(Supply:D \supply) {
            nqp::create(self)!not-ready(supply)
        }
        method !not-ready(\supply) {
            $!already = False;
            $!supply := supply;
            self
        }

        method subscribe-awaiter(&subscriber --> Nil) {
            my $final := Nil;
            $!supply.tap:
                -> \val { $final := val },
                done => { subscriber(True, $final) },
                quit => -> \ex { subscriber(False, ex) };
        }
    }

    method get-await-handle(--> Awaitable::Handle) {
        SupplyAwaitableHandle.not-ready(self)
    }

    multi method unique(Supply:D: :&as, :&with, :$expires!) {
        $expires
          ?? supply {
                if &with and !(&with === &[===]) {
                    my @seen;  # really Mu, but doesn't work in settings
                    my Mu $target;
                    if &as {
                        whenever self -> \val {
                            my $now := now;
                            $target = &as(val);
                            my $index =
                              @seen.first({&with($target,$_[0])},:k);
                            with $index {
                                if $now > @seen[$index][1] {  # expired
                                    @seen[$index][1] = $now+$expires;
                                    emit(val);
                                }
                            }
                            else {
                                @seen.push: [$target, $now+$expires];
                                emit(val);
                            }
                        }
                    }
                    else {
                        whenever self -> \val {
                            my $now := now;
                            my $index =
                              @seen.first({&with(val,$_[0])},:k);
                            with $index {
                                if $now > @seen[$index][1] {  # expired
                                    @seen[$index][1] = $now+$expires;
                                    emit(val);
                                }
                            }
                            else {
                                @seen.push: [val, $now+$expires];
                                emit(val);
                            }
                        }
                    }
                }
                else {
                    my $seen := nqp::hash();
                    my str $target;
                    if &as {
                        whenever self -> \val {
                            my $now := now;
                            $target = nqp::unbox_s(&as(val).WHICH);
                            if !nqp::existskey($seen,$target) ||
                              $now > nqp::atkey($seen,$target) { #expired
                                emit(val);
                                nqp::bindkey($seen,$target,$now+$expires);
                            }
                        }
                    }
                    else {
                        whenever self -> \val {
                            my $now := now;
                            $target = nqp::unbox_s(val.WHICH);
                            if !nqp::existskey($seen,$target) ||
                              $now > nqp::atkey($seen,$target) { #expired
                                emit(val);
                                nqp::bindkey($seen,$target,$now+$expires);
                            }
                        }
                    }
                }
            }
          !! self.unique(:&as, :&with)
    }

    multi method unique(Supply:D: :&as, :&with) {
        supply {
            if &with and !(&with === &[===]) {
                my @seen;  # really Mu, but doesn't work in settings
                my Mu $target;
                if &as {
                    whenever self -> \val {
                        $target = &as(val);
                        if @seen.first({ &with($target,$_) } ) =:= Nil {
                            @seen.push($target);
                            emit(val);
                        }
                    }
                }
                else {
                    whenever self -> \val {
                        if @seen.first({ &with(val,$_) } ) =:= Nil {
                            @seen.push(val);
                            emit(val);
                        }
                    }
                }
            }
            else {
                my $seen := nqp::hash();
                my str $target;
                if &as {
                    whenever self -> \val {
                        $target = nqp::unbox_s(&as(val).WHICH);
                        unless nqp::existskey($seen, $target) {
                            nqp::bindkey($seen, $target, 1);
                            emit(val);
                        }
                    }
                }
                else {
                    whenever self -> \val {
                        $target = nqp::unbox_s(val.WHICH);
                        unless nqp::existskey($seen, $target) {
                            nqp::bindkey($seen, $target, 1);
                            emit(val);
                        }
                    }
                }
            }
        }
    }

    method squish(Supply:D: :&as, :&with is copy) {
        &with //= &[===];
        supply {
            my int $first = 1;
            my Mu $last;
            my Mu $target;

            if &as {
                whenever self -> \val {
                    $target = &as(val);
                    if $first || !&with($last,$target) {
                        $first = 0;
                        emit(val);
                    }
                    $last  = $target;
                }
            }
            else {
                whenever self -> \val {
                    if $first || !&with($last, val) {
                        $first = 0;
                        emit(val);
                    }
                    $last = val;
                }
            }
        }
    }

    multi method rotor(Supply:D: Int:D $batch, :$partial) {
        self.rotor(($batch,), :$partial)
    }
    multi method rotor(Supply:D: *@cycle, :$partial) {
        my @c := @cycle.is-lazy ?? @cycle !! (@cycle xx *).flat.cache;
        supply {
            my Int $elems;
            my Int $gap;
            my int $to-skip;
            my int $skip;
            my \c = @c.iterator;

            sub next-batch(--> Nil) {
                given c.pull-one {
                    when Pair {
                        $elems   = +.key;
                        $gap     = +.value;
                        $to-skip = $gap > 0 ?? $gap !! 0;
                    }
                    default {
                        $elems   = +$_;
                        $gap     = 0;
                        $to-skip = 0;
                    }
                }
            }
            next-batch;

            my @batched;
            sub flush(--> Nil) {
                emit( @batched.splice(0, +@batched, @batched[* + $gap .. *]) );
                $skip = $to-skip;
            }

            whenever self -> \val {
                @batched.push: val unless $skip && $skip--;
                if @batched.elems == $elems {
                    flush;
                    next-batch;
                }
                LAST {
                    flush if @batched and $partial;
                }
            }
        }
    }

    method batch(Supply:D: Int(Cool) :$elems = 0, :$seconds) {
        supply {
            my int $max = $elems >= 0 ?? $elems !! 0;
            my $batched := nqp::list;
            my $last_time;
            sub flush(--> Nil) {
                emit($batched);
                $batched := nqp::list;
            }
            sub final-flush(--> Nil) {
                flush if nqp::elems($batched);
            }

            if $seconds {
                $last_time = time div $seconds;

                if $elems > 0 { # and $seconds
                    whenever self -> \val {
                        my $this_time = time div $seconds;
                        if $this_time != $last_time {
                            flush if nqp::elems($batched);
                            $last_time = $this_time;
                            nqp::push($batched,val);
                        }
                        else {
                            nqp::push($batched,val);
                            flush if nqp::iseq_i(nqp::elems($batched),$max);
                        }
                        LAST { final-flush; }
                    }
                }
                else {
                    whenever self -> \val {
                        my $this_time = time div $seconds;
                        if $this_time != $last_time {
                            flush if nqp::elems($batched);
                            $last_time = $this_time;
                        }
                        nqp::push($batched,val);
                        LAST { final-flush; }
                    }
                }
            }
            else { # just $elems
                whenever self -> \val {
                    nqp::push($batched,val);
                    flush if nqp::isge_i(nqp::elems($batched),$max);
                    LAST { final-flush; }
                }
            }
        }
    }

    proto method lines(|) {*}

    # optional chomping lines from a Supply
    multi method lines(Supply:D: :$chomp! ) {
        $chomp
          ?? self.lines            # need to chomp
          !! supply {              # no chomping wanted
                 my str $str;
                 my int $left;
                 my int $pos;
                 my int $nextpos;

                 whenever self -> str $val {
                     $str = nqp::concat($str,$val);
                     $pos = 0;

                     while ($left = nqp::chars($str) - $pos) > 0 {
                         $nextpos = nqp::findcclass(
                           nqp::const::CCLASS_NEWLINE,$str,$pos,$left);

                         last
                           if $nextpos >= nqp::chars($str)        # no line delimiter
                           or nqp::eqat($str,"\r",$nextpos)       # broken CRLF?
                             && $nextpos == nqp::chars($str) - 1; # yes!

                         emit nqp::p6box_s(nqp::substr($str,$pos,$nextpos - $pos + 1));
                         $pos = $nextpos + 1;
                     }
                     $str = nqp::substr($str,$pos);

                     LAST {
                         emit nqp::p6box_s($str) if nqp::chars($str);
                     }
                 }
             }
    }

    # chomping lines from a Supply
    multi method lines(Supply:D:) {
        supply {
            my str $str;
            my int $pos;
            my int $left;
            my int $nextpos;

            whenever self -> str $val {
                $str = nqp::concat($str,$val);
                $pos = 0;

                while ($left = nqp::chars($str) - $pos) > 0 {
                    $nextpos = nqp::findcclass(
                      nqp::const::CCLASS_NEWLINE,$str,$pos,$left);

                    last
                      if $nextpos >= nqp::chars($str)        # no line delimiter
                      or nqp::eqat($str,"\r",$nextpos)       # broken CRLF?
                        && $nextpos == nqp::chars($str) - 1; # yes!

                    emit nqp::p6box_s(nqp::substr($str,$pos,$nextpos - $pos));
                    $pos = $nextpos + 1;
                }
                $str = nqp::substr($str,$pos);

                LAST {
                    emit nqp::p6box_s(nqp::substr($str,0,
                      nqp::chars($str) - nqp::iscclass(    # skip whitespace at end
                        nqp::const::CCLASS_NEWLINE,$str,nqp::chars($str) - 1)
                    )) if nqp::chars($str);
                }
            }
        }
    }

    method words(Supply:D:) {
        supply {
            my str $str;
            my int $left;
            my int $pos;
            my int $nextpos;

            whenever self -> str $val {
                $str = nqp::concat($str,$val);
                $pos = nqp::findnotcclass(
                  nqp::const::CCLASS_WHITESPACE,$str,0,nqp::chars($str));

                while ($left = nqp::chars($str) - $pos) > 0 {
                    $nextpos = nqp::findcclass(
                      nqp::const::CCLASS_WHITESPACE,$str,$pos,$left);

                    last unless $left = nqp::chars($str) - $nextpos; # broken word

                    emit nqp::p6box_s(nqp::substr($str,$pos,$nextpos - $pos));

                    $pos = nqp::findnotcclass(
                      nqp::const::CCLASS_WHITESPACE,$str,$nextpos,$left);
                }
                $str = nqp::substr($str,$pos);

                LAST {
                    emit nqp::p6box_s($str) if nqp::chars($str);
                }
            }
        }
    }

    multi method elems(Supply:D:) {
        supply {
            my int $elems;
            whenever self { emit ++$elems }
        }
    }
    multi method elems(Supply:D: $seconds ) {
        supply {
            my $last-time := nqp::time_i() div $seconds;
            my $this-time;

            my int $elems;
            my int $last-elems;

            whenever self {
                $last-elems = ++$elems;
                $this-time := nqp::time_i() div $seconds;

                if $this-time != $last-time {
                    emit $elems;
                    $last-time := $this-time;
                }
                LAST emit $elems if $elems != $last-elems;
            }
        }
    }

    multi method head(Supply:D:) {
        supply { whenever self -> \val { emit val; done } }
    }
    multi method head(Supply:D: Int(Cool) $number) {
        supply {
            my int $todo = $number > 0 && $number + 1;
            whenever self -> \val {
                --$todo
                  ?? emit(val)
                  !! done
            }
        }
    }

    method tail(Supply:D: Int(Cool) $number = 1) {
        my int $size = $number;

        supply {
            if $size == 1 {
                my $last;
                whenever self -> \val {
                    $last := val;
                    LAST emit $last;
                }
            }
            elsif $size > 1 {
                my $lastn := nqp::list;
                my int $index = 0;
                nqp::setelems($lastn,$number);  # presize list
                nqp::setelems($lastn,0);

                whenever self -> \val {
                    nqp::bindpos($lastn,$index,val);
                    $index = ($index + 1) % $size;
                    LAST {
                        my int $todo = nqp::elems($lastn);
                        $index = 0           # start from beginning
                          if $todo < $size;  # if not a full set
                        while $todo {
                            emit nqp::atpos($lastn,$index);
                            $index = ($index + 1) % $size;
                            $todo = $todo - 1;
                        }
                    }
                }
            }
            else {  # number <= 0, needed to keep tap open
                whenever self -> \val { }
            }
        }
    }

    method skip(Supply:D: Int(Cool) $number = 1) {
        supply {
            my int $size = $number + 1;
            my int $skipping = $size > 1;
            whenever self {
                .emit unless $skipping && ($skipping = --$size)
            }
        }
    }

    method min(Supply:D: &by = &infix:<cmp>) {
        my &cmp = &by.arity == 2 ?? &by !! { by($^a) cmp by($^b) }
        supply {
            my $min;
            whenever self -> \val {
                if val.defined and !$min.defined || cmp(val,$min) < 0 {
                    emit( $min := val );
                }
            }
        }
    }

    method max(Supply:D: &by = &infix:<cmp>) {
        my &cmp = &by.arity == 2 ?? &by !! { by($^a) cmp by($^b) }
        supply {
            my $max;
            whenever self -> \val {
                 if val.defined and !$max.defined || cmp(val,$max) > 0 {
                     emit( $max = val );
                 }
            }
        }
    }

    method minmax(Supply:D: &by = &infix:<cmp>) {
        my &cmp = &by.arity == 2 ?? &by !! { by($^a) cmp by($^b) }
        supply {
            my $min;
            my $max;
            whenever self -> \val {
                if nqp::istype(val,Failure) {
                    val.throw;  # XXX or just ignore ???
                }
                elsif val.defined {
                    if !$min.defined {
                        emit( Range.new($min = val, $max = val) );
                    }
                    elsif cmp(val,$min) < 0 {
                        emit( Range.new( $min = val, $max ) );
                    }
                    elsif cmp(val,$max) > 0 {
                        emit( Range.new( $min, $max = val ) );
                    }
                }
            }
        }
    }

    method grab(Supply:D: &when_done) {
        supply {
            my @seen;
            whenever self -> \val {
                @seen.push: val;
                LAST {
                    emit($_) for when_done(@seen);
                }
            }
        }
    }

    method reverse(Supply:D:)        { self.grab( {.reverse} ) }
    multi method sort(Supply:D:)     { self.grab( {.sort} ) }
    multi method sort(Supply:D: &by) { self.grab( {.sort(&by)} ) }

    method zip(**@s, :&with) {
        @s.unshift(self) if self.DEFINITE;  # add if instance method
        return supply { } unless +@s;       # nothing to be done

        X::Supply::Combinator.new(
           combinator => 'zip'
        ).throw unless Rakudo::Internals.ALL_DEFINED_TYPE(@s,Supply);

        return @s[0]  if +@s == 1;          # nothing to be done

        supply {
            my @values = nqp::create(Array) xx +@s;
            for @s.kv -> $index, $supply {
                if &with {
                    whenever $supply -> \val {
                        @values[$index].push(val);
                        emit( [[&with]] @values.map(*.shift) ) if all(@values);
                        LAST { done }
                    }
                }
                else {
                    whenever $supply -> \val {
                        @values[$index].push(val);
                        emit( $(@values.map(*.shift).list.eager) ) if all(@values);
                        LAST { done }
                    }
                }
            }
        }
    }

    method zip-latest(**@s, :&with, :$initial ) {
        @s.unshift(self) if self.DEFINITE;  # add if instance method
        return supply { } unless +@s;       # nothing to do.

        X::Supply::Combinator.new(
           combinator => 'zip-latest'
        ).throw unless Rakudo::Internals.ALL_DEFINED_TYPE(@s,Supply);

        return @s[0] if +@s == 1;           # nothing to do.

        supply {
            my @values;

            my $uninitialised = +@s; # how many supplies have yet to emit until we
                                     # can start emitting, too?

            if $initial {
                @values = @$initial;
                $uninitialised = 0 max $uninitialised - @$initial;
            }

            for @s.kv -> $index, $supply {
                if &with {
                    whenever $supply -> \val {
                        --$uninitialised
                        if $uninitialised > 0 && not @values.EXISTS-POS($index);
                        @values[$index] = val;
                        emit( [[&with]] @values ) unless $uninitialised;
                    }
                }
                else {
                    whenever $supply -> \val {
                        --$uninitialised
                            if $uninitialised > 0 && not @values.EXISTS-POS($index);
                        @values[$index] = val;
                        emit( @values.List.item ) unless $uninitialised;
                    }
                }
            }
        }
    }

    proto method throttle(|) {*}
    multi method throttle(Supply:D:
      Int()  $elems,
      Real() $seconds,
      Real() $delay  = 0,
      :$scheduler    = $*SCHEDULER,
      :$control,
      :$status,
      :$bleed,
      :$vent-at,
    ) {
        my $timer = Supply.interval($seconds,$delay,:$scheduler);
        my int $limit   = $elems;
        my int $vent = $vent-at if $bleed;
        supply {
            my @buffer;
            my int $allowed = $limit;
            my int $emitted;
            my int $bled;
            my int $done;
            sub emit-status($id --> Nil) {
               $status.emit(
                 { :$allowed, :$bled, :buffered(+@buffer),
                   :$emitted, :$id,   :$limit,  :$vent-at } );
            }

            whenever $timer -> \tick {
                if +@buffer -> \buffered {
                    my int $todo = buffered > $limit ?? $limit !! buffered;
                    emit(@buffer.shift) for ^$todo;
                    $emitted = $emitted + $todo;
                    $allowed = $limit   - $todo;
                }
                else {
                    $allowed = $limit;
                }
                if $done && !@buffer {
                    done;
                }
            }

            whenever self -> \val {
                if $allowed {
                    emit(val);
                    $emitted = $emitted + 1;
                    $allowed = $allowed - 1;
                }
                elsif $vent && +@buffer >= $vent {
                    $bleed.emit(val);
                }
                else {
                    @buffer.push(val);
                }
                LAST {
                    if $status {
                        emit-status("done");
                        $status.done;
                    }
                    if $bleed && @buffer {
                        $bleed.emit(@buffer.shift) while @buffer;
                        $bleed.done;
                    }
                    $done = 1;
                }
            }

            if $control {
                whenever $control -> \val {
                   my str $type;
                   my str $value;
                   Rakudo::Internals.KEY_COLON_VALUE(val,$type,$value);

                   if $type eq 'limit' {
                       my int $extra = $value - $limit;
                       $allowed = $extra > 0 || $allowed + $extra >= 0
                         ?? $allowed + $extra
                         !! 0;
                       $limit = $value;
                   }
                   elsif $type eq 'bleed' && $bleed {
                       my int $todo = $value min +@buffer;
                       $bleed.emit(@buffer.shift) for ^$todo;
                       $bled = $bled + $todo;
                   }
                   elsif $type eq 'status' && $status {
                       emit-status($value);
                   }
                   elsif $type eq 'vent-at' && $bleed {
                       $vent = $value;
                       if $vent && +@buffer > $vent {
                           $bleed.emit(@buffer.shift)
                             until !@buffer || +@buffer == $vent;
                       }
                   }
                }
            }
        }
    }
    multi method throttle(Supply:D:
      Int()  $elems,
      Callable:D $process,
      Real() $delay = 0,
      :$scheduler   = $*SCHEDULER,
      :$control,
      :$status,
      :$bleed,
      :$vent-at,
    ) {
        sleep $delay if $delay;
        my @buffer;
        my int $limit   = $elems;
        my int $allowed = $limit;
        my int $running;
        my int $emitted;
        my int $bled;
        my int $done;
        my int $vent = $vent-at if $bleed;
        my $ready = Supplier::Preserving.new;
        sub start-process(\val --> Nil) {
            my $p = Promise.start( $process, :$scheduler, val );
            $running = $running + 1;
            $allowed = $allowed - 1;
            $p.then: { $ready.emit($p) };
        }
        sub emit-status($id --> Nil) {
           $status.emit(
             { :$allowed, :$bled, :buffered(+@buffer),
               :$emitted, :$id,   :$limit, :$running } );
        }
        supply {
            whenever $ready.Supply -> \val { # when a process is ready
                $running = $running - 1;
                $allowed = $allowed + 1;
                emit(val);
                $emitted = $emitted + 1;
                start-process(@buffer.shift) if $allowed > 0 && @buffer;

                if $done && !$running {
                    $control.done if $control;
                    if $status {
                        emit-status("done");
                        $status.done;
                    }
                    if $bleed && @buffer {
                        $bleed.emit(@buffer.shift) while @buffer;
                        $bleed.done;
                    }
                    done;
                }
            }

            if $control {
                whenever $control -> \val {
                    my str $type;
                    my str $value;
                    Rakudo::Internals.KEY_COLON_VALUE(val,$type,$value);

                    if $type eq 'limit' {
                        $allowed = $allowed + $value - $limit;
                        $limit   = $value;
                        start-process(@buffer.shift)
                          while $allowed > 0 && @buffer;
                    }
                    elsif $type eq 'bleed' && $bleed {
                        my int $todo = $value min +@buffer;
                        $bleed.emit(@buffer.shift) for ^$todo;
                        $bled = $bled + $todo;
                    }
                    elsif $type eq 'status' && $status {
                        emit-status($value);
                    }
                    elsif $type eq 'vent-at' && $bleed {
                        $vent = $value;
                        if $vent && +@buffer > $vent {
                            $bleed.emit(@buffer.shift)
                              until !@buffer || +@buffer == $vent;
                        }
                    }
                }
            }

            whenever self -> \val {
                $allowed > 0
                  ?? start-process(val)
                  !! $vent && $vent == +@buffer
                    ?? $bleed.emit(val)
                    !! @buffer.push(val);
                LAST { $done = 1 }
            }
        }
    }

    method share(Supply:D:) {
        my $sup = Supplier.new;
        self.tap:
            -> \msg { $sup.emit(msg) },
            done => -> { $sup.done() },
            quit => -> \ex { $sup.quit(ex) }
        $sup.Supply
    }
}

# vim: ft=perl6 expandtab sw=4
