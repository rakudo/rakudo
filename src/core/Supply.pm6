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
                    my \accepted = try $!test.ACCEPTS(value);
                    if accepted {
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
                    my $t = Tap.new({ self!cleanup($cleaned-up, $source-tap) });
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
        return self unless $time;  # nothing to do
        Supply.new(Delayed.new(source => self.sanitize, :$time, :$scheduler))
    }

    ##
    ## A bunch of the more complex combinators, implemented as supply blocks
    ##

    method do(Supply:D $self: &side-effect) {
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

    method reduce(Supply:D $self: &with) {
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

    method produce(Supply:D $self: &with) {
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

    method unique(Supply:D $self: :&as, :&with, :$expires) {
        supply {
            if $expires {
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
            else { # !$!expires
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
    }

    method squish(Supply:D $self: :&as, :&with is copy) {
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

    multi method rotor(Supply:D $self: Int:D $batch, :$partial) {
        self.rotor(($batch,), :$partial)
    }
    multi method rotor(Supply:D $self: *@cycle, :$partial) {
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

    method batch(Supply:D $self: Int(Cool) :$elems = 0, :$seconds) {
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

    method lines(Supply:D $self: :$chomp = True ) {
        supply {
            my str $str;
            my int $chars;
            my int $left;
            my int $pos;
            my int $nextpos;
            my int $found;

            whenever self -> \val {
                $str   = $str ~ nqp::unbox_s(val);
                $chars = nqp::chars($str);
                $pos   = 0;

                while ($left = $chars - $pos) > 0 {
                    $nextpos = nqp::findcclass(
                      nqp::const::CCLASS_NEWLINE, $str, $pos, $left
                    );

                    last
                      if $nextpos >= $chars     # no line delimiter
                      or $nextpos == $chars - 1 # broken CRLF ?
                        && nqp::eqat($str, "\r", $nextpos); # yes!

                    if $chomp {
                        emit( ($found = $nextpos - $pos)
                          ?? nqp::p6box_s(nqp::substr($str,$pos,$found))
                          !! ''
                        );
                        $pos = $nextpos + 1;
                    }
                    else {
                        $found = $nextpos - $pos + 1;
                        emit(
                          nqp::p6box_s(nqp::substr($str,$pos,$found)));
                        $pos = $pos + $found;
                    }
                }
                $str = $pos < $chars
                  ?? nqp::substr($str,$pos)
                  !! '';

                LAST {
                    if $str {
                        $chars = nqp::chars($str);
                        emit( $chomp && nqp::iscclass(
                          nqp::const::CCLASS_NEWLINE,$str,$chars-1)
                            ?? nqp::p6box_s(nqp::substr($str,0,$chars - 1))
                            !! nqp::p6box_s($str)
                        );
                    }
                }
            }
        }
    }

    method words(Supply:D $self:) {
        supply {
            my str $str;
            my int $chars;
            my int $left;
            my int $pos;
            my int $nextpos;
            my int $found;
            my int $cr;
            my int $crlf;

            whenever self -> \val {
                $str   = $str ~ nqp::unbox_s(val);
                $chars = nqp::chars($str);
                $pos   = nqp::findnotcclass(
                  nqp::const::CCLASS_WHITESPACE, $str, 0, $chars);

                while ($left = $chars - $pos) > 0 {
                    $nextpos = nqp::findcclass(
                      nqp::const::CCLASS_WHITESPACE, $str, $pos, $left
                    );

                    last unless $left = $chars - $nextpos; # broken word

                    emit( nqp::box_s(
                      nqp::substr( $str, $pos, $nextpos - $pos ), Str)
                    );

                    $pos = nqp::findnotcclass(
                      nqp::const::CCLASS_WHITESPACE,$str,$nextpos,$left);
                }
                $str = $pos < $chars
                  ?? nqp::substr($str,$pos)
                  !! '';

                LAST {
                    emit( nqp::box_s($str, Str) ) if $str;
                }
            }
        }
    }

    method elems(Supply:D $self: $seconds? ) {
        supply {
            my int $elems = 0;
            if $seconds {
                my $last_time = time div $seconds;
                my int $last_elems = $elems;
                whenever self -> \val {
                    $last_elems = $elems = $elems + 1;
                    my $this_time = time div $seconds;
                    if $this_time != $last_time {
                        emit $elems;
                        $last_time = $this_time;
                    }
                    LAST emit($elems) if $elems != $last_elems;
                }
            }
            else {
                whenever self -> \val { emit $elems = $elems + 1 }
            }
        }
    }

    method head(Supply:D: Int(Cool) $number = 1) {
        supply {
            my int $todo = $number;
            whenever self -> \val {
                if $todo > 0 {
                    emit val;
                    $todo = $todo - 1;
                }
                done if $todo <= 0;  # nothing left to do
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

    method min(Supply:D $self: &by = &infix:<cmp>) {
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

    method max(Supply:D $self: &by = &infix:<cmp>) {
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

    method minmax(Supply:D $self: &by = &infix:<cmp>) {
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

    method grab(Supply:D $self: &when_done) {
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
            my @values = [] xx +@s;
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
    multi method throttle(Supply:D $self:
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
    multi method throttle(Supply:D $self:
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

# A Supplier is a convenient way to create a live Supply. The publisher can
# be used to emit/done/quit. The Supply objects obtained from it will tap into
# the same live Supply.
my class Supplier {
    my class TapList does Tappable {
        my class TapListEntry {
            has &.emit;
            has &.done;
            has &.quit;
        }

        # Lock serializes updates to tappers.
        has Lock $!lock = Lock.new;

        # An immutable list of tappers. Always replaced on change, never
        # mutated in-place ==> thread safe together with lock (and only
        # need lock on modification).
        has Mu $!tappers;

        method tap(&emit, &done, &quit, &tap) {
            my $tle := TapListEntry.new(:&emit, :&done, :&quit);
            my $t = Tap.new({
                $!lock.protect({
                    my Mu $update := nqp::list();
                    for nqp::hllize($!tappers) -> \entry {
                        nqp::push($update, entry) unless entry =:= $tle;
                    }
                    $!tappers := $update;
                });
            });
            tap($t);
            $!lock.protect({
                my Mu $update := nqp::isconcrete($!tappers)
                    ?? nqp::clone($!tappers)
                    !! nqp::list();
                nqp::push($update, $tle);
                $!tappers := $update;
            });
            $t
        }

        method emit(\value --> Nil) {
            nqp::if(
              nqp::isconcrete(my $snapshot := $!tappers)
                && (my int $n = nqp::elems($snapshot)),
              nqp::if(                                 # at least one tap
                nqp::isgt_i($n,1),
                nqp::stmts(                            # multiple taps
                  (my int $i = -1),
                  nqp::while(
                    nqp::islt_i(($i = nqp::add_i($i,1)),$n),
                    nqp::atpos($snapshot,$i).emit()(value)
                  )
                ),
                nqp::atpos($snapshot,0).emit()(value)  # only one tap
              )
            )
        }

        method done(--> Nil) {
            my $snapshot := $!tappers;
            if nqp::isconcrete($snapshot) {
                my int $n = nqp::elems($snapshot);
                loop (my int $i = 0; $i < $n; $i = $i + 1) {
                    nqp::atpos($snapshot, $i).done()();
                }
            }
        }

        method quit($ex --> Nil) {
            my $snapshot := $!tappers;
            if nqp::isconcrete($snapshot) {
                my int $n = nqp::elems($snapshot);
                loop (my int $i = 0; $i < $n; $i = $i + 1) {
                    nqp::atpos($snapshot, $i).quit()($ex);
                }
            }
        }

        method live(--> True) { }
        method serial(--> False) { }
        method sane(--> False)  { }
    }

    has $!taplist;

    method new() {
        self.bless(taplist => TapList.new)
    }
    submethod BUILD(:$!taplist! --> Nil) { }

    method emit(Supplier:D: Mu \value --> Nil) {
        $!taplist.emit(value);
    }

    method done(Supplier:D: --> Nil) {
        $!taplist.done();
    }

    proto method quit($) {*}
    multi method quit(Supplier:D: Exception $ex) {
        $!taplist.quit($ex);
    }
    multi method quit(Supplier:D: Str() $message) {
        $!taplist.quit(X::AdHoc.new(payload => $message));
    }

    method Supply(Supplier:D:) {
        Supply.new($!taplist).sanitize
    }

    method unsanitized-supply(Supplier:D:) {
        Supply.new($!taplist)
    }
}

# A preserving supplier holds on to emitted values and state when nobody is
# tapping. As soon as there a tap is made, any preserved events will be
# immediately sent to that tapper.
my class Supplier::Preserving is Supplier {
    my class PreservingTapList does Tappable {
        my class TapListEntry {
            has &.emit;
            has &.done;
            has &.quit;
        }

        # Lock serializes updates to tappers.
        has Lock $!lock = Lock.new;

        # An immutable list of tappers. Always replaced on change, never
        # mutated in-place ==> thread safe together with lock (and only
        # need lock on modification).
        has Mu $!tappers;

        # Events to reply, whether the replay was done, and a lock to protect
        # updates to these.
        has @!replay;
        has int $!replay-done;
        has $!replay-lock = Lock.new;

        method tap(&emit, &done, &quit, &tap) {
            my $tle := TapListEntry.new(:&emit, :&done, :&quit);
            my int $replay = 0;
            my $t = Tap.new({
                $!lock.protect({
                    my Mu $update := nqp::list();
                    for nqp::hllize($!tappers) -> \entry {
                        nqp::push($update, entry) unless entry =:= $tle;
                    }
                    $!replay-done = 0 if nqp::elems($update) == 0;
                    $!tappers := $update;
                });
            });
            tap($t);
            $!lock.protect({
                my Mu $update := nqp::isconcrete($!tappers)
                    ?? nqp::clone($!tappers)
                    !! nqp::list();
                nqp::push($update, $tle);
                $replay = 1 if nqp::elems($update) == 1;
                self!replay($tle) if $replay;
                $!tappers := $update;
            });
            $t
        }

        method emit(\value --> Nil) {
            loop {
                my int $sent = 0;
                my $snapshot := $!tappers;
                if nqp::isconcrete($snapshot) {
                    $sent = nqp::elems($snapshot);
                    loop (my int $i = 0; $i < $sent; $i = $i + 1) {
                        nqp::atpos($snapshot, $i).emit()(value);
                    }
                }
                return if $sent;
                return if self!add-replay({ $_.emit()(value) });
            }
        }

        method done(--> Nil) {
            loop {
                my int $sent = 0;
                my $snapshot := $!tappers;
                if nqp::isconcrete($snapshot) {
                    $sent = nqp::elems($snapshot);
                    loop (my int $i = 0; $i < $sent; $i = $i + 1) {
                        nqp::atpos($snapshot, $i).done()();
                    }
                }
                return if $sent;
                return if self!add-replay({ $_.done()() });
            }
        }

        method quit($ex --> Nil) {
            loop {
                my int $sent = 0;
                my $snapshot := $!tappers;
                if nqp::isconcrete($snapshot) {
                    $sent = nqp::elems($snapshot);
                    loop (my int $i = 0; $i < $sent; $i = $i + 1) {
                        nqp::atpos($snapshot, $i).quit()($ex);
                    }
                }
                return if $sent;
                return if self!add-replay({ $_.quit()($ex) });
            }
        }

        method !add-replay(&replay --> Bool) {
            $!replay-lock.protect: {
                if $!replay-done {
                    False
                }
                else {
                    @!replay.push(&replay);
                    True
                }
            }
        }

        method !replay($tle) {
            $!replay-lock.protect: {
                while @!replay.shift -> $rep {
                    $rep($tle);
                }
                $!replay-done = 1;
            }
        }

        method live(--> True) { }
        method serial(--> False) { }
        method sane(--> False) { }
    }

    method new() {
        self.bless(taplist => PreservingTapList.new)
    }
}

augment class Rakudo::Internals {
    my constant ADD_WHENEVER_PROMPT = Mu.new;

    class CachedAwaitHandle does Awaitable {
        has $.get-await-handle;
    }

    class SupplyBlockAddWheneverAwaiter does Awaiter {
        has $!continuations;

        method await(Awaitable:D $a) {
            my $handle := $a.get-await-handle;
            if $handle.already {
                $handle.success
                    ?? $handle.result
                    !! $handle.cause.rethrow
            }
            else {
                my $reawaitable := CachedAwaitHandle.new(get-await-handle => $handle);
                $!continuations := nqp::list() unless nqp::isconcrete($!continuations);
                nqp::continuationcontrol(0, ADD_WHENEVER_PROMPT, -> Mu \c {
                    nqp::push($!continuations, -> $delegate-awaiter {
                        nqp::continuationinvoke(c, {
                            $delegate-awaiter.await($reawaitable);
                        });
                    });
                });
            }
        }

        method await-all(Iterable:D \i) {
            $!continuations := nqp::list() unless nqp::isconcrete($!continuations);
            nqp::continuationcontrol(0, ADD_WHENEVER_PROMPT, -> Mu \c {
                nqp::push($!continuations, -> $delegate-awaiter {
                    nqp::continuationinvoke(c, {
                        $delegate-awaiter.await-all(i);
                    });
                });
            });
        }

        method take-all() {
            if nqp::isconcrete($!continuations) {
                my \result = $!continuations;
                $!continuations := Mu;
                result
            }
            else {
                Empty
            }
        }
    }

    class SupplyBlockState {
        has &.emit;
        has &.done;
        has &.quit;
        has @.close-phasers;
        has $.active;
        has $!lock;
        has %!active-taps;
        has $.run-async-lock;
        has $.awaiter;

        method new(:&emit!, :&done!, :&quit!) {
            self.CREATE!SET-SELF(&emit, &done, &quit)
        }

        method !SET-SELF(&emit, &done, &quit) {
            &!emit := &emit;
            &!done := &done;
            &!quit := &quit;
            $!active = 1;
            $!lock := Lock.new;
            $!run-async-lock := Lock::Async.new;
            $!awaiter := SupplyBlockAddWheneverAwaiter.CREATE;
            self
        }

        method decrement-active() {
            $!lock.protect: { --$!active }
        }

        method get-and-zero-active() {
            $!lock.protect: {
                my $result = $!active;
                $!active = 0;
                $result
            }
        }

        method add-active-tap($tap --> Nil) {
            $!lock.protect: {
                ++$!active;
                %!active-taps{nqp::objectid($tap)} = $tap;
            }
        }

        method delete-active-tap($tap --> Nil) {
            $!lock.protect: {
                %!active-taps{nqp::objectid($tap)}:delete;
            }
        }

        method teardown(--> Nil) {
            my $to-close := nqp::create(IterationBuffer);
            $!lock.protect: {
                %!active-taps.values.iterator.push-all($to-close);
                %!active-taps = ();
                $!active = 0;
            }
            my int $n = nqp::elems($to-close);
            loop (my int $i = 0; $i < $n; $i++) {
                nqp::atpos($to-close, $i).close();
            }
            my @close-phasers := @!close-phasers;
            while @close-phasers {
                @close-phasers.pop()();
            }
        }

        method run-emit(--> Nil) {
            if $!active {
                my \ex := nqp::exception();
                my $emit-handler := &!emit;
                $emit-handler(nqp::getpayload(ex)) if $emit-handler.DEFINITE;
                nqp::resume(ex)
            }
        }

        method run-done(--> Nil) {
            self.get-and-zero-active();
            self.teardown();
            my $done-handler := &!done;
            $done-handler() if $done-handler.DEFINITE;
        }

        method run-catch(--> Nil) {
            my \ex = EXCEPTION(nqp::exception());
            self.get-and-zero-active();
            self.teardown();
            my $quit-handler = &!quit;
            $quit-handler(ex) if $quit-handler;
        }
    }

    class SupplyBlockTappable does Tappable {
        has &!block;

        submethod BUILD(:&!block --> Nil) { }

        method tap(&emit, &done, &quit, &tap) {
            # Create state for this tapping.
            my $state := Rakudo::Internals::SupplyBlockState.new(:&emit, :&done, :&quit);

            # Placed here so it can close over $state, but we only need to
            # closure-clone it once per Supply block, not once per whenever.
            sub add-whenever($supply, &whenever-block) {
                my $tap;
                $state.run-async-lock.with-lock-hidden-from-recursion-check: {
                    my $*AWAITER := $state.awaiter;
                    nqp::continuationreset(ADD_WHENEVER_PROMPT, {
                        $supply.tap(
                            tap => {
                                $tap := $_;
                                $state.add-active-tap($tap);
                            },
                            -> \value {
                                self!run-supply-code(&whenever-block, value, $state,
                                    &add-whenever)
                            },
                            done => {
                                $state.delete-active-tap($tap);
                                my @phasers := &whenever-block.phasers('LAST');
                                if @phasers {
                                    self!run-supply-code({ .() for @phasers }, Nil, $state,
                                        &add-whenever)
                                }
                                $tap.close;
                                self!deactivate-one($state);
                            },
                            quit => -> \ex {
                                $state.delete-active-tap($tap);
                                my $handled := False;
                                self!run-supply-code({
                                    my $phaser := &whenever-block.phasers('QUIT')[0];
                                    if $phaser.DEFINITE {
                                        $handled := $phaser(ex) === Nil;
                                    }
                                    if !$handled && $state.get-and-zero-active() {
                                        $state.quit().(ex) if $state.quit;
                                        $state.teardown();
                                    }
                                }, Nil, $state, &add-whenever);
                                if $handled {
                                    $tap.close;
                                    self!deactivate-one($state);
                                }
                            });
                    });
                }
                $tap
            }

            # Stash away any CLOSE phasers.
            if nqp::istype(&!block, Block) {
                $state.close-phasers.append(&!block.phasers('CLOSE'));
            }

            # Create and pass on tap; when closed, tear down the state and all
            # of our subscriptions.
            my $t := Tap.new(-> { $state.teardown() });
            tap($t);

            # Run the Supply block, then decrease active count afterwards (it
            # counts as an active runner).
            self!run-supply-code:
                { &!block(); self!deactivate-one-internal($state) },
                Nil, $state, &add-whenever;

            # Evaluate to the Tap.
            $t
        }

        method !run-supply-code(&code, \value, SupplyBlockState $state, &add-whenever) {
            my @run-after;
            my $queued := $state.run-async-lock.protect-or-queue-on-recursion: {
                my &*ADD-WHENEVER := &add-whenever;
                $state.active > 0 and nqp::handle(code(value),
                    'EMIT', $state.run-emit(),
                    'DONE', $state.run-done(),
                    'CATCH', $state.run-catch(),
                    'NEXT', 0);
                @run-after = $state.awaiter.take-all;
            }
            if $queued.defined {
                $queued.then({ self!run-add-whenever-awaits(@run-after) });
            }
            else {
                self!run-add-whenever-awaits(@run-after);
            }
        }

        method !run-add-whenever-awaits(@run-after --> Nil) {
            if @run-after {
                my $nested-awaiter := SupplyBlockAddWheneverAwaiter.CREATE;
                my $delegate-awaiter := $*AWAITER;
                while @run-after.elems {
                    my $*AWAITER := $nested-awaiter;
                    nqp::continuationreset(ADD_WHENEVER_PROMPT, {
                        @run-after.shift()($delegate-awaiter);
                    });
                    @run-after.append($nested-awaiter.take-all);
                }
            }
        }

        method !deactivate-one(SupplyBlockState $state) {
            $state.run-async-lock.protect-or-queue-on-recursion:
                { self!deactivate-one-internal($state) };
        }

        method !deactivate-one-internal(SupplyBlockState $state) {
            if $state.decrement-active() == 0 {
                my $done-handler := $state.done;
                $done-handler() if $done-handler;
                $state.teardown();
            }
        }

        method live(--> False) { }
        method sane(--> True) { }
        method serial(--> True) { }
    }

    class SupplyOneWheneverState {
        has &.emit;
        has &.done;
        has &.quit;
        has @.close-phasers;
        has $.tap is rw;
        has $.active;

        method new(:&emit!, :&done!, :&quit!) {
            self.CREATE!SET-SELF(&emit, &done, &quit)
        }

        method !SET-SELF(&emit, &done, &quit) {
            &!emit := &emit;
            &!done := &done;
            &!quit := &quit;
            $!active = 1;
            self
        }

        method teardown(--> Nil) {
            $!active = 0;
            $!tap.close if $!tap;
            my @close-phasers := @!close-phasers;
            while @close-phasers {
                @close-phasers.pop()();
            }
        }

        method run-emit(--> Nil) {
            if $!active {
                my \ex := nqp::exception();
                my $emit-handler := &!emit;
                $emit-handler(nqp::getpayload(ex)) if $emit-handler.DEFINITE;
                nqp::resume(ex)
            }
        }

        method run-done(--> Nil) {
            if $!active {
                self.teardown();
                my $done-handler := &!done;
                $done-handler() if $done-handler.DEFINITE;
            }
        }

        method run-catch(--> Nil) {
            if $!active {
                my \ex = EXCEPTION(nqp::exception());
                self.teardown();
                my $quit-handler = &!quit;
                $quit-handler(ex) if $quit-handler;
            }
        }
    }

    class SupplyOneWheneverTappable does Tappable {
        has &!block;

        submethod BUILD(:&!block --> Nil) { }

        method tap(&emit, &done, &quit, &tap) {
            # Create state for this tapping.
            my $state := Rakudo::Internals::SupplyOneWheneverState.new(:&emit, :&done, :&quit);

            # We only expcet one whenever; detect getting a second and complain.
            my $*WHENEVER-SUPPLY-TO-ADD := Nil;
            my &*WHENEVER-BLOCK-TO-ADD := Nil;
            sub add-whenever(\the-supply, \the-whenever-block) {
                if $*WHENEVER-SUPPLY-TO-ADD =:= Nil {
                    $*WHENEVER-SUPPLY-TO-ADD := the-supply;
                    &*WHENEVER-BLOCK-TO-ADD := the-whenever-block;
                }
                else {
                    die "Single whenever block special case tried to add second whenever";
                }
            }

            # Stash away any CLOSE phasers.
            if nqp::istype(&!block, Block) {
                $state.close-phasers.append(&!block.phasers('CLOSE'));
            }

            # Create and pass on tap; when closed, tear down the state and all
            # of our subscriptions.
            my $t := Tap.new(-> { $state.teardown() });
            tap($t);

            # Run the Supply block. Only proceed if it didn't send done/quit.
            self!run-supply-code: { &!block() }, Nil, $state, &add-whenever;
            if $state.active {
                # If we didn't get a whenever, something is badly wrong.
                if $*WHENEVER-SUPPLY-TO-ADD =:= Nil {
                    die "Single whenever block special case did not get a whenever block";
                }

                # Otherwise, we can now tap that whenever block. Since it is the
                # only one, and we know from compile-time analysis it is the last
                # thing in the block, then it's safe to do it now the block is
                # completed and without any concurrency control. However, we do
                # call .sanitize just in case, to ensure that we have a serial and
                # protocol-following Supply. That is enough.
                my $supply := $*WHENEVER-SUPPLY-TO-ADD.sanitize;
                my &whenever-block := &*WHENEVER-BLOCK-TO-ADD;
                my $tap;
                $supply.tap(
                    tap => {
                        $tap := $_;
                        $state.tap = $tap;
                    },
                    -> \value {
                        self!run-supply-code(&whenever-block, value, $state,
                            &add-whenever)
                    },
                    done => {
                        my @phasers := &whenever-block.phasers('LAST');
                        if @phasers {
                            self!run-supply-code({ .() for @phasers }, Nil, $state,
                                &add-whenever)
                        }
                        $tap.close;
                        $state.run-done();
                    },
                    quit => -> \ex {
                        my $handled := False;
                        self!run-supply-code({
                            my $phaser := &whenever-block.phasers('QUIT')[0];
                            if $phaser.DEFINITE {
                                $handled := $phaser(ex) === Nil;
                            }
                            if !$handled {
                                $state.quit().(ex) if $state.quit;
                                $state.teardown();
                            }
                        }, Nil, $state, &add-whenever);
                        if $handled {
                            $tap.close;
                            $state.run-done();
                        }
                    });
            }

            # Evaluate to the Tap.
            $t
        }

        method !run-supply-code(&code, \value, SupplyOneWheneverState $state, &add-whenever) {
            my &*ADD-WHENEVER := &add-whenever;
            {
                $state.active > 0 and nqp::handle(code(value),
                    'EMIT', $state.run-emit(),
                    'DONE', $state.run-done(),
                    'CATCH', $state.run-catch(),
                    'NEXT', 0);
            }(); # XXX Workaround for optimizer bug
        }

        method live(--> False) { }
        method sane(--> True) { }
        method serial(--> True) { }
    }

    class OneEmitTappable does Tappable {
        has &!block;

        submethod BUILD(:&!block! --> Nil) {}

        method tap(&emit, &done, &quit, &tap) {
            my $t := Tap.new;
            tap($t);
            try {
                emit(&!block());
                done();
                CATCH {
                    default {
                        quit($_);
                    }
                }
            }
            $t
        }

        method live(--> False) { }
        method sane(--> True) { }
        method serial(--> True) { }
    }
}

sub SUPPLY(&block) {
    Supply.new(Rakudo::Internals::SupplyBlockTappable.new(:&block))
}

sub WHENEVER(Supply() $supply, &block) {
    my \adder = nqp::getlexdyn('&*ADD-WHENEVER');
    nqp::isnull(adder)
        ?? X::WheneverOutOfScope.new.throw
        !! adder.($supply, &block)
}

sub REACT(&block) {
    my $s := SUPPLY(&block);
    my $p := Promise.new;
    $s.tap(
        { warn "Useless use of emit in react" },
        done => { $p.keep(Nil) },
        quit => { $p.break($_) });
    await $p;
}

sub SUPPLY-ONE-EMIT(&block) {
    Supply.new(Rakudo::Internals::OneEmitTappable.new(:&block))
}

sub SUPPLY-ONE-WHENEVER(&block) {
    Supply.new(Rakudo::Internals::SupplyOneWheneverTappable.new(:&block))
}

sub REACT-ONE-WHENEVER(&block) {
    my $s := SUPPLY-ONE-WHENEVER(&block);
    my $p := Promise.new;
    $s.tap(
        { warn "Useless use of emit in react" },
        done => { $p.keep(Nil) },
        quit => { $p.break($_) });
    await $p;
}

# vim: ft=perl6 expandtab sw=4
