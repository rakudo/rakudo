# When we tap a Supply, we get back a Tap object. We close the tap in order
# to turn off the flow of values.
my class Tap {
    has &!on-close;

    submethod BUILD(:&!on-close --> Nil) { }

    method new(&on-close) {
        self.bless(:&on-close)
    }

    method close() {
        if &!on-close {
            my \close-result = &!on-close();
            await close-result if nqp::istype(close-result, Promise);
        }
        True
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
    method tap() { ... }
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

    proto method new(|) { * }
    multi method new() {
        X::Supply::New.new.throw
    }
    multi method new(Tappable $tappable) {
        self.bless(:$tappable);
    }
    submethod BUILD(:$!tappable! --> Nil) { }

    method live(Supply:D:) { $!tappable.live }
    method serial(Supply:D:) { $!tappable.serial }

    my \DISCARD = -> $ {};
    my \NOP = -> {};
    my \DEATH = -> $ex { $ex.throw };
    method tap(Supply:D: &emit = DISCARD, :&done = NOP, :&quit = DEATH) {
        $!tappable.tap(&emit, &done, &quit)
    }

    method act(Supply:D: &actor, *%others) {
        self.sanitize.tap(&actor, |%others)
    }

    ##
    ## Supply factories
    ##

    method on-demand(Supply:U: &producer, :&closing, :$scheduler = CurrentThreadScheduler) {
        Supply.new(class :: does Tappable {
            has &!producer;
            has &!closing;
            has $!scheduler;

            submethod BUILD(:&!producer!, :&!closing!, :$!scheduler! --> Nil) {}

            method tap(&emit, &done, &quit) {
                my $p = Supplier.new;
                $p.Supply.tap(&emit, :&done, :&quit); # sanitizes
                $!scheduler.cue({ &!producer($p) },
                    catch => -> \ex { $p.quit(ex) });
                Tap.new(&!closing)
            }
            
            method live() { False }
            method sane() { True }
            method serial() { True }
        }.new(:&producer, :&closing, :$scheduler))
    }

    method from-list(Supply:U: +@values, :$scheduler = CurrentThreadScheduler) {
        self.on-demand(-> $p {
            $p.emit($_) for @values;
            $p.done();
        }, :$scheduler);
    }

    method interval(Supply:U: $interval, $delay = 0, :$scheduler = $*SCHEDULER) {
        Supply.new(class :: does Tappable {
            has $!scheduler;
            has $!interval;
            has $!delay;

            submethod BUILD(:$!scheduler, :$!interval, :$!delay --> Nil) { }

            method tap(&emit, |) {
                my $i = 0;
                my $lock = Lock.new;
                my $cancellation = $!scheduler.cue(
                    {
                        emit($lock.protect: { $i++ });
                        CATCH { $cancellation.cancel if $cancellation }
                    },
                    :every($!interval), :in($!delay)
                );
                Tap.new({ $cancellation.cancel })
            }

            method live { False }
            method sane { True }
            method serial { False }
        }.new(:$interval, :$delay, :$scheduler));
    }

    ##
    ## Simple operations are those that operate on a single Supply, carry its
    ## liveness, and are always serial. We implement the directly as they are
    ## common and fairly "hot path".
    ##

    my role SimpleOpTappable does Tappable {
        has $!source;
        method live() { $!source.live }
        method sane() { True }
        method serial() { True }
        method !cleanup(int $cleaned-up is rw, $source-tap) {
            if $source-tap && !$cleaned-up  {
                $cleaned-up = 1;
                $source-tap.close;
            }
        }
    }

    method serialize(Supply:D:) {
        $!tappable.serial ?? self !! Supply.new(class :: does SimpleOpTappable {
            has $!lock = Lock.new;

            submethod BUILD(:$!source! --> Nil) { }

            method tap(&emit, &done, &quit) {
                my int $cleaned-up = 0;
                my $source-tap = $!source.tap(
                    -> \value{
                        $!lock.protect: { emit(value); }
                    },
                    done => -> {
                        $!lock.protect: {
                            done();
                            self!cleanup($cleaned-up, $source-tap);
                        }
                    },
                    quit => -> $ex {
                        $!lock.protect: {
                            quit($ex);
                            self!cleanup($cleaned-up, $source-tap);
                        }
                    });
                Tap.new({ self!cleanup($cleaned-up, $source-tap) })
            }
        }.new(source => self))
    }    

    method sanitize() {
        $!tappable.sane ?? self !! Supply.new(class :: does SimpleOpTappable {
            has int $!finished;

            submethod BUILD(:$!source! --> Nil) { }

            method tap(&emit, &done, &quit) {
                my int $cleaned-up = 0;
                my $source-tap = $!source.tap(
                    -> \value{
                        emit(value) unless $!finished;
                    },
                    done => -> {
                        unless $!finished {
                            $!finished = 1;
                            done();
                            self!cleanup($cleaned-up, $source-tap);
                        }
                    },
                    quit => -> $ex {
                        unless $!finished {
                            $!finished = 1;
                            quit($ex);
                            self!cleanup($cleaned-up, $source-tap);
                        }
                    });
                Tap.new({ self!cleanup($cleaned-up, $source-tap) })
            }
        }.new(source => self.serialize))
    }

    method on-close(Supply:D: &on-close) {
        return Supply.new(class :: does SimpleOpTappable {
            has int $!finished;
            has &!on-close;

            submethod BUILD(:$!source!, :&!on-close! --> Nil) { }

            method tap(&emit, &done, &quit) {
                my int $cleaned-up = 0;
                my $source-tap = $!source.tap(&emit, :&done, :&quit);
                Tap.new({
                    &!on-close();
                    self!cleanup($cleaned-up, $source-tap)
                })
            }
        }.new(source => self, :&on-close))
    }

    method map(Supply:D: &mapper) {
        Supply.new(class :: does SimpleOpTappable {
            has &!mapper;

            submethod BUILD(:$!source!, :&!mapper! --> Nil) { }

            method tap(&emit, &done, &quit) {
                my int $cleaned-up = 0;
                my $source-tap = $!source.tap(
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
                Tap.new({ self!cleanup($cleaned-up, $source-tap) })
            }
        }.new(source => self.sanitize, :&mapper))
    }

    method grep(Supply:D: Mu $test) {
        Supply.new(class :: does SimpleOpTappable {
            has Mu $!test;

            submethod BUILD(:$!source!, Mu :$!test! --> Nil) { }

            method tap(&emit, &done, &quit) {
                my int $cleaned-up = 0;
                my $source-tap = $!source.tap(
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
                Tap.new({ self!cleanup($cleaned-up, $source-tap) })
            }
        }.new(source => self.sanitize, :$test))
    }

    method schedule-on(Supply:D: Scheduler $scheduler) {
        Supply.new(class :: does SimpleOpTappable {
            has $!scheduler;

            submethod BUILD(:$!source!, :$!scheduler! --> Nil) { }

            method tap(&emit, &done, &quit) {
                my int $cleaned-up = 0;
                my $source-tap = $!source.tap(
                    -> \value {
                        $!scheduler.cue: { emit(value) }
                    },
                    done => -> {
                        $!scheduler.cue: { done(); self!cleanup($cleaned-up, $source-tap); }
                    },
                    quit => -> $ex {
                        $!scheduler.cue: { quit($ex); self!cleanup($cleaned-up, $source-tap); }
                    });
                Tap.new({ self!cleanup($cleaned-up, $source-tap) })
            }
        }.new(source => self.sanitize, :$scheduler))
    }

    method start(Supply:D: &startee) {
        self.map: -> \value {
            Supply.new(class :: does SimpleOpTappable {
                has $!value;
                has &!startee;

                submethod BUILD(:$!value, :&!startee --> Nil) { }

                method tap(&emit, &done, &quit) {
                    my int $closed = 0;
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
                    Tap.new({ $closed = 1 })
                }
            }.new(:value(value), :&startee))
        }
    }

    method stable(Supply:D: $time, :$scheduler = $*SCHEDULER) {
        return self unless $time;
        Supply.new(class :: does SimpleOpTappable {
            has $!time;
            has $!scheduler;
            has $!last_cancellation;
            has $!lock = Lock.new;

            submethod BUILD(:$!source!, :$!time!, :$!scheduler! --> Nil) { }

            method tap(&emit, &done, &quit) {
                my int $cleaned-up = 0;
                my $source-tap = $!source.tap(
                    -> \value {
                        $!lock.protect: {
                            if $!last_cancellation {
                                $!last_cancellation.cancel;
                            }
                            $!last_cancellation = $!scheduler.cue(
                                :in($time),
                                {
                                    $!lock.protect: { $!last_cancellation = Nil; }
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
                Tap.new({ self!cleanup($cleaned-up, $source-tap) })
            }
        }.new(source => self.sanitize, :$time, :$scheduler))
    }

    method delayed(Supply:D: $time, :$scheduler = $*SCHEDULER) {
        return self unless $time;  # nothing to do
        Supply.new(class :: does SimpleOpTappable {
            has $!time;
            has $!scheduler;

            submethod BUILD(:$!source!, :$!time, :$!scheduler! --> Nil) { }

            method tap(&emit, &done, &quit) {
                my int $cleaned-up = 0;
                my $source-tap = $!source.tap(
                    -> \value {
                        $!scheduler.cue: { emit(value) }, :in($time)
                    },
                    done => -> {
                        $!scheduler.cue:
                            { done(); self!cleanup($cleaned-up, $source-tap); },
                            :in($time)
                    },
                    quit => -> $ex {
                        $!scheduler.cue:
                            { quit($ex); self!cleanup($cleaned-up, $source-tap); },
                            :in($time)
                    });
                Tap.new({ self!cleanup($cleaned-up, $source-tap) })
            }
        }.new(source => self.sanitize, :$time, :$scheduler))
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

    proto method classify(|) { * }
    multi method classify(Supply:D: &mapper )  {
        self!classify(&mapper);
    }
    multi method classify(Supply:D: %mapper )  {
        self!classify({ %mapper{$^a} });
    }
    multi method classify(Supply:D: @mapper )  {
        self!classify({ @mapper[$^a] });
    }

    proto method categorize (|) { * }
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

    method Supply(Supply:) { self }

    method Channel(Supply:D:) {
        my $c = Channel.new();
        self.sanitize.tap:
            -> \val { $c.send(val) },
            done => { $c.close },
            quit => -> $ex { $c.fail($ex) };
        $c
    }

    my class ConcQueue is repr('ConcBlockingQueue') { }
    method list(Supply:D:) {
        gather {
            my Mu \queue = ConcQueue.CREATE;
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
            self.CREATE!not-ready(supply)
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

    method batch(Supply:D $self: :$elems, :$seconds ) {
        return self if (!$elems or $elems == 1) and !$seconds;  # nothing to do
        supply {
            my @batched;
            my $last_time;
            sub flush(--> Nil) {
                emit([@batched]);
                @batched = ();
            }
            sub final-flush(--> Nil) {
                flush if @batched;
            }

            if $seconds {
                $last_time = time div $seconds;

                if $elems { # and $seconds
                    whenever self -> \val {
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
                        LAST { final-flush; }
                    }
                }
                else {
                    whenever self -> \val {
                        my $this_time = time div $seconds;
                        if $this_time != $last_time {
                            flush if @batched;
                            $last_time = $this_time;
                        }
                        @batched.push: val;
                        LAST { final-flush; }
                    }
                }
            }
            else { # just $elems
                whenever self -> \val {
                    @batched.push: val;
                    flush if @batched.elems == $elems;
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
                    }
                }
                else {
                    whenever $supply -> \val {
                        @values[$index].push(val);
                        emit( $(@values.map(*.shift).list) ) if all(@values);
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

    proto method throttle(|) { * }
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
        my int $vent = $vent-at if $bleed;;
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
                    $done = 0;
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

        method tap(&emit, &done, &quit) {
            my $tle := TapListEntry.new(:&emit, :&done, :&quit);
            $!lock.protect({
                my Mu $update := nqp::isconcrete($!tappers)
                    ?? nqp::clone($!tappers)
                    !! nqp::list();
                nqp::push($update, $tle);
                $!tappers := $update;
            });
            Tap.new({
                $!lock.protect({
                    my Mu $update := nqp::list();
                    for nqp::hllize($!tappers) -> \entry {
                        nqp::push($update, entry) unless entry =:= $tle;
                    }
                    $!tappers := $update;
                });
            })
        }

        method emit(\value --> Nil) {
            my $snapshot := $!tappers;
            if nqp::isconcrete($snapshot) {
                my int $n = nqp::elems($snapshot);
                loop (my int $i = 0; $i < $n; $i = $i + 1) {
                    nqp::atpos($snapshot, $i).emit()(value);
                }
            }
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

        method live     { True  }
        method serial() { False }
        method sane()   { False }
    }

    has $!taplist;

    method new() {
        self.bless(taplist => TapList.new)
    }
    submethod BUILD(:$!taplist! --> Nil) { }

    method emit(Supplier:D: Mu \value) {
        $!taplist.emit(value);
    }

    method done(Supplier:D:) {
        $!taplist.done();
    }

    proto method quit($) { * }
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

        # Events to reply, and a lock to protect it.
        has @!replay;
        has $!replay-lock = Lock.new;

        method tap(&emit, &done, &quit) {
            my $tle := TapListEntry.new(:&emit, :&done, :&quit);
            my int $replay = 0;
            $!lock.protect({
                my Mu $update := nqp::isconcrete($!tappers)
                    ?? nqp::clone($!tappers)
                    !! nqp::list();
                nqp::push($update, $tle);
                $!tappers := $update;
                $replay = 1 if nqp::elems($update) == 1;
            });
            self!replay($tle) if $replay;
            Tap.new({
                $!lock.protect({
                    my Mu $update := nqp::list();
                    for nqp::hllize($!tappers) -> \entry {
                        nqp::push($update, entry) unless entry =:= $tle;
                    }
                    $!tappers := $update;
                });
            })
        }

        method emit(\value --> Nil) {
            my int $sent = 0;
            my $snapshot := $!tappers;
            if nqp::isconcrete($snapshot) {
                my int $n = nqp::elems($snapshot);
                loop (my int $i = 0; $i < $n; $i = $i + 1) {
                    nqp::atpos($snapshot, $i).emit()(value);
                    $sent = 1;
                }
            }
            unless $sent {
                self!add-replay({ $_.emit()(value) })
            }
        }

        method done(--> Nil) {
            my int $sent = 0;
            my $snapshot := $!tappers;
            if nqp::isconcrete($snapshot) {
                my int $n = nqp::elems($snapshot);
                loop (my int $i = 0; $i < $n; $i = $i + 1) {
                    nqp::atpos($snapshot, $i).done()();
                    $sent = 1;
                }
            }
            unless $sent {
                self!add-replay({ $_.done()() })
            }
        }

        method quit($ex --> Nil) {
            my int $sent = 0;
            my $snapshot := $!tappers;
            if nqp::isconcrete($snapshot) {
                my int $n = nqp::elems($snapshot);
                loop (my int $i = 0; $i < $n; $i = $i + 1) {
                    nqp::atpos($snapshot, $i).quit()($ex);
                    $sent = 1;
                }
            }
            unless $sent {
                self!add-replay({ $_.quit()($ex) })
            }
        }

        method !add-replay(&replay) {
            $!replay-lock.protect: { @!replay.push(&replay) }
        }

        method !replay($tle) {
            while $!replay-lock.protect({ @!replay.shift }) -> $rep {
                $rep($tle)
            }
        }

        method live     { True  }
        method serial() { False }
        method sane()   { False }
    }

    method new() {
        self.bless(taplist => PreservingTapList.new)
    }
}

sub SUPPLY(&block) {
    my class SupplyBlockState {
        has &.emit;
        has &.done;
        has &.quit;
        has @.close-phasers;
        has $!active = 1;
        has $!lock = Lock.new;
        has %!active-taps;
        has @!queued-operations;

        method increment-active() {
            $!lock.protect: { ++$!active }
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
            $!lock.protect: { %!active-taps{nqp::objectid($tap)} = $tap }
        }

        method delete-active-tap($tap --> Nil) {
            $!lock.protect: { %!active-taps{nqp::objectid($tap)}:delete }
        }

        method consume-active-taps() {
            my @active;
            $!lock.protect: {
                @active = %!active-taps.values;
                %!active-taps = ();
            }
            @active
        }

        method run-operation(&op --> Nil) {
            if $!active {
                my $run-now = False;
                $!lock.protect({
                    if @!queued-operations {
                        @!queued-operations.push({
                            op();
                            self!maybe-another();
                        });
                    }
                    else {
                        @!queued-operations.push(&op);
                        $run-now = True;
                    }
                });
                if $run-now {
                    op();
                    self!maybe-another();
                }
            }
        }

        method !maybe-another(--> Nil) {
            my &another;
            $!lock.protect({
                @!queued-operations.shift;
                &another = @!queued-operations[0] if $!active && @!queued-operations;
            });
            &another && another();
        }
    }

    Supply.new(class :: does Tappable {
        has &!block;

        submethod BUILD(:&!block --> Nil) { }

        method tap(&emit, &done, &quit) {
            my $state = SupplyBlockState.new(:&emit, :&done, :&quit);
            self!run-supply-code(&!block, $state);
            if nqp::istype(&!block,Block) {
                $state.close-phasers.push(.clone) for &!block.phasers('CLOSE')
            }
            self!deactivate-one($state);
            Tap.new(-> { self!teardown($state) })
        }

        method !run-supply-code(&code, $state) {
            $state.run-operation({
                my &*ADD-WHENEVER = sub ($supply, &whenever-block) {
                    $state.increment-active();
                    my $tap = $supply.tap(
                        -> \value {
                            self!run-supply-code({ whenever-block(value) }, $state)
                        },
                        done => {
                            $state.delete-active-tap($tap) if $tap.DEFINITE;
                            my @phasers := &whenever-block.phasers('LAST');
                            if @phasers {
                                self!run-supply-code({ .() for @phasers }, $state)
                            }
                            self!deactivate-one($state);
                        },
                        quit => -> \ex {
                            $state.delete-active-tap($tap) if $tap.DEFINITE;
                            self!run-supply-code({
                                my $handled;
                                my $phaser := &whenever-block.phasers('QUIT')[0];
                                if $phaser.DEFINITE {
                                    $handled = $phaser(ex) === Nil;
                                }
                                if $handled {
                                    self!deactivate-one($state);
                                }
                                elsif $state.get-and-zero-active() {
                                    $state.quit().(ex) if $state.quit;
                                    self!teardown($state);
                                }
                            }, $state);
                        });
                    $state.add-active-tap($tap);
                    $tap
                }

                my $emitter = {
                    my \ex := nqp::exception();
                    $state.emit().(nqp::getpayload(ex)) if $state.emit;
                    nqp::resume(ex)
                }
                my $done = {
                    $state.done().() if $state.done;
                    $state.get-and-zero-active();
                    self!teardown($state);
                }
                my $catch = {
                    my \ex = EXCEPTION(nqp::exception());
                    $state.quit().(ex) if $state.quit;
                    $state.get-and-zero-active();
                    self!teardown($state);
                }
                nqp::handle(code(),
                    'EMIT', $emitter(),
                    'DONE', $done(),
                    'CATCH', $catch(),
                    'NEXT', 0);
            });
        }

        method !deactivate-one($state) {
            $state.run-operation({
                if $state.decrement-active() == 0 {
                    $state.done().() if $state.done;
                    self!teardown($state);
                }
            });
        }

        method !teardown($state) {
            .close for $state.consume-active-taps;
            while $state.close-phasers.pop() -> $close {
                $close();
            }
        }

        method live { False }
        method sane { True }
        method serial { True }
    }.new(:&block))
}

sub WHENEVER(Supply() $supply, &block) {
    my \adder = &*ADD-WHENEVER;
    adder.defined
        ?? adder.($supply, &block)
        !! X::WheneverOutOfScope.new.throw
}

sub REACT(&block) {
    my $s = SUPPLY(&block);
    my $p = Promise.new;
    $s.tap(
        { warn "Useless use of emit in react" },
        done => { $p.keep(Nil) },
        quit => { $p.break($_) });
    await $p;
}

# vim: ft=perl6 expandtab sw=4
