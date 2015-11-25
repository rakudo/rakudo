# When we tap a Supply, we get back a Tap object. We close the tap in order
# to turn off the flow of values.
my class Tap {
    has &!on-close;

    submethod BUILD(:&!on-close) { }

    method new(&on-close) {
        self.bless(:&on-close)
    }

    method close() {
        &!on-close() if &!on-close;
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
    method is-live() { ... }    # Taps into a live data source
    method is-serial() { ... }  # Promises no concurrent emits
    method is-sane() { ... }    # Matches emit* [done|quit] grammar
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
        " - To use a Publisher in order to get a live supply\n" ~
        " - To use Supply.on-demand to create an on-demand supply\n" ~
        " - To create a Supply using a supply block"
    }
}


# A Supply is like an asynchronous Seq. All the methods that you can do on
# a Supply go in here.
my class Publisher { ... }
my class Supply {
    has Tappable $!tappable;

    proto method new(|) { * }
    multi method new() {
        X::Supply::New.new.throw
    }
    multi method new(Tappable $tappable) {
        self.bless(:$tappable);
    }
    submethod BUILD(:$!tappable!) {}

    method live(Supply:D:) { $!tappable.is-live }
    method serial(Supply:D:) { $!tappable.is-serial }

    my \NOP = -> {};
    my \DEATH = -> $ex { $ex.throw };
    method tap(Supply:D: &emit = NOP, :&done = NOP, :&quit = DEATH) {
        $!tappable.tap(&emit, &done, &quit)
    }

    method act(Supply:D: &actor) {
        self.sanitize.tap(&actor)
    }

    ##
    ## Supply factories
    ##

    method on-demand(&producer, :&closing, :$scheduler = CurrentThreadScheduler) {
        Supply.new(class :: does Tappable {
            has &!producer;
            has &!closing;
            has $!scheduler;

            submethod BUILD(:&!producer!, :&!closing!, :$!scheduler!) {}

            method tap(&emit, &done, &quit) {
                my $p = Publisher.new;
                $p.Supply.tap(&emit, :&done, :&quit); # sanitizes
                $!scheduler.cue({ &!producer($p) },
                    catch => -> \ex { $p.quit(ex) });
                Tap.new(&!closing)
            }
            
            method is-live() { False }
            method is-sane() { True }
            method is-serial() { True }
        }.new(:&producer, :&closing, :$scheduler))
    }

    method from-list(*@values, :$scheduler = CurrentThreadScheduler) {
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

            submethod BUILD(:$!scheduler, :$!interval, :$!delay) {}

            method tap(&emit, |) {
                my $cancellation = $!scheduler.cue(
                    {
                        state $i = 0;
                        emit($i++);
                        CATCH { $cancellation.cancel }
                    },
                    :every($!interval), :in($!delay)
                );
                Tap.new({ $cancellation.cancel })
            }

            method is-live { False }
            method is-sane { True }
            method is-serial { False }
        }.new(:$interval, :$delay, :$scheduler));
    }

    ##
    ## Simple operations are those that operate on a single Supply, carry its
    ## liveness, and are always serial. We implement the directly as they are
    ## common and fairly "hot path".
    ##

    my role SimpleOpTappable does Tappable {
        has $!source;
        has $!source-tap;
        has int $!cleaned-up;
        method is-live() { $!source.is-live }
        method is-sane() { True }
        method is-serial() { True }
        method !cleanup() {
            unless $!cleaned-up {
                $!cleaned-up = 1;
                $!source-tap.close;
            }
        }
    }

    method serialize(Supply:D:) {
        $!tappable.is-serial ?? self !! Supply.new(class :: does SimpleOpTappable {
            has $!lock = Lock.new;

            submethod BUILD(:$!source!) {}

            method tap(&emit, &done, &quit) {
                $!source-tap = $!source.tap(
                    -> \value{
                        $!lock.protect: { emit(value); }
                    },
                    done => -> {
                        $!lock.protect: {
                            done();
                            self!cleanup;
                        }
                    },
                    quit => -> $ex {
                        $!lock.protect: {
                            quit($ex);
                            self!cleanup;
                        }
                    });
                Tap.new({ self!cleanup })
            }
        }.new(source => self))
    }    

    method sanitize() {
        $!tappable.is-sane ?? self !! Supply.new(class :: does SimpleOpTappable {
            has int $!finished;

            submethod BUILD(:$!source!) {}

            method tap(&emit, &done, &quit) {
                $!source-tap = $!source.tap(
                    -> \value{
                        emit(value) unless $!finished;
                    },
                    done => -> {
                        unless $!finished {
                            $!finished = 1;
                            done();
                            self!cleanup;
                        }
                    },
                    quit => -> $ex {
                        unless $!finished {
                            $!finished = 1;
                            quit($ex);
                            self!cleanup;
                        }
                    });
                Tap.new({ self!cleanup })
            }
        }.new(source => self.serialize))
    }

    method on-close(Supply:D: &on-close) {
        return Supply.new(class :: does SimpleOpTappable {
            has int $!finished;
            has &!on-close;

            submethod BUILD(:$!source!, :&!on-close!) {}

            method tap(&emit, &done, &quit) {
                $!source-tap = $!source.tap(&emit, :&done, :&quit);
                Tap.new({
                    &!on-close();
                    self!cleanup
                })
            }
        }.new(source => self, :&on-close))
    }

    method map(Supply:D: &mapper) {
        Supply.new(class :: does SimpleOpTappable {
            has &!mapper;

            submethod BUILD(:$!source!, :&!mapper!) {}

            method tap(&emit, &done, &quit) {
                $!source-tap = $!source.tap(
                    -> \value {
                        my \result = try &!mapper(value);
                        if $! {
                            quit($!);
                            self!cleanup;
                        }
                        else {
                            emit(result)
                        }
                    },
                    done => -> {
                        done();
                        self!cleanup;
                    },
                    quit => -> $ex {
                        quit($ex);
                        self!cleanup;
                    });
                Tap.new({ self!cleanup })
            }
        }.new(source => self.sanitize, :&mapper))
    }

    method grep(Supply:D: Mu $test) {
        Supply.new(class :: does SimpleOpTappable {
            has Mu $!test;

            submethod BUILD(:$!source!, Mu :$!test!) {}

            method tap(&emit, &done, &quit) {
                $!source-tap = $!source.tap(
                    -> \value {
                        my \accepted = try $test.ACCEPTS(value);
                        if accepted {
                            emit(value);
                        }
                        elsif $! {
                            quit($!);
                            self!cleanup;
                        }
                    },
                    done => -> {
                        done();
                        self!cleanup;
                    },
                    quit => -> $ex {
                        quit($ex);
                        self!cleanup;
                    });
                Tap.new({ self!cleanup })
            }
        }.new(source => self.sanitize, :$test))
    }

    method schedule-on(Supply:D: Scheduler $scheduler) {
        Supply.new(class :: does SimpleOpTappable {
            has $!scheduler;

            submethod BUILD(:$!source!, :$!scheduler!) {}

            method tap(&emit, &done, &quit) {
                $!source-tap = $!source.tap(
                    -> \value {
                        $!scheduler.cue: { emit(value) }
                    },
                    done => -> {
                        $!scheduler.cue: { done(); self!cleanup; }
                    },
                    quit => -> $ex {
                        $!scheduler.cue: { quit($ex); self!cleanup; }
                    });
                Tap.new({ self!cleanup })
            }
        }.new(source => self.sanitize, :$scheduler))
    }

    method start(Supply:D: &startee) {
        self.map: -> \value {
            Supply.new(class :: does SimpleOpTappable {
                has $!value;
                has &!startee;

                submethod BUILD(:$!value, :&!startee) { }

                method tap(&emit, &done, &quit) {
                    Promise.start({ &!startee($!value) }).then({
                        if .status == Kept {
                            emit(.result);
                            done();
                        }
                        else {
                            quit(.cause);
                        }
                    });
                    Tap.new({ self!cleanup })
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

            submethod BUILD(:$!source!, :$!time!, :$!scheduler!) {}

            method tap(&emit, &done, &quit) {
                $!source-tap = $!source.tap(
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
                                                self!cleanup;
                                            }
                                        }
                                    }
                                });
                        }
                    },
                    done => -> {
                        done();
                        self!cleanup;
                    },
                    quit => -> $ex {
                        quit($ex);
                        self!cleanup;
                    });
                Tap.new({ self!cleanup })
            }
        }.new(source => self.sanitize, :$time, :$scheduler))
    }

    method delayed(Supply:D: $time, :$scheduler = $*SCHEDULER) {
        return self unless $time;  # nothing to do
        Supply.new(class :: does SimpleOpTappable {
            has $!time;
            has $!scheduler;

            submethod BUILD(:$!source!, :$!time, :$!scheduler!) {}

            method tap(&emit, &done, &quit) {
                $!source-tap = $!source.tap(
                    -> \value {
                        $!scheduler.cue: { emit(value) }, :in($time)
                    },
                    done => -> {
                        $!scheduler.cue: { done(); self!cleanup; }, :in($time)
                    },
                    quit => -> $ex {
                        $!scheduler.cue: { quit($ex); self!cleanup; }, :in($time)
                    });
                Tap.new({ self!cleanup })
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
        ).throw if Rakudo::Internals.NOT_ALL_DEFINED_TYPE(@s,Supply);

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
                    done;
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
                    my $p = Publisher.new;
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
                    done;
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
            quit => -> $ex { $c.quit($ex) };
        $c
    }

    method list(Supply:D:) {
        # Use a Channel to handle any asynchrony.
        self.Channel.list;
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

    method unique(Supply:D $self: :&as, :&with, :$expires) {
        supply {
            if $expires {
                if &with and &with !=== &[===] {
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
                if &with and &with !=== &[===] {
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
                    if $first || !&with($target,$last) {
                        $first = 0;
                        $last  = $target;
                        emit(val);
                    }
                }
            }
            else {
                whenever self -> \val {
                    if $first || !&with(val,$last) {
                        $first = 0;
                        $last = val;
                        emit(val);
                    }
                }
            }
        }
    }

    proto method rotor(|) {*}
    multi method rotor(Supply:D $self: *@cycle, :$partial) {
        my @c := @cycle.is-lazy ?? @cycle !! (@cycle xx *).flat.cache;
        supply {
            my Int $elems;
            my Int $gap;
            my int $to-skip;
            my int $skip;
            my \c = @c.iterator;

            sub next-batch() {
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
            sub flush() {
                emit( @batched.splice(0, +@batched, @batched[* + $gap .. *]) );
                $skip = $to-skip;
            }

            whenever self -> \val {
                @batched.append: val unless $skip && $skip--;
                if @batched.elems == $elems {
                    flush;
                    next-batch;
                }
                LAST {
                    flush if @batched and $partial;
                    done;
                }
            }
        }
    }

    method batch(Supply:D $self: :$elems, :$seconds ) {
        return self if (!$elems or $elems == 1) and !$seconds;  # nothing to do
        supply {
            my @batched;
            my $last_time;
            sub flush {
                emit([@batched]);
                @batched = ();
            }
            sub final-flush {
                flush if @batched;
                done;
            }

            if $seconds {
                $last_time = time div $seconds;

                if $elems { # and $seconds
                    whenever self -> \val {
                      my $this_time = time div $seconds;
                      if $this_time != $last_time {
                          flush if @batched;
                          $last_time = $this_time;
                          @batched.append: val;
                        }
                        else {
                            @batched.append: val;
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
                        @batched.append: val;
                        LAST { final-flush; }
                    }
                }
            }
            else { # just $elems
                whenever self -> \val {
                    @batched.append: val;
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
                    done;
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
                    done;
                }
            }
        }
    }

    method elems(Supply:D $self: $seconds? ) {
        supply {
            my $elems = 0;
            if $seconds {
                my $last_time = time div $seconds;
                my $last_elems = $elems;
                whenever self -> \val {
                    $last_elems = ++$elems;
                    my $this_time = time div $seconds;
                    if $this_time != $last_time {
                        emit($elems);
                        $last_time = $this_time;
                    }
                    LAST {
                        emit($elems) if $elems != $last_elems;
                        done;
                    }
                }
            }
            else {
                whenever self -> \val {
                    emit(++$elems)
                }
            }
        }
    }

    method last(Supply:D $self: Int $number = 1) {  # should be Natural
        supply {
            my @seen;
            if $number == 1 {
                whenever self -> \val {
                    @seen[0] := val;
                    LAST { emit($_) for @seen; }
                }
            }
            else {
                whenever self -> \val {
                    @seen.shift if +@seen == $number;
                    @seen.push: val;
                    LAST { emit($_) for @seen; }
                }
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
                if val.defined {
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

    method reverse(Supply:D:)                 { self.grab( {.reverse} ) }
    method sort(Supply:D: &by = &infix:<cmp>) { self.grab( {.sort(&by)} ) }

    method zip(**@s, :&with) {
        @s.unshift(self) if self.DEFINITE;  # add if instance method
        return supply { } unless +@s;       # nothing to be done

        X::Supply::Combinator.new(
           combinator => 'zip'
        ).throw if Rakudo::Internals.NOT_ALL_DEFINED_TYPE(@s,Supply);

        return @s[0]  if +@s == 1;          # nothing to be done

        supply {
            my @values = [] xx +@s;
            for @s.kv -> $index, $supply {
                if &with {
                    whenever self -> \val {
                        @values[$index].push(val);
                        emit( [[&with]] @values.map(*.shift) ) if all(@values);
                    }
                }
                else {
                    whenever self -> \val {
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
        ).throw if Rakudo::Internals.NOT_ALL_DEFINED_TYPE(@s,Supply);

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
                    whenever self -> \val {
                        --$uninitialised
                        if $uninitialised > 0 && not @values.EXISTS-POS($index);
                        @values[$index] = val;
                        emit( [[&with]] @values ) unless $uninitialised;
                    }
                }
                else {
                    whenever self -> \val {
                        --$uninitialised
                            if $uninitialised > 0 && not @values.EXISTS-POS($index);
                        @values[$index] = val;
                        emit( @values.List.item ) unless $uninitialised;
                    }
                }
            }
        }
    }

#    proto method throttle(|) { * }
#    multi method throttle(Supply:D $self:
#      Int()  $elems,
#      Real() $seconds,
#      Real() $delay  = 0,
#      :$scheduler    = $*SCHEDULER,
#      :$control,
#      :$status,
#      :$bleed,
#      :$vent-at,
#    ) {
#        my $timer = Supply.interval($seconds,$delay,:$scheduler);
#        my @buffer;
#        my int $limit   = $elems;
#        my int $allowed = $limit;
#        my int $emitted;
#        my int $bled;
#        my int $vent = $vent-at if $bleed;;
#        sub emit-status($id) {
#           $status.emit(
#             { :$allowed, :$bled, :buffered(+@buffer),
#               :$emitted, :$id,   :$limit,  :$vent-at } );
#        }
#        on -> $res {
#            $timer => { 
#                emit => -> \tick {
#                    if +@buffer -> \buffered {
#                        my int $todo = buffered > $limit ?? $limit !! buffered;
#                        $res.emit(@buffer.shift) for ^$todo;
#                        $emitted = $emitted + $todo;
#                        $allowed = $limit   - $todo;
#                    }
#                    else {
#                        $allowed = $limit;
#                    }
#                },
#            },
#            $self => {
#                emit => -> \val {
#                    if $allowed {
#                        $res.emit(val);
#                        $emitted = $emitted + 1;
#                        $allowed = $allowed - 1;
#                    }
#                    elsif $vent && +@buffer >= $vent {
#                        $bleed.emit(val);
#                    }
#                    else {
#                        @buffer.push(val);
#                    }
#                },
#                done => {
#                    $res.done;  # also stops the timer ??
#                    $control.done if $control;
#                    $status.done  if $status;
#                    if $status {
#                        emit-status("done");
#                        $status.done;
#                    }
#                    if $bleed && @buffer {
#                        $bleed.emit(@buffer.shift) while @buffer;
#                        $bleed.done;
#                    }
#                },
#            },
#            $control
#              ?? ($control => {
#                   emit => -> \val {
#                       my str $type;
#                       my str $value;
#                       Rakudo::Internals.KEY_COLON_VALUE(val,$type,$value);
#
#                       if $type eq 'limit' {
#                           my int $extra = $value - $limit;
#                           $allowed = $extra > 0 || $allowed + $extra >= 0
#                             ?? $allowed + $extra
#                             !! 0;
#                           $limit = $value;
#                       }
#                       elsif $type eq 'bleed' && $bleed {
#                           my int $todo = $value min +@buffer;
#                           $bleed.emit(@buffer.shift) for ^$todo;
#                           $bled = $bled + $todo;
#                       }
#                       elsif $type eq 'status' && $status {
#                           emit-status($value);
#                       }
#                       elsif $type eq 'vent-at' && $bleed {
#                           $vent = $value;
#                           if $vent && +@buffer > $vent {
#                               $bleed.emit(@buffer.shift)
#                                 until !@buffer || +@buffer == $vent;
#                           }
#                       }
#                   },
#                 })
#              !! |()
#        }
#    }
#    multi method throttle(Supply:D $self:
#      Int()  $elems,
#      Callable:D $process,
#      Real() $delay = 0,
#      :$scheduler   = $*SCHEDULER,
#      :$control,
#      :$status,
#      :$bleed,
#      :$vent-at,
#    ) {
#        sleep $delay if $delay;
#        my @buffer;
#        my int $limit   = $elems;
#        my int $allowed = $limit;
#        my int $running;
#        my int $emitted;
#        my int $bled;
#        my int $done;
#        my int $vent = $vent-at if $bleed;
#        my $ready = Supply.new;
#        sub start-process(\val) {
#            my $p = Promise.start( $process, :$scheduler, val );
#            $running = $running + 1;
#            $allowed = $allowed - 1;
#            $p.then: { $ready.emit($p) };
#        }
#        sub emit-status($id) {
#           $status.emit(
#             { :$allowed, :$bled, :buffered(+@buffer),
#               :$emitted, :$id,   :$limit, :$running } );
#        }
#        on -> $res {
#            $self => {
#                emit => -> \val {
#                    $allowed > 0
#                      ?? start-process(val)
#                      !! $vent && $vent == +@buffer
#                        ?? $bleed.emit(val)
#                        !! @buffer.push(val);
#                },
#                done => {
#                    $done = 1;
#                },
#            },
#            $ready => {  # when a process is ready
#                emit => -> \val {
#                    $running = $running - 1;
#                    $allowed = $allowed + 1;
#                    $res.emit(val);
#                    $emitted = $emitted + 1;
#                    start-process(@buffer.shift) if $allowed > 0 && @buffer;
#
#                    if $done && !$running {
#                        $control.done if $control;
#                        if $status {
#                            emit-status("done");
#                            $status.done;
#                        }
#                        if $bleed && @buffer {
#                            $bleed.emit(@buffer.shift) while @buffer;
#                            $bleed.done;
#                        }
#                        $res.done;
#                    }
#                },
#            },
#            $control
#              ?? ($control => {
#                   emit => -> \val {
#                       my str $type;
#                       my str $value;
#                       Rakudo::Internals.KEY_COLON_VALUE(val,$type,$value);
#
#                       if $type eq 'limit' {
#                           $allowed = $allowed + $value - $limit;
#                           $limit   = $value;
#                           start-process(@buffer.shift)
#                             while $allowed > 0 && @buffer;
#                       }
#                       elsif $type eq 'bleed' && $bleed {
#                           my int $todo = $value min +@buffer;
#                           $bleed.emit(@buffer.shift) for ^$todo;
#                           $bled = $bled + $todo;
#                       }
#                       elsif $type eq 'status' && $status {
#                           emit-status($value);
#                       }
#                       elsif $type eq 'vent-at' && $bleed {
#                           $vent = $value;
#                           if $vent && +@buffer > $vent {
#                               $bleed.emit(@buffer.shift)
#                                 until !@buffer || +@buffer == $vent;
#                           }
#                       }
#                   },
#                 })
#              !! |()
#        }
#    }
}

# A Publisher is a convenient way to create a live Supply. The publisher can
# be used to emit/done/quit. The Supply objects obtained from it will tap into
# the same live Supply.
my class Publisher {
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

        method emit(\value) {
            my $snapshot := $!tappers;
            if nqp::isconcrete($snapshot) {
                my int $n = nqp::elems($snapshot);
                loop (my int $i = 0; $i < $n; $i = $i + 1) {
                    nqp::atpos($snapshot, $i).emit()(value);
                }
            }
        }

        method done() {
            my $snapshot := $!tappers;
            if nqp::isconcrete($snapshot) {
                my int $n = nqp::elems($snapshot);
                loop (my int $i = 0; $i < $n; $i = $i + 1) {
                    nqp::atpos($snapshot, $i).done()();
                }
            }
        }

        method quit($ex) {
            my $snapshot := $!tappers;
            if nqp::isconcrete($snapshot) {
                my int $n = nqp::elems($snapshot);
                loop (my int $i = 0; $i < $n; $i = $i + 1) {
                    nqp::atpos($snapshot, $i).quit()($ex);
                }
            }
        }

        method is-live     { True  }
        method is-serial() { False }
        method is-sane()   { False }
    }

    has $!taplist = TapList.new;

    method emit(Publisher:D: Mu \value) {
        $!taplist.emit(value);
    }

    method done(Publisher:D:) {
        $!taplist.done();
    }

    proto method quit($) { * }
    multi method quit(Publisher:D: Exception $ex) {
        $!taplist.quit($ex);
    }
    multi method quit(Publisher:D: Str() $message) {
        $!taplist.quit(X::AdHoc.new(:$message));
    }

    method Supply(Publisher:D:) {
        Supply.new($!taplist).sanitize
    }

    method unsanitized-supply(Publisher:D:) {
        Supply.new($!taplist)
    }
}

sub SUPPLY(&block) {
    my class SupplyBlockState {
        has &.emit;
        has &.done;
        has &.quit;
        has $.lock;
        has $.active is rw;
        has %.active-taps;
    }

    Supply.new(class :: does Tappable {
        has &!block;

        submethod BUILD(:&!block) { }

        method tap(&emit, &done, &quit) {
            my $state = SupplyBlockState.new(
                :&emit, :&done, :&quit,
                lock => Lock.new,
                active => 1);
            self!run-supply-code(&!block, $state);
            self!deactivate-one($state);
            Tap.new(-> { self!teardown($state) })
        }

        method !run-supply-code(&code, $state) {
            my &*ADD-WHENEVER = sub ($supply, &whenever-block) {
                $state.active++;
                my $tap = $supply.tap(
                    -> \value {
                        self!run-supply-code({ whenever-block(value) }, $state)
                    },
                    done => {
                        $state.active-taps{nqp::objectid($tap)}:delete if $tap.DEFINITE;
                        my @phasers := &whenever-block.phasers('LAST');
                        if @phasers {
                            self!run-supply-code({ .() for @phasers }, $state)
                        }
                        self!deactivate-one($state);
                    },
                    quit => -> \ex {
                        $state.active-taps{nqp::objectid($tap)}:delete if $tap.DEFINITE;
                        my $handled;
                        my $phaser := &whenever-block.phasers('QUIT')[0];
                        if $phaser.DEFINITE {
                            self!run-supply-code({ $handled = $phaser(ex) === Nil }, $state)
                        }
                        if $handled {
                            self!deactivate-one($state);
                        }
                        elsif $state.active {
                            $state.quit().(ex) if $state.quit;
                            $state.active = 0;
                            self!teardown($state);
                        }
                    });
                $state.active-taps{nqp::objectid($tap)} = $tap;
            }

            my $emitter = {
                my \ex := nqp::exception();
                $state.emit().(nqp::getpayload(ex)) if $state.emit;
                nqp::resume(ex)
            }
            my $done = {
                $state.done().() if $state.done;
                $state.active = 0;
                self!teardown($state);
            }
            my $catch = {
                my \ex = EXCEPTION(nqp::exception());
                $state.quit().(ex) if $state.quit;
                $state.active = 0;
                self!teardown($state);
            }
            nqp::handle($state.lock.protect(&code),
                'EMIT', $emitter(),
                'DONE', $done(),
                'CATCH', $catch());
        }

        method !deactivate-one($state) {
            $state.lock.protect({
                if --$state.active == 0 {
                    $state.done().() if $state.done;
                    self!teardown($state);
                }
            });
        }

        method !teardown($state) {
            .close for $state.active-taps.values;
            $state.active-taps = ();
        }

        method is-live { False }
        method is-sane { True }
        method is-serial { True }
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
