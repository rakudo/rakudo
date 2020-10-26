# When we tap a Supply, we get back a Tap object. We close the tap in order
# to turn off the flow of values.
my class Tap {
    has &!on-close;

    submethod BUILD(:&!on-close --> Nil) { } # for subclasses of Tap

    multi method new(Tap: --> Tap:D) {
        nqp::create(self)
    }
    multi method new(Tap: &on-close --> Tap:D) {
        nqp::eqaddr(self.WHAT,Tap)
          ?? nqp::p6bindattrinvres(              # we're a real Tap, fast path
               nqp::create(self),Tap,'&!on-close',&on-close
             )
          !! self.bless(:&on-close)              # subclass, use slow path
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
        nqp::eqaddr(self.WHAT,Supply)
          ?? nqp::p6bindattrinvres(             # we're a real Supply, fast path
               nqp::create(self),Supply,'$!tappable',$tappable
             )
          !! self.bless(:$tappable)             # subclass, use slow path
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

# continued in src/core.c/Supply-factories.pm6

# vim: expandtab shiftwidth=4
