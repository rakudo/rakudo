# Anything that can be subscribed to does this role. It provides the basic
# supply management infrastructure, as well as various coercions that
# turn Supply-like things into something else and convenience forms of calls
# to SupplyOperations.

my class SupplyOperations { ... }
my role Supply {
    my class Tap {
        has &.next;
        has &.last;
        has &.fail;
        has $.supply;
        method untap() {
            $!supply.untap(self)
        }
    }

    has @!tappers;
    has $!tappers_lock = Lock.new;

    method tap(&next, &last?, &fail?) {
        my $sub = Tap.new(:&next, :&last, :&fail, :supply(self));
        $!tappers_lock.protect({
            @!tappers.push($sub);
        });
        $sub
    }

    method untap(Tap $t) {
        $!tappers_lock.protect({
            @!tappers .= grep(* !=== $t);
        });
    }

    method tappers() {
        # Shallow clone to provide safe snapshot.
        my @tappers;
        $!tappers_lock.protect({ @tappers = @!tappers });
        @tappers
    }

    method Channel() {
        my $c = Channel.new();
        self.tap(
            -> \val { $c.send(val) },
            { $c.close },
            -> $ex { $c.fail($ex) });
        $c
    }

    method list() {
        # Use a Channel to handle any asynchrony.
        my $c = self.Channel;
        (1..*).map(sub ($) {
            winner(
                $c        => -> \val { return val },
                $c.closed => -> $p { $p.result; last }
            )
        })
    }

    method do(&side_effect) { SupplyOperations.do(self, &side_effect) }
    method grep(&filter)    { SupplyOperations.grep(self, &filter) }
    method map(&mapper)     { SupplyOperations.map(self, &mapper) }
    method merge($s)        { SupplyOperations.merge(self, $s) }
    method zip($s, *@with)  { SupplyOperations.zip(self, $s, |@with) }
}

# The on meta-combinator provides a mechanism for implementing thread-safe
# combinators on Supplies. It subscribes to a bunch of sources, but will
# only let one of the specified callbacks to handle their next/last/fail run
# at a time. A little bit actor-like.
my class X::Supply::On::BadSetup is Exception {
    method message() {
        "on requires a callable that returns a list of pairs with Supply keys"
    }
}
my class X::Supply::On::NoNext is Exception {
    method message() {
        "on requires that next be specified for each supply"
    }
}
sub on(&setup) {
    my class OnSupply does Supply {
        has &!setup;
        
        submethod BUILD(:&!setup) { }

        method !add_source(
          $source, $lock, :&next, :&last is copy, :&fail is copy
        ) {
            unless defined &next {
                X::Supply::On::NoNext.new.throw;
            }
            unless defined &last {
                &last = { self.last }
            }
            unless defined &fail {
                &fail = -> $ex { self.fail($ex) }
            }
            $source.tap(
                -> \val {
                    $lock.protect({ next(val) });
                    CATCH { self.fail($_) }
                },
                {
                    $lock.protect({ last() });
                    CATCH { self.fail($_) }
                },
                -> $ex {
                    $lock.protect({ fail($ex) });
                    CATCH { self.fail($_) }
                }
            );
        }
        
        method tap(|c) {
            my $sub     = self.Supply::tap(|c);
            my @tappers = &!setup(self);
            my $lock    = Lock.new;

            for @tappers -> $tap {
                unless $tap ~~ Pair && $tap.key ~~ Supply {
                    X::Supply::On::BadSetup.new.throw;
                }
                given $tap.value {
                    when EnumMap {
                        self!add_source($tap.key, $lock, |$tap.value);
                    }
                    when Callable {
                        self!add_source($tap.key, $lock, next => $tap.value);
                    }
                    default {
                        X::Supply::On::BadSetup.new.throw;
                    }
                }
            }
            $sub
        }

        method next(\msg) {
            for self.tappers {
                .next().(msg)
            }
            Nil;
        }

        method last() {
            for self.tappers {
                if .last -> $l { $l() }
            }
            Nil;
        }

        method fail($ex) {
            for self.tappers {
                if .fail -> $t { $t($ex) }
            }
            Nil;
        }
    }

    OnSupply.new(:&setup)
}
