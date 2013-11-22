# Anything that can be subscribed to does this role. It provides the basic
# supply management infrastructure, as well as various coercions that
# turn Supply-like things into something else and convenience forms of calls
# to SupplyOperations.

my class SupplyOperations { ... }
my role Supply {
    my class Tap {
        has &.more;
        has &.done;
        has &.quit;
        has $.supply;
        method close() {
            $!supply.close(self)
        }
    }

    has @!tappers;
    has $!tappers_lock = Lock.new;

    method tap(&more, &done?, &quit?) {
        my $sub = Tap.new(:&more, :&done, :&quit, :supply(self));
        $!tappers_lock.protect({
            @!tappers.push($sub);
        });
        $sub
    }

    method close(Tap $t) {
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

    method more(\msg) {
        for self.tappers -> $t {
            $t.more().(msg)
        }
        Nil;
    }

    method done() {
        for self.tappers -> $t {
            my $l = $t.done();
            $l() if $l;
        }
        Nil;
    }

    method quit($ex) {
        for self.tappers -> $t {
            my $f = $t.quit();
            $f($ex) if $f;
        }
        Nil;
    }

    method Channel() {
        my $c = Channel.new();
        self.tap(
            -> \val { $c.send(val) },
            { $c.close },
            -> $ex { $c.quit($ex) });
        $c
    }

    method list() {
        # Use a Channel to handle any asynchrony.
        my $c = self.Channel;
        my $condition = False;
        (1..*).map(sub ($) {
            last if $condition;
            winner $c {
                more * { $_ }
                done * { $condition = False; Nil }
            }
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
# only let one of the specified callbacks to handle their more/done/quit run
# at a time. A little bit actor-like.
my class X::Supply::On::BadSetup is Exception {
    method message() {
        "on requires a callable that returns a list of pairs with Supply keys"
    }
}
my class X::Supply::On::NoMore is Exception {
    method message() {
        "on requires that more be specified for each supply"
    }
}
sub on(&setup) {
    my class OnSupply does Supply {
        has &!setup;
        
        submethod BUILD(:&!setup) { }

        method !add_source(
          $source, $lock, :&more, :&done is copy, :&quit is copy
        ) {
            unless defined &more {
                X::Supply::On::NoMore.new.throw;
            }
            unless defined &done {
                &done = { self.done }
            }
            unless defined &quit {
                &quit = -> $ex { self.quit($ex) }
            }
            $source.tap(
                -> \val {
                    $lock.protect({ more(val) });
                    CATCH { self.quit($_) }
                },
                {
                    $lock.protect({ done() });
                    CATCH { self.quit($_) }
                },
                -> $ex {
                    $lock.protect({ quit($ex) });
                    CATCH { self.quit($_) }
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
                        self!add_source($tap.key, $lock, more => $tap.value);
                    }
                    default {
                        X::Supply::On::BadSetup.new.throw;
                    }
                }
            }
            $sub
        }

        method more(\msg) {
            for self.tappers {
                .more().(msg)
            }
            Nil;
        }

        method done() {
            for self.tappers {
                if .done -> $l { $l() }
            }
            Nil;
        }

        method quit($ex) {
            for self.tappers {
                if .quit -> $t { $t($ex) }
            }
            Nil;
        }
    }

    OnSupply.new(:&setup)
}
