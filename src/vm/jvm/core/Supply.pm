# Anything that can be subscribed to does this role. It provides the basic
# subscription management infrastructure, as well as various coercions that
# turn Subscribable things into something else and convenience forms of calls
# to SubscribableOperations.
my class SubscribableOperations { ... }
my role Subscribable {
    my class Subscription {
        has &.next;
        has &.last;
        has &.fail;
        has $.subscribable;
        method unsubscribe() {
            $!subscribable.unsubscribe(self)
        }
    }

    has @!subscriptions;
    has $!subscriptions_lock = Lock.new;

    method subscribe(&next, &last?, &fail?) {
        my $sub = Subscription.new(:&next, :&last, :&fail, :subscribable(self));
        $!subscriptions_lock.run({
            @!subscriptions.push($sub);
        });
        $sub
    }

    method unsubscribe(Subscription $s) {
        $!subscriptions_lock.run({
            @!subscriptions.=grep(* !=== $s);
        });
    }

    method subscriptions() {
        # Shallow clone to provide safe snapshot.
        my @subs;
        $!subscriptions_lock.run({ @subs = @!subscriptions });
        @subs
    }

    method Channel() {
        my $c = Channel.new();
        self.subscribe(
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

    method do(&side_effect) { SubscribableOperations.do(self, &side_effect) }
    method grep(&filter)    { SubscribableOperations.grep(self, &filter) }
    method map(&mapper)     { SubscribableOperations.map(self, &mapper) }
    method merge($s)        { SubscribableOperations.merge(self, $s) }
    method zip($s, *@with)  { SubscribableOperations.zip(self, $s, |@with) }
}

# The on meta-combinator provides a mechanism for implementing thread-safe
# combinators on Subscribables. It subscribes to a bunch of sources, but will
# only let one of the specified callbacks to handle their next/last/fail run
# at a time. A little bit actor-like.
my class X::Subscribable::On::BadSetup is Exception {
    method message() {
        "on requires a callable that returns a list of pairs with Subscribable keys"
    }
}
my class X::Subscribable::On::NoNext is Exception {
    method message() {
        "on requires that next be specified for each subscribable"
    }
}
sub on(&setup) {
    my class OnSubscribable does Subscribable {
        has &!setup;
        
        submethod BUILD(:&!setup) { }

        method !add_source($source, $lock, :&next, :&last is copy, :&fail is copy) {
            unless defined &next {
                X::Subscribable::On::NoNext.new.throw;
            }
            unless defined &last {
                &last = { self.last }
            }
            unless defined &fail {
                &fail = -> $ex { self.fail($ex) }
            }
            $source.subscribe(
                -> \val {
                    $lock.run({ next(val) });
                    CATCH { self.fail($_) }
                },
                {
                    $lock.run({ last() });
                    CATCH { self.fail($_) }
                },
                -> $ex {
                    $lock.run({ fail($ex) });
                    CATCH { self.fail($_) }
                }
            );
        }
        
        method subscribe(|c) {
            my $sub = self.Subscribable::subscribe(|c);
            my @subscriptions = &!setup(self);
            my $lock = Lock.new;
            for @subscriptions -> $ssn {
                unless $ssn ~~ Pair && $ssn.key ~~ Subscribable {
                    X::Subscribable::On::BadSetup.new.throw;
                }
                given $ssn.value {
                    when EnumMap {
                        self!add_source($ssn.key, $lock, |$ssn.value);
                    }
                    when Callable {
                        self!add_source($ssn.key, $lock, next => $ssn.value);
                    }
                    default {
                        X::Subscribable::On::BadSetup.new.throw;
                    }
                }
            }
            $sub
        }

        method next(\msg) {
            for self.subscriptions {
                .next().(msg)
            }
            Nil;
        }

        method last() {
            for self.subscriptions {
                if .last -> $l { $l() }
            }
            Nil;
        }

        method fail($ex) {
            for self.subscriptions {
                if .fail -> $t { $t($ex) }
            }
            Nil;
        }
    }

    OnSubscribable.new(:&setup)
}
