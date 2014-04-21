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
        has &.closing;
        has $.supply;
        method close() {
            $!supply.close(self)
        }
    }

    has @!tappers;
    has $!tappers_lock = Lock.new;

    method tap(&more = -> $ { }, :&done, :&quit = {.die}, :&closing) {
        my $sub = Tap.new(:&more, :&done, :&quit, :&closing, :supply(self));
        $!tappers_lock.protect({
            @!tappers.push($sub);
        });
        $sub
    }

    method close(Tap $t) {
        $!tappers_lock.protect({
            @!tappers .= grep(* !=== $t);
        });
        if $t.closing -> &closing {
            closing();
        }
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
        self.tap( -> \val { $c.send(val) },
          done => { $c.close },
          quit => -> $ex { $c.quit($ex) });
        $c
    }

    method list() {
        # Use a Channel to handle any asynchrony.
        my $c = self.Channel;
        map sub ($) {
            winner $c {
                more * { $_ }
                done * { last }
            }
        }, *;
    }

    method for(|c)             { SupplyOperations.for(|c) }
    method interval(|c)        { SupplyOperations.interval(|c) }
    method flat()              { SupplyOperations.flat(self) }
    method do(&side_effect)    { SupplyOperations.do(self, &side_effect) }
    method grep(&filter)       { SupplyOperations.grep(self, &filter) }
    method map(&mapper)        { SupplyOperations.map(self, &mapper) }
    method uniq(:&as,:&with)   { SupplyOperations.uniq(self, :&as, :&with) }
    method squish(:&as,:&with) { SupplyOperations.squish(self, :&as, :&with) }
    method rotor( $elems?, $overlap? ) {
        SupplyOperations.rotor(self, $elems, $overlap)
    }
    method batch( :$elems, :$seconds ) {
        SupplyOperations.batch( self, :$elems, :$seconds)
    }
    method merge(*@s)          { SupplyOperations.merge(self, @s) }
    method zip(*@s,:&with)     { SupplyOperations.zip(self, @s, :&with) }

    method act(&actor) {
        self.do(&actor).tap(|%_)
    }
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
          $source, $lock, $index, :&more, :&done is copy, :&quit is copy
        ) {
            &more // X::Supply::On::NoMore.new.throw;
            &done //= { self.done };
            &quit //= -> $ex { self.quit($ex) };

            my &tap_more = &more.arity == 2
              ?? -> \val {
                  $lock.protect({ more(val,$index) });
                  CATCH { default { self.quit($_) } }
              }
              !!  -> \val {
                  $lock.protect({ more(val) });
                  CATCH { default { self.quit($_) } }
              };

            my &tap_done = &done.arity == 2
              ?? {
                  $lock.protect({ done($index) });
                  CATCH { default { self.quit($_) } }
              }
              !! {
                  $lock.protect({ done() });
                  CATCH { default { self.quit($_) } }
              };

            my &tap_quit = &quit.arity == 2
              ?? -> $ex {
                  $lock.protect({ quit($ex,$index) });
                  CATCH { default { self.quit($_) } }
              }
              !! -> $ex {
                  $lock.protect({ quit($ex) });
                  CATCH { default { self.quit($_) } }
              };

            $source.tap( &tap_more, done => &tap_done, quit => &tap_quit );
        }
        
        method tap(|c) {
            my @to_close;
            my $sub = self.Supply::tap( |c, closing => {.close for @to_close});
            my @tappers = &!setup(self);
            my $lock    = Lock.new;

            sub add ($source, $what, $index?) {
                unless $source ~~ Supply {
                    X::Supply::On::BadSetup.new.throw;
                }
                given $what {
                    when EnumMap {
                        @to_close.push(self!add_source($source, $lock, $index, |$what));
                    }
                    when Callable {
                        @to_close.push(self!add_source($source, $lock, $index, more => $what));
                    }
                    default {
                        X::Supply::On::BadSetup.new.throw;
                    }
                }
            }

            for @tappers -> $tap {
                unless $tap ~~ Pair {
                    X::Supply::On::BadSetup.new.throw;
                }
                given $tap.key {
                    when Positional {
                        my $todo := $tap.value;
                        for .list.kv -> $index, $supply {
                            add( $supply, $todo, $index );
                        }
                    }
                    when Supply {
                        add( $_, $tap.value );
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
