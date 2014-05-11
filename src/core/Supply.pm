# Anything that can be subscribed to does this role. It provides the basic
# supply management infrastructure, as well as various coercions that
# turn Supply-like things into something else and convenience forms of calls
# to SupplyOperations.

my class SupplyOperations { ... }

my class Tap {
    has &.more;
    has &.done;
    has &.quit;
    has &.closing;
    has $.supply;

    method close (Tap:D:) { $!supply.close(self) }
}

my role Supply {
    has $!tappers_lock = Lock.new;
    has @!tappers;
    has $!been_tapped;
    has @!paused;

    method tap(Supply:D: &more = -> $ { }, :&done,:&quit={die $_},:&closing) {
        my $tap = Tap.new(:&more, :&done, :&quit, :&closing, :supply(self));
        $!tappers_lock.protect({
            @!tappers.push($tap);
            if @!paused -> \todo {
                $tap.more().($_) for todo;
                @!paused = ();
            }
            $!been_tapped = True;
        });
        $tap
    }

    method close(Supply:D: Tap $t) {
        my $found;
        $!tappers_lock.protect({
            @!tappers .= grep( { $_ === $t ?? !($found = True) !! True } );
        });
        if $t.closing -> &closing {
            closing();
        }
        $found // False;
    }

    method tappers(Supply:D:) {
        # Shallow clone to provide safe snapshot.
        my @tappers;
        $!tappers_lock.protect({ @tappers = @!tappers });
        @tappers
    }

    method more(Supply:D: \msg) {
        if self.tappers -> \tappers {
            .more().(msg) for tappers;
        }
        elsif !$!been_tapped {
            $!tappers_lock.protect({ @!paused.push: msg });
        }
        Nil;
    }

    method done(Supply:D:) {
        for self.tappers -> $t {
            my $l = $t.done();
            $l() if $l;
        }
        Nil;
    }

    method quit(Supply:D: $ex) {
        for self.tappers -> $t {
            my $f = $t.quit();
            $f($ex) if $f;
        }
        Nil;
    }

    method live(Supply:D:) { True };

    method Supply(Supply:) { self }
    method Channel(Supply:D:) {
        my $c = Channel.new();
        self.tap( -> \val { $c.send(val) },
          done => { $c.close },
          quit => -> $ex { $c.quit($ex) });
        $c
    }

    method Promise(Supply:D:) {
        my $l = Lock.new;
        my $p = Promise.new;
        my $v = $p.vow;
        my $t = self.tap(
          -> \val {
              $l.protect( {
                  if $p.status == Planned {
                      $v.keep(val);
                      $t.close()
                  }
              } );
          },
          done => { $v.break("No value received") },
          quit => -> \ex {
              $l.protect( {
                  if $p.status == Planned {
                      $v.break(ex);
                      $t.close()
                  }
              } );
          },
        );
        $p
    }

    method wait(Supply:D:) {
        my $l = Lock.new;
        my $p = Promise.new;
        my $t = self.tap( -> \val {},
          done => {
              $l.protect( {
                  if $p.status == Planned {
                      $p.keep(True);
                      $t.close()
                  }
              } );
          },
          quit => -> \ex {
              $l.protect( {
                  if $p.status == Planned {
                      $p.break(ex);
                      $t.close()
                  }
              } );
          },
        );
        $p.result
    }

    method list(Supply:D:) {
        # Use a Channel to handle any asynchrony.
        my $c = self.Channel;
        map sub ($) {
            winner $c {
                more * { $_ }
                done * { last }
            }
        }, *;
    }

    method for(Supply:U: |c)             { SupplyOperations.for(|c) }
    method interval(Supply:U: |c)        { SupplyOperations.interval(|c) }
    method flat(Supply:D: )              { SupplyOperations.flat(self) }
    method grep(Supply:D: Mu $test)      { SupplyOperations.grep(self, $test) }
    method map(Supply:D: &mapper)        { SupplyOperations.map(self, &mapper) }
    method schedule_on(Supply:D: Scheduler $scheduler) {
        SupplyOperations.schedule_on(self, $scheduler);
    }
    method start(Supply:D: &startee)     { SupplyOperations.start(self, &startee) }
    method stable(Supply:D: $time, :$scheduler = $*SCHEDULER) {
        SupplyOperations.stable(self, $time, :$scheduler)
    }
    method delay(Supply:D: $time, :$scheduler = $*SCHEDULER) {
        SupplyOperations.delay(self, $time, :$scheduler)
    }
    method migrate(Supply:D: )           { SupplyOperations.migrate(self) }

    proto method classify (|) { * }
    multi method classify(Supply:D: &mapper )  {
        SupplyOperations.classify(self, &mapper);
    }
    multi method classify(Supply:D: %mapper )  {
        SupplyOperations.classify(self, { %mapper{$^a} });
    }
    multi method classify(Supply:D: @mapper )  {
        SupplyOperations.classify(self, { @mapper[$^a] });
    }

    proto method categorize (|) { * }
    multi method categorize(Supply:D: &mapper )  {
        SupplyOperations.classify(self, &mapper, :multi);
    }
    multi method categorize(Supply:D: %mapper )  {
        SupplyOperations.classify(self, { %mapper{$^a} }, :multi);
    }
    multi method categorize(Supply:D: @mapper )  {
        SupplyOperations.classify(self, { @mapper[$^a] }, :multi);
    }

    method act(Supply:D: &actor) {
        self.do(&actor).tap(|%_) # need "do" for serializing callbacks
    }

    method do(Supply:D $self: &side_effect) {
        on -> $res {
            $self => -> \val { side_effect(val); $res.more(val) }
        }
    }

    method uniq(Supply:D $self: :&as, :&with, :$expires) {
        on -> $res {
            $self => do {
                if $expires {
                    if &with and &with !=== &[===] {
                        my @seen;  # really Mu, but doesn't work in settings
                        my Mu $target;
                        &as
                          ?? -> \val {
                              my $now := now;
                              $target = &as(val);
                              my $index =
                                @seen.first-index({&with($target,$_[0])});
                              if $index.defined {
                                  if $now > @seen[$index][1] {  # expired
                                      @seen[$index][1] = $now+$expires;
                                      $res.more(val);
                                  }
                              }
                              else {
                                  @seen.push: [$target, $now+$expires];
                                  $res.more(val);
                              }
                          }
                          !! -> \val {
                              my $now := now;
                              my $index =
                                @seen.first-index({&with(val,$_[0])});
                              if $index.defined {
                                  if $now > @seen[$index][1] {  # expired
                                      @seen[$index][1] = $now+$expires;
                                      $res.more(val);
                                  }
                              }
                              else {
                                  @seen.push: [val, $now+$expires];
                                  $res.more(val);
                              }
                          };
                    }
                    else {
                        my $seen := nqp::hash();
                        my str $target;
                        &as
                          ?? -> \val {
                              my $now := now;
                              $target = nqp::unbox_s(&as(val).WHICH);
                              if !nqp::existskey($seen,$target) ||
                                $now > nqp::atkey($seen,$target) { #expired
                                  $res.more(val);
                                  nqp::bindkey($seen,$target,$now+$expires);
                              }
                          }
                          !! -> \val {
                              my $now := now;
                              $target = nqp::unbox_s(val.WHICH);
                              if !nqp::existskey($seen,$target) ||
                                $now > nqp::atkey($seen,$target) { #expired
                                  $res.more(val);
                                  nqp::bindkey($seen,$target,$now+$expires);
                              }
                          };
                    }
                }
                else { # !$!expires
                    if &with and &with !=== &[===] {
                        my @seen;  # really Mu, but doesn't work in settings
                        my Mu $target;
                        &as
                          ?? -> \val {
                              $target = &as(val);
                              if @seen.first({ &with($target,$_) } ) =:= Nil {
                                  @seen.push($target);
                                  $res.more(val);
                              }
                          }
                          !! -> \val {
                              if @seen.first({ &with(val,$_) } ) =:= Nil {
                                  @seen.push(val);
                                  $res.more(val);
                              }
                          };
                    }
                    else {
                        my $seen := nqp::hash();
                        my str $target;
                        &as
                          ?? -> \val {
                              $target = nqp::unbox_s(&as(val).WHICH);
                              unless nqp::existskey($seen, $target) {
                                  nqp::bindkey($seen, $target, 1);
                                  $res.more(val);
                              }
                          }
                          !! -> \val {
                              $target = nqp::unbox_s(val.WHICH);
                              unless nqp::existskey($seen, $target) {
                                  nqp::bindkey($seen, $target, 1);
                                  $res.more(val);
                              }
                          };
                    }
                }
            }
        }
    }

    method squish(Supply:D $self: :&as, :&with is copy) {
        &with //= &[===];
        on -> $res {
            my @secret;
            $self => do {
                my Mu $last = @secret;
                my Mu $target;
                &as
                  ?? -> \val {
                      $target = &as(val);
                      unless &with($target,$last) {
                          $last = $target;
                          $res.more(val);
                      }
                  }
                  !! -> \val {
                      unless &with(val,$last) {
                          $last = val;
                          $res.more(val);
                      }
                  };
            }
        }
    }

    method rotor(Supply:D $self: $elems? is copy, $overlap? is copy ) {

        $elems   //= 2;
        $overlap //= 1;
        return $self if $elems == 1 and $overlap == 0;  # nothing to do

        on -> $res {
            $self => do {
                my @batched;
                sub flush {
                    $res.more( [@batched] );
                    @batched.splice( 0, +@batched - $overlap );
                }

                {
                    more => -> \val {
                        @batched.push: val;
                        flush if @batched.elems == $elems;
                    },
                    done => {
                        flush if @batched;
                        $res.done;
                    }
                }
            }
        }
    }

    method batch(Supply:D $self: :$elems, :$seconds ) {

        return $self if (!$elems or $elems == 1) and !$seconds;  # nothing to do

        on -> $res {
            $self => do {
                my @batched;
                my $last_time;
                sub flush {
                    $res.more([@batched]);
                    @batched = ();
                }

                {
                    more => do {
                        if $seconds {
                            $last_time = time div $seconds;

                            $elems # and $seconds
                              ??  -> \val {
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
                              }
                              !! -> \val {
                                  my $this_time = time div $seconds;
                                  if $this_time != $last_time {
                                      flush if @batched;
                                      $last_time = $this_time;
                                  }
                                  @batched.push: val;
                              }
                        }
                        else { # just $elems
                            -> \val {
                                @batched.push: val;
                                flush if @batched.elems == $elems;
                            }
                        }
                    },
                    done => {
                        flush if @batched;
                        $res.done;
                    }
                }
            }
        }
    }

    method elems(Supply:D $self: $seconds? ) {

        on -> $res {
            $self => do {
                my $elems = 0;
                my $last_time;
                my $last_elems;

                {
                    more => do {
                        if $seconds {
                            $last_time  = time div $seconds;
                            $last_elems = $elems;
                            -> \val {
                                  $last_elems = ++$elems;
                                  my $this_time = time div $seconds;
                                  if $this_time != $last_time {
                                      $res.more($elems);
                                      $last_time = $this_time;
                                  }
                            }
                        }
                        else {
                            -> \val { $res.more(++$elems) }
                        }
                    },
                    done => {
                        $res.more($elems) if $seconds and $elems != $last_elems;
                        $res.done;
                    }
                }
            }
        }
    }

    method last(Supply:D $self: Int $number = 1) {  # should be Natural
        on -> $res {
            $self => do {
                my @seen;
                {
                    more => $number == 1
                      ?? -> \val { @seen[0] = val }
                      !! -> \val {
                          @seen.shift if +@seen == $number;
                          @seen.push: val;
                      },
                    done => {
                        $res.more($_) for @seen;
                        $res.done;
                    }
                }
            }
        }
    }

    method min(Supply:D $self: &by = &infix:<cmp>) {
        my &cmp = &by.arity == 2 ?? &by !! { by($^a) cmp by($^b) }
        on -> $res {
            $self => do {
                my $min;
                {
                    more => -> \val {
                        if val.defined and !$min.defined || cmp(val,$min) < 0 {
                            $res.more( $min = val );
                        }
                    },
                    done => { $res.done }
                }
            }
        }
    }

    method max(Supply:D $self: &by = &infix:<cmp>) {
        my &cmp = &by.arity == 2 ?? &by !! { by($^a) cmp by($^b) }
        on -> $res {
            $self => do {
                my $max;
                {
                    more => -> \val {
                        if val.defined and !$max.defined || cmp(val,$max) > 0 {
                            $res.more( $max = val );
                        }
                    },
                    done => { $res.done }
                }
            }
        }
    }

    method minmax(Supply:D $self: &by = &infix:<cmp>) {
        my &cmp = &by.arity == 2 ?? &by !! { by($^a) cmp by($^b) }
        on -> $res {
            $self => do {
                my $min;
                my $max;
                {
                    more => -> \val {
                        if val.defined {
                            if !$min.defined {
                                $res.more( Range.new($min = val, $max = val) );
                            }
                            elsif cmp(val,$min) < 0 {
                                $res.more( Range.new( $min = val, $max ) );
                            }
                            elsif cmp(val,$max) > 0 {
                                $res.more( Range.new( $min, $max = val ) );
                            }
                        }
                    },
                    done => { $res.done }
                }
            }
        }
    }

    method reduce(Supply:D $self: &with) {
        on -> $res {
            $self => do {
                my $notfirst;
                my $reduced;
                {
                    more => -> \val {
                        $reduced = $notfirst ?? with($reduced,val) !! val;
                        $res.more($reduced);
                        once $notfirst = True;
                    },
                    done => { $res.done }
                }
            }
        }
    }

    method grab(Supply:D $self: &when_done) {
        on -> $res {
            $self => do {
                my @seen;
                {
                    more => -> \val { @seen.push: val },
                    done => {
                        $res.more($_) for when_done(@seen);
                        $res.done;
                    }
                }
            }
        }
    }

    method reverse(Supply:D:)                 { self.grab( {.reverse} ) }
    method sort(Supply:D: &by = &infix:<cmp>) { self.grab( {.sort(&by)} ) }

    method merge(*@s) {

        @s.unshift(self) if self.DEFINITE;  # add if instance method
        return Supply unless +@s;           # nothing to be done
        return @s[0]  if +@s == 1;          # nothing to be done

        my $dones = 0;
        on -> $res {
            @s => {
                more => -> \val { $res.more(val) },
                done => { $res.done() if ++$dones == +@s }
            },
        }
    }
    
    method zip(*@s, :&with is copy) {

        @s.unshift(self) if self.DEFINITE;  # add if instance method
        return Supply unless +@s;           # nothing to be done
        return @s[0]  if +@s == 1;          # nothing to be done

        my &infix:<op> = &with // &[,]; # hack for [[&with]] parse failure
        my @values = ( [] xx +@s );
        on -> $res {
            @s => -> $val, $index {
                @values[$index].push($val);
                if all(@values) {
                    $res.more( [op] @values>>.shift );
                }
            }
        }
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
        has Bool $!live = False;
        
        submethod BUILD(:&!setup) { }

        method !add_source(
          $source, $lock, $index, :&more, :&done is copy, :&quit is copy
        ) {
            $!live ||= True if $source.live;
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
        
        method live { $!live }
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
