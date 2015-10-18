# Anything that can be subscribed to does this role. It provides the basic
# supply management infrastructure, as well as various coercions that
# turn Supply-like things into something else and convenience forms of calls
# to SupplyOperations.

my class SupplyOperations is repr('Uninstantiable') { ... }
my class X::Supply::Combinator is Exception {
    has $.combinator;
    method message() { "Can only use $!combinator to combine defined Supply objects" }
}

my class Tap {
    has &.emit;
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

    method tap(Supply:D:
    &emit = -> $ { }, :&done,:&quit={die $_},:&closing) {
        my $tap = Tap.new(:&emit, :&done, :&quit, :&closing, :supply(self));
        $!tappers_lock.protect({
            @!tappers.push($tap);
            if @!paused -> \todo {
                $tap.emit().($_) for todo;
                @!paused = ();
            }
            $!been_tapped = True;
        });
        $tap
    }

    proto method close(|) { * }
    multi method close(Supply:D:) { self.close($_) for self.tappers }
    multi method close(Supply:D: Tap $t) {
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

    method emit(Supply:D: \msg --> Nil) {
        if self.tappers -> \tappers {
            .emit().(msg) for tappers;
        }
        elsif !$!been_tapped {
            $!tappers_lock.protect({ @!paused.append: msg });
        }
    }

    method done(Supply:D: --> Nil) {
        for self.tappers -> $t {
            my $l = $t.done();
            $l() if $l;
        }
    }

    method quit(Supply:D: $ex --> Nil) {
        for self.tappers -> $t {
            my $f = $t.quit();
            $f($ex) if $f;
        }
    }

    method taps(Supply:D:) { +@!tappers }
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

    method await(Supply:D:) {
        my $l = Lock.new;
        my $p = Promise.new;
        my $t = self.tap( -> \val {},
          done => {
              $l.protect( {
                  if $p.status == Planned {
                      $p.keep(self);
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
        $p
    }

    method wait(Supply:D:) { self.await.result }

    method list(Supply:D:) {
        # Use a Channel to handle any asynchrony.
        self.Channel.list;
    }

    method on-demand(Supply:U: |c)       { SupplyOperations.on-demand(|c) }
    method from-list(Supply:U: |c)       { SupplyOperations.from-list(|c) }
    method interval(Supply:U: |c)        { SupplyOperations.interval(|c) }
    method flat(Supply:D: )              { SupplyOperations.flat(self) }
    method grep(Supply:D: Mu $test)      { SupplyOperations.grep(self, $test) }
    method map(Supply:D: &mapper)        { SupplyOperations.map(self, &mapper) }
    method schedule-on(Supply:D: Scheduler $scheduler) {
        SupplyOperations.schedule-on(self, $scheduler);
    }
    method start(Supply:D: &startee)     { SupplyOperations.start(self, &startee) }
    method stable(Supply:D: $time, :$scheduler = $*SCHEDULER) {
        SupplyOperations.stable(self, $time, :$scheduler);
    }
    method delayed(Supply:D: $time, :$scheduler = $*SCHEDULER) {
        SupplyOperations.delayed(self, $time, :$scheduler)
    }
    method migrate(Supply:D: )           { SupplyOperations.migrate(self) }

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
            $self => -> \val { side_effect(val); $res.emit(val) }
        }
    }

    method unique(Supply:D $self: :&as, :&with, :$expires) {
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
                                @seen.first({&with($target,$_[0])},:k);
                              with $index {
                                  if $now > @seen[$index][1] {  # expired
                                      @seen[$index][1] = $now+$expires;
                                      $res.emit(val);
                                  }
                              }
                              else {
                                  @seen.push: [$target, $now+$expires];
                                  $res.emit(val);
                              }
                          }
                          !! -> \val {
                              my $now := now;
                              my $index =
                                @seen.first({&with(val,$_[0])},:k);
                              with $index {
                                  if $now > @seen[$index][1] {  # expired
                                      @seen[$index][1] = $now+$expires;
                                      $res.emit(val);
                                  }
                              }
                              else {
                                  @seen.push: [val, $now+$expires];
                                  $res.emit(val);
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
                                  $res.emit(val);
                                  nqp::bindkey($seen,$target,$now+$expires);
                              }
                          }
                          !! -> \val {
                              my $now := now;
                              $target = nqp::unbox_s(val.WHICH);
                              if !nqp::existskey($seen,$target) ||
                                $now > nqp::atkey($seen,$target) { #expired
                                  $res.emit(val);
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
                                  $res.emit(val);
                              }
                          }
                          !! -> \val {
                              if @seen.first({ &with(val,$_) } ) =:= Nil {
                                  @seen.push(val);
                                  $res.emit(val);
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
                                  $res.emit(val);
                              }
                          }
                          !! -> \val {
                              $target = nqp::unbox_s(val.WHICH);
                              unless nqp::existskey($seen, $target) {
                                  nqp::bindkey($seen, $target, 1);
                                  $res.emit(val);
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
            $self => do {
                my int $first = 1;
                my Mu $last;
                my Mu $target;
                &as
                  ?? -> \val {
                      $target = &as(val);
                      if $first || !&with($target,$last) {
                          $first = 0;
                          $last  = $target;
                          $res.emit(val);
                      }
                  }
                  !! -> \val {
                      if $first || !&with(val,$last) {
                          $first = 0;
                          $last = val;
                          $res.emit(val);
                      }
                  };
            }
        }
    }

    proto method rotor(|) {*}
    multi method rotor(Supply:D $self: *@cycle, :$partial) {
        my @c := @cycle.is-lazy ?? @cycle !! (@cycle xx *).flat.cache;

        on -> $res {
            $self => do {
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
                    $res.emit( [@batched.splice(0, +@batched, @batched[* + $gap .. *]),] );
                    $skip = $to-skip;
                }

                {
                    emit => -> \val {
                        @batched.append: val unless $skip && $skip--;
                        if @batched.elems == $elems {
                            flush;
                            next-batch;
                        }
                    },
                    done => {
                        flush if @batched and $partial;
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
                    $res.emit(([@batched],));
                    @batched = ();
                }

                {
                    emit => do {
                        if $seconds {
                            $last_time = time div $seconds;

                            $elems # and $seconds
                              ??  -> \val {
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
                              }
                              !! -> \val {
                                  my $this_time = time div $seconds;
                                  if $this_time != $last_time {
                                      flush if @batched;
                                      $last_time = $this_time;
                                  }
                                  @batched.append: val;
                              }
                        }
                        else { # just $elems
                            -> \val {
                                @batched.append: val;
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

    method lines(Supply:D $self: :$chomp = True ) {

        on -> $res {
            $self => do {
                my str $str;
                my int $chars;
                my int $left;
                my int $pos;
                my int $nextpos;
                my int $found;
                my int $cr;
                my int $crlf;

                {
                    emit => -> \val {
                        $str   = $str ~ nqp::unbox_s(val);
                        $chars = nqp::chars($str);
                        $pos   = 0;

                        while ($left = $chars - $pos) > 0 {
                            $nextpos = nqp::findcclass(
                              nqp::const::CCLASS_NEWLINE, $str, $pos, $left
                            );

                            # no trailing line delimiter, so go buffer
                            last unless nqp::iscclass(
                              nqp::const::CCLASS_NEWLINE, $str, $nextpos
                            );

                            # potentially broken CRLF, so go buffer
                            $cr = nqp::ordat($str, $nextpos) == 13;    # CR
                            last if $cr == 1 and $nextpos + 1 == $chars;

                            $crlf = $cr
                              && nqp::ordat($str, $nextpos + 1) == 10; # LF

                            if $chomp {
                                $res.emit( ($found = $nextpos - $pos)
                                  ?? nqp::box_s(
                                       nqp::substr($str, $pos, $found), Str)
                                  !! ''
                                );
                                $pos = $nextpos + 1 + $crlf;
                            }
                            else {
                                $found = $nextpos - $pos + 1 + $crlf;
                                $res.emit( nqp::box_s(
                                  nqp::substr($str, $pos, $found), Str)
                                );
                                $pos = $pos + $found;
                            }
                        }
                        $str = $pos < $chars
                          ?? nqp::substr($str,$pos)
                          !! '';
                    },
                    done => {
                        if $str {
                            $chars = nqp::chars($str);
                            $res.emit( $chomp
                              && nqp::ordat($str, $chars - 1) == 13    # CR
                              ?? nqp::box_s(nqp::substr($str,0,$chars - 1),Str)
                              !! nqp::box_s($str, Str)
                            );
                        }
                        $res.done;
                    }
                }
            }
        }
    }

    method words(Supply:D $self:) {

        on -> $res {
            $self => do {
                my str $str;
                my int $chars;
                my int $left;
                my int $pos;
                my int $nextpos;
                my int $found;
                my int $cr;
                my int $crlf;

                {
                    emit => -> \val {
                        $str   = $str ~ nqp::unbox_s(val);
                        $chars = nqp::chars($str);
                        $pos   = nqp::findnotcclass(
                          nqp::const::CCLASS_WHITESPACE, $str, 0, $chars);

                        while ($left = $chars - $pos) > 0 {
                            $nextpos = nqp::findcclass(
                              nqp::const::CCLASS_WHITESPACE, $str, $pos, $left
                            );

                            last unless $left = $chars - $nextpos; # broken word

                            $res.emit( nqp::box_s(
                              nqp::substr( $str, $pos, $nextpos - $pos ), Str)
                            );

                            $pos = nqp::findnotcclass(
                              nqp::const::CCLASS_WHITESPACE,$str,$nextpos,$left);
                        }
                        $str = $pos < $chars
                          ?? nqp::substr($str,$pos)
                          !! '';
                    },
                    done => {
                        $res.emit( nqp::box_s($str, Str) ) if $str;
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
                    emit => do {
                        if $seconds {
                            $last_time  = time div $seconds;
                            $last_elems = $elems;
                            -> \val {
                                  $last_elems = ++$elems;
                                  my $this_time = time div $seconds;
                                  if $this_time != $last_time {
                                      $res.emit($elems);
                                      $last_time = $this_time;
                                  }
                            }
                        }
                        else {
                            -> \val { $res.emit(++$elems) }
                        }
                    },
                    done => {
                        $res.emit($elems) if $seconds and $elems != $last_elems;
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
                    emit => $number == 1
                      ?? -> \val { @seen[0] = val }
                      !! -> \val {
                          @seen.shift if +@seen == $number;
                          @seen.append: val;
                      },
                    done => {
                        $res.emit($_) for @seen;
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
                    emit => -> \val {
                        if val.defined and !$min.defined || cmp(val,$min) < 0 {
                            $res.emit( $min = val );
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
                    emit => -> \val {
                        if val.defined and !$max.defined || cmp(val,$max) > 0 {
                            $res.emit( $max = val );
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
                    emit => -> \val {
                        if val.defined {
                            if !$min.defined {
                                $res.emit( (Range.new($min = val, $max = val),) );
                            }
                            elsif cmp(val,$min) < 0 {
                                $res.emit( (Range.new( $min = val, $max ),) );
                            }
                            elsif cmp(val,$max) > 0 {
                                $res.emit( (Range.new( $min, $max = val ),) );
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
                    emit => -> \val {
                        $reduced = $notfirst ?? with($reduced,val) !! val;
                        $res.emit($reduced);
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
                    emit => -> \val { @seen.append: val },
                    done => {
                        $res.emit($_) for when_done(@seen);
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

        X::Supply::Combinator.new(
           combinator => 'merge'
        ).throw if NOT_ALL_DEFINED_TYPE(@s,Supply);

        return @s[0]  if +@s == 1;          # nothing to be done

        my $dones = 0;
        on -> $res {
            @s => {
                emit => -> \val { $res.emit(val) },
                done => { $res.done() if ++$dones == +@s }
            },
        }
    }

    method zip(**@s, :&with) {
        @s.unshift(self) if self.DEFINITE;  # add if instance method
        return Supply unless +@s;           # nothing to be done

        X::Supply::Combinator.new(
           combinator => 'zip'
        ).throw if NOT_ALL_DEFINED_TYPE(@s,Supply);

        return @s[0]  if +@s == 1;          # nothing to be done

        my @values = [] xx +@s;
        on -> $res {
            @s => &with
              ?? -> $val, $index {
                  @values[$index].push($val);
                  $res.emit( [[&with]] @values.map(*.shift) ) if all(@values);
              }
              !! -> $val, $index {
                  @values[$index].push($val);
                  $res.emit( $(@values.map(*.shift).list) ) if all(@values);
              }
        }
    }

    method zip-latest(**@s, :&with, :$initial ) {
        @s.unshift(self) if self.DEFINITE;  # add if instance method
        return Supply unless +@s;           # nothing to do.

        X::Supply::Combinator.new(
           combinator => 'zip-latest'
        ).throw if NOT_ALL_DEFINED_TYPE(@s,Supply);

        return @s[0] if +@s == 1;           # nothing to do.

        my @values;

        my $uninitialised = +@s; # how many supplies have yet to emit until we
                                 # can start emitting, too?

        if $initial {
            @values = @$initial;
            $uninitialised = 0 max $uninitialised - @$initial;
        }

        my $dones = 0;

        on -> $res {
            @s => do {
                {
                emit => &with
                  ?? -> $val, $index {
                      --$uninitialised
                        if $uninitialised > 0 && not @values.EXISTS-POS($index);
                      @values[$index] = $val;
                      $res.emit( [[&with]] @values ) unless $uninitialised;
                  }
                  !! -> $val, $index {
                      --$uninitialised
                        if $uninitialised > 0 && not @values.EXISTS-POS($index);
                      @values[$index] = $val;
                      $res.emit( @values.List.item ) unless $uninitialised;
                  },
                done => { $res.done() if ++$dones == +@s }
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
    ) {
        my $timer = Supply.interval($seconds,$delay,:$scheduler);
        my @buffer;
        my int $limit   = $elems;
        my int $allowed = $limit;
        my int $emitted;
        my int $bled;
        sub emit-status($id) {
           $status.emit(
             { :$allowed, :$bled, :buffered(+@buffer),
               :$emitted, :$id,   :$limit } );
        }
        on -> $res {
            $timer => { 
                emit => -> \tick {
                    if +@buffer -> \buffered {
                        my int $todo = buffered > $limit ?? $limit !! buffered;
                        $res.emit(@buffer.shift) for ^$todo;
                        $emitted = $emitted + $todo;
                        $allowed = $limit   - $todo;
                    }
                    else {
                        $allowed = $limit;
                    }
                },
            },
            $self => {
                emit => -> \val {
                    if $allowed {
                        $res.emit(val);
                        $emitted = $emitted + 1;
                        $allowed = $allowed - 1;
                    }
                    else {
                        @buffer.push(val);
                    }
                },
                done => {
                    $res.done;  # also stops the timer ??
                    $control.done if $control;
                    $status.done  if $status;
                    if $status {
                        emit-status("done");
                        $status.done;
                    }
                    if $bleed && @buffer {
                        $bleed.emit(@buffer.shift) while @buffer;
                        $bleed.done;
                    }
                },
            },
            $control
              ?? ($control => {
                   emit => -> \val {
                       my $type  = val.key;
                       my $value = val.value;

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
                   },
                 })
              !! |()
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
    ) {
        sleep $delay if $delay;
        my @buffer;
        my int $limit   = $elems;
        my int $allowed = $limit;
        my int $running;
        my int $emitted;
        my int $bled;
        my int $done;
        my $ready = Supply.new;
        sub start-process(\val) {
            my $p = Promise.start( $process, :$scheduler, val );
            $running = $running + 1;
            $allowed = $allowed - 1;
            $p.then: { $ready.emit($p) };
        }
        sub emit-status($id) {
           $status.emit(
             { :$allowed, :$bled, :buffered(+@buffer),
               :$emitted, :$id,   :$limit, :$running } );
        }
        on -> $res {
            $self => {
                emit => -> \val {
                    $allowed > 0 ?? start-process(val) !! @buffer.push(val);
                },
                done => {
                    $done = 1;
                },
            },
            $ready => {  # when a process is ready
                emit => -> \val {
                    $running = $running - 1;
                    $allowed = $allowed + 1;
                    $res.emit(val);
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
                        $res.done;
                    }
                },
            },
            $control
              ?? ($control => {
                   emit => -> \val {
                       my $type  = val.key;
                       my $value = val.value;

                       if $type eq 'limit' {
                           $allowed = $allowed + $value - $limit;
                           $limit   = $value;
                       }
                       elsif $type eq 'bleed' && $bleed {
                           my int $todo = $value min +@buffer;
                           $bleed.emit(@buffer.shift) for ^$todo;
                           $bled = $bled + $todo;
                       }
                       elsif $type eq 'status' && $status {
                           emit-status($value);
                       }
                   },
                 })
              !! |()
        }
    }
}

# The on meta-combinator provides a mechanism for implementing thread-safe
# combinators on Supplies. It subscribes to a bunch of sources, but will
# only let one of the specified callbacks to handle their emit/done/quit run
# at a time. A little bit actor-like.
my class X::Supply::On::BadSetup is Exception {
    method message() {
        "on requires a callable that returns a list of pairs with Supply keys"
    }
}
my class X::Supply::On::NoEmit is Exception {
    method message() {
        "on requires that emit be specified for each supply"
    }
}
sub on(&setup) {
    my class OnSupply does Supply {
        has &!setup;
        has Bool $!live = False;

        submethod BUILD(:&!setup) { }

        method !add_source(
          $source, $lock, $index, :&done is copy, :&quit is copy,
          :&emit
        ) {
            $!live ||= True if $source.live;
            &emit // X::Supply::On::NoEmit.new.throw;
            &done //= { self.done };
            &quit //= -> $ex { self.quit($ex) };

            my &tap_emit = &emit.arity == 2
              ?? -> \val {
                  $lock.protect({ emit(val,$index) });
                  CATCH { default { self.quit($_) } }
              }
              !!  -> \val {
                  $lock.protect({ emit(val) });
                  CATCH { default { self.quit($_) } }
              };

            my &tap_done = &done.arity == 1
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

            $source.tap( &tap_emit, done => &tap_done, quit => &tap_quit );
        }

        method live { $!live }
        method tap(|c) {
            my @to_close;
            my $sub = self.Supply::tap( |c, closing => {.close for @to_close});
            my @tappers = &!setup(self);
            my $lock    = Lock.new;

            sub add ($source, $what, $index?) {
                unless nqp::istype($source,Supply) {
                    X::Supply::On::BadSetup.new.throw;
                }
                given $what {
                    when Map {
                        @to_close.append(self!add_source($source, $lock, $index, |$what));
                    }
                    when Callable {
                        @to_close.append(self!add_source($source, $lock, $index, emit => $what));
                    }
                    default {
                        X::Supply::On::BadSetup.new.throw;
                    }
                }
            }

            for @tappers -> $tap {
                unless nqp::istype($tap,Pair) {
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

        method emit(\msg --> Nil) {
            for self.tappers {
                .emit().(msg)
            }
        }

        method done( --> Nil) {
            for self.tappers {
                if .done -> $l { $l() }
            }
        }

        method quit($ex --> Nil) {
            for self.tappers {
                if .quit -> $t { $t($ex) }
            }
        }
    }

    OnSupply.new(:&setup)
}

sub SUPPLY(&block) {
    my class SupplyBlockState {
        has $.sub;
        has $.lock;
        has $.active is rw;
        has %.active-taps;
    }

    class :: does Supply {
        has &!block;

        submethod BUILD(:&!block) { }

        method tap(|c) {
            my $state = SupplyBlockState.new(
                sub => self.Supply::tap(|c, closing => { ; }),
                lock => Lock.new,
                active => 1);
            self!run-supply-code(&!block, $state);
            self!deactivate-one($state);
            $state.sub
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
                            $state.sub.quit().(ex) if $state.sub.quit;
                            $state.active = 0;
                            self!teardown($state);
                        }
                    });
                $state.active-taps{nqp::objectid($tap)} = $tap;
            }

            my $emitter = {
                my \ex := nqp::exception();
                $state.sub.emit().(nqp::getpayload(ex)) if $state.sub.emit;
                nqp::resume(ex)
            }
            my $done = {
                $state.sub.done().() if $state.sub.done;
                $state.active = 0;
                self!teardown($state);
            }
            my $catch = {
                my \ex = EXCEPTION(nqp::exception());
                $state.sub.quit().(ex) if $state.sub.quit;
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
                    $state.sub.done().() if $state.sub.done;
                    self!teardown($state);
                }
            });
        }

        method !teardown($state) {
            .close for $state.active-taps.values;
            $state.active-taps = ();
        }
    }.new(:&block)
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
