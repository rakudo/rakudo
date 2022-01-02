# continued from src/core.c/Supply-factories.pm6

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

    my class SupplyIterator does Iterator {
        my class ConcQueue is repr('ConcBlockingQueue') { }
        has $!queue;
        has $!exception;

        method TWEAK(:$supply) {
            $!queue     := nqp::create(ConcQueue);
            $!exception := Nil;
            $supply.tap: {
                nqp::push($!queue, $_)
            }, done => -> {
                nqp::push($!queue, ConcQueue); # Sentinel value.
            }, quit => {
                $!exception := $_;
                nqp::push($!queue, ConcQueue); # Sentinel value.
            };
            self
        }

        method pull-one() is raw {
            nqp::eqaddr((my $got := nqp::shift($!queue)),ConcQueue)
              ?? nqp::isconcrete($!exception)
                ?? $!exception.rethrow
                !! IterationEnd
              !! $got
        }

        method push-all(\target --> IterationEnd) {
            nqp::until(
              nqp::eqaddr((my $got := nqp::shift($!queue)),ConcQueue),
              target.push($got)
            );

            $!exception.rethrow
              if nqp::isconcrete($!exception);
        }

        # method is-lazy(--> Bool:D) { ... }
    }

    multi method iterator(Supply:D:) {
        SupplyIterator.new: supply => self
    }
    multi method list(Supply:D:) {
        List.from-iterator: self.iterator
    }
    method Seq(Supply:D:) {
        Seq.new: self.iterator
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

    multi method unique(Supply:D: :&as, :&with, :$expires!) {
        $expires
          ?? supply {
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
                            if nqp::not_i(nqp::existskey($seen,$target)) ||
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
                            if nqp::not_i(nqp::existskey($seen,$target)) ||
                              $now > nqp::atkey($seen,$target) { #expired
                                emit(val);
                                nqp::bindkey($seen,$target,$now+$expires);
                            }
                        }
                    }
                }
            }
          !! self.unique(:&as, :&with)
    }

    multi method unique(Supply:D: :&as, :&with) {
        supply {
            if &with and !(&with === &[===]) {
                my $seen := nqp::create(IterationBuffer);
                my Mu $target;
                if &as {
                    whenever self -> \val {
                        emit(val) unless seen($seen, as(val), &with);
                    }
                }
                else {
                    whenever self -> \val {
                        emit(val) unless seen($seen, val, &with);
                    }
                }
            }
            else {
                my $seen := nqp::hash();
                my $which;
                if &as {
                    whenever self -> \val {
                        $which := as(val).WHICH;
                        unless nqp::existskey($seen, $which) {
                            nqp::bindkey($seen, $which, 1);
                            emit(val);
                        }
                    }
                }
                else {
                    whenever self -> \val {
                        $which := val.WHICH;
                        unless nqp::existskey($seen, $which) {
                            nqp::bindkey($seen, $which, 1);
                            emit(val);
                        }
                    }
                }
            }
        }
    }

    multi method squish(Supply:D:) {
        supply {
            my $last := nqp::null;
            my $which;
            whenever self -> \val {
                if nqp::isnull($last) {
                    emit val;
                    $last := val.WHICH;
                }
                elsif $last ne ($which := val.WHICH) {
                    emit val;
                    $last := $which;
                }
            }
        }
    }
    multi method squish(Supply:D: :&as!, :&with!) {
        supply {
            my $target;
            my $last := nqp::null;
            whenever self -> \val {
                $target := as(val);
                if nqp::isnull($last) {
                    emit val;
                }
                else {
                    emit val unless with($last, $target);
                }
                $last := $target;
            }
        }
    }
    multi method squish(Supply:D: :&as!) {
        supply {
            my $target;
            my $last := nqp::null;
            my $which;
            whenever self -> \val {
                $target := as(val);
                if nqp::isnull($last) {
                    emit val;
                    $last := $target.WHICH;
                }
                elsif $last ne ($which := $target.WHICH) {
                    emit val;
                    $last := $which;
                }
            }
        }
    }
    multi method squish(Supply:D: :&with!) {
        supply {
            my $last := nqp::null;
            whenever self -> \val {
                emit val
                  if nqp::isnull($last)
                  || nqp::not_i(with($last, val));
                $last := val;
            }
        }
    }

    sub seen(IterationBuffer:D \seen, \value, &with) {
        my int $i = -1;
        my int $elems = nqp::elems(seen);
        return 1 if with(value, nqp::atpos(seen,$i))
          while ++$i < $elems;

        # not seen
        nqp::push(seen, value);
        0
    }

    multi method repeated(Supply:D:) {
        supply {
            my $seen := nqp::hash;
            my $which;
            whenever self -> \val {
                nqp::existskey($seen,($which := val.WHICH))
                  ?? emit(val)
                  !! nqp::bindkey($seen,$which,1)
            }
        }
    }
    multi method repeated(Supply:D: :&as!, :&with!) {
        supply {
            my $seen := nqp::create(IterationBuffer);
            whenever self -> \val {
                emit(val) if seen($seen, as(val), &with);
            }
        }
    }
    multi method repeated(Supply:D: :&as!) {
        supply {
            my $seen := nqp::hash;
            my $which;
            whenever self -> \val {
                nqp::existskey($seen,($which := as(val).WHICH))
                  ?? emit(val)
                  !! nqp::bindkey($seen,$which,1)
            }
        }
    }
    multi method repeated(Supply:D: :&with!) {
        supply {
            my $seen := nqp::create(IterationBuffer);
            whenever self -> \val {
                emit(val) if seen($seen, val, &with);
            }
        }
    }

    multi method rotor(Supply:D: Int:D $batch, :$partial) {
        self.rotor(($batch,), :$partial)
    }
    multi method rotor(Supply:D: *@cycle, :$partial) {
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

    method batch(Supply:D:
      Int(Cool) :$elems = 0, :$seconds, :$emit-timed
    --> Supply:D) {
        supply {
            my int $max = $elems >= 0 ?? $elems !! 0;
            my $batched := nqp::list;
            sub flush(--> Nil) {
                emit($batched);
                $batched := nqp::list;
            }
            sub final-flush(--> Nil) {
                emit($batched) if nqp::elems($batched);
            }

            if $seconds {
                if $emit-timed {
                    my $timer = Supply.interval($seconds);

                    whenever $timer -> \tick {
                        flush if nqp::elems($batched);
                        LAST { final-flush; }
                    }

                    if $max > 0 {
                        whenever self -> \val {
                            nqp::push($batched,val);
                            flush if nqp::iseq_i(nqp::elems($batched),$max);
                        }
                    }
                }

                else {   # no emit-timed
                    my int $msecs = ($seconds * 1000).Int;
                    my int $last_time =
                      nqp::div_i(nqp::mul_i(nqp::time,1000000),$msecs);

                    if $max > 0 {
                        whenever self -> \val {
                            my int $this_time =
                              nqp::div_i(nqp::time,nqp::mul_i($msecs,1000000));
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
                    else {            # no max and $seconds
                        whenever self -> \val {
                            my int $this_time =
                              nqp::div_i(nqp::time,nqp::mul_i($msecs,1000000));
                            if $this_time != $last_time {
                                flush if nqp::elems($batched);
                                $last_time = $this_time;
                            }
                            nqp::push($batched,val);
                            LAST { final-flush; }
                        }
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

    proto method lines(|) {*}

    # optional chomping lines from a Supply
    multi method lines(Supply:D: :$chomp! ) {
        $chomp
          ?? self.lines            # need to chomp
          !! supply {              # no chomping wanted
                 my str $str;
                 my int $left;
                 my int $pos;
                 my int $nextpos;

                 whenever self -> str $val {
                     $str = nqp::concat($str,$val);
                     $pos = 0;

                     while ($left = nqp::chars($str) - $pos) > 0 {
                         $nextpos = nqp::findcclass(
                           nqp::const::CCLASS_NEWLINE,$str,$pos,$left);

                         last
                           if $nextpos >= nqp::chars($str)        # no line delimiter
                           or nqp::eqat($str,"\r",$nextpos)       # broken CRLF?
                             && $nextpos == nqp::chars($str) - 1; # yes!

                         emit nqp::p6box_s(nqp::substr($str,$pos,$nextpos - $pos + 1));
                         $pos = $nextpos + 1;
                     }
                     $str = nqp::substr($str,$pos);

                     LAST {
                         emit nqp::p6box_s($str) if nqp::chars($str);
                     }
                 }
             }
    }

    # chomping lines from a Supply
    multi method lines(Supply:D:) {
        supply {
            my str $str;
            my int $pos;
            my int $left;
            my int $nextpos;

            whenever self -> str $val {
                $str = nqp::concat($str,$val);
                $pos = 0;

                while ($left = nqp::chars($str) - $pos) > 0 {
                    $nextpos = nqp::findcclass(
                      nqp::const::CCLASS_NEWLINE,$str,$pos,$left);

                    last
                      if $nextpos >= nqp::chars($str)        # no line delimiter
                      or nqp::eqat($str,"\r",$nextpos)       # broken CRLF?
                        && $nextpos == nqp::chars($str) - 1; # yes!

                    emit nqp::p6box_s(nqp::substr($str,$pos,$nextpos - $pos));
                    $pos = $nextpos + 1;
                }
                $str = nqp::substr($str,$pos);

                LAST {
                    emit nqp::p6box_s(nqp::substr($str,0,
                      nqp::chars($str) - nqp::iscclass(    # skip whitespace at end
                        nqp::const::CCLASS_NEWLINE,$str,nqp::chars($str) - 1)
                    )) if nqp::chars($str);
                }
            }
        }
    }

    method words(Supply:D:) {
        supply {
            my str $str;
            my int $left;
            my int $pos;
            my int $nextpos;

            whenever self -> str $val {
                $str = nqp::concat($str,$val);
                $pos = nqp::findnotcclass(
                  nqp::const::CCLASS_WHITESPACE,$str,0,nqp::chars($str));

                while ($left = nqp::chars($str) - $pos) > 0 {
                    $nextpos = nqp::findcclass(
                      nqp::const::CCLASS_WHITESPACE,$str,$pos,$left);

                    last unless $left = nqp::chars($str) - $nextpos; # broken word

                    emit nqp::p6box_s(nqp::substr($str,$pos,$nextpos - $pos));

                    $pos = nqp::findnotcclass(
                      nqp::const::CCLASS_WHITESPACE,$str,$nextpos,$left);
                }
                $str = nqp::substr($str,$pos);

                LAST {
                    emit nqp::p6box_s($str) if nqp::chars($str);
                }
            }
        }
    }

    multi method elems(Supply:D:) {
        supply {
            my int $elems;
            whenever self { emit ++$elems }
        }
    }
    multi method elems(Supply:D: $seconds ) {
        supply {
            my $last-time := nqp::div_i(nqp::time(),1000000000) div $seconds;
            my $this-time;

            my int $elems;
            my int $last-elems;

            whenever self {
                $last-elems = ++$elems;
                $this-time := nqp::div_i(nqp::time(),1000000000) div $seconds;

                if $this-time != $last-time {
                    emit $elems;
                    $last-time := $this-time;
                }
                LAST emit $elems if $elems != $last-elems;
            }
        }
    }

    multi method head(Supply:D:) {
        supply { whenever self -> \val { emit val; done } }
    }
    multi method head(Supply:D: Callable:D $limit) {
        (my int $lose = -$limit(0)) <= 0
          ?? self
          !! supply {
                 my $values := nqp::list;
                 whenever self -> \val {
                     nqp::push($values,val);
                     LAST {
                         nqp::while(
                           nqp::elems($values) > $lose,
                           (emit nqp::shift($values))
                         );
                     }
                 }
             }
    }
    multi method head(Supply:D: \limit) {
        nqp::istype(limit,Whatever) || limit == Inf
          ?? self
          !! limit <= 0
            ?? supply { }
            !! supply {
                   my int $todo = limit.Int;
                   whenever self -> \val {
                       emit(val);
                       done unless --$todo;
                   }
               }
    }

    multi method tail(Supply:D:) {
        supply {
            my $last;
            whenever self -> \val {
                $last := val;
                LAST emit $last;
            }
        }
    }
    multi method tail(Supply:D: Callable:D $limit) {
        self.skip(-$limit(0))
    }
    multi method tail(Supply:D: \limit) {
        nqp::istype(limit,Whatever) || limit == Inf
          ?? self
          !! limit <= 0
            ?? supply { whenever self -> \val { } }
            !! (my int $size = limit.Int) == 1
              ?? self.tail
              !! supply {
                     my $lastn := nqp::list;
                     my int $index = 0;
                     nqp::setelems($lastn,$size);  # presize list
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

    method min(Supply:D: &by = &infix:<cmp>) {
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

    method max(Supply:D: &by = &infix:<cmp>) {
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

    method minmax(Supply:D: &by = &infix:<cmp>) {
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

    method grab(Supply:D: &when_done) {
        supply {
            my $seen := nqp::create(IterationBuffer);
            whenever self -> \val {
                nqp::push($seen,val);
                LAST {
                    emit($_) for when_done($seen.List);
                }
            }
        }
    }

    method rotate(Supply:D: Int(Cool) $rotate = 1) {

        # potentially ok
        if $rotate > 0 {
            my $rotated := nqp::create(IterationBuffer);
            supply {
                whenever self -> \val {
                    nqp::elems($rotated) < $rotate
                      ?? nqp::push($rotated,val)
                      !! emit(val);

                    LAST {
                        # not enough elems found to rotate, adapt rotation
                        if nqp::elems($rotated) < $rotate {
                            emit($_) for $rotated.List.rotate($rotate);
                        }

                        # produce the rotated values at the end
                        else {
                            emit(nqp::shift($rotated))
                              while nqp::elems($rotated);
                        }
                    }
                }
            }
        }

        # must first grab all
        elsif $rotate < 0 {
            self.grab: *.rotate($rotate)
        }

        # no need to change anything
        else {
            self
        }
    }

    method reverse(Supply:D:)        { self.grab: *.reverse }
    multi method sort(Supply:D:)     { self.grab: *.sort }
    multi method sort(Supply:D: &by) { self.grab: *.sort(&by) }
    multi method collate(Supply:D:)  { self.grab: *.collate }

    method zip(**@s, :&with) {
        @s.unshift(self) if self.DEFINITE;  # add if instance method
        return supply { } unless +@s;       # nothing to be done

        X::Supply::Combinator.new(
           combinator => 'zip'
        ).throw unless Rakudo::Internals.ALL_DEFINED_TYPE(@s,Supply);

        return @s[0]  if +@s == 1;          # nothing to be done

        supply {
            my @values = nqp::create(Array) xx +@s;
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
    multi method throttle(Supply:D:
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
    multi method throttle(Supply:D:
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

# vim: expandtab shiftwidth=4
