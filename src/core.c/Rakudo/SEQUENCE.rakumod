sub SEQUENCE(
  \left, Mu \right, :$exclude_end
--> Iterator:D) is implementation-detail {

    my \righti := (nqp::iscont(right) ?? right !! [right]).iterator;
    my $endpoint := righti.pull-one;
    $endpoint.throw if nqp::istype($endpoint,Failure);
    Any.throw-cannot-be-empty(
      'get sequence endpoint', 'list (use * or :!elems instead?)'
    ) if nqp::eqaddr($endpoint,IterationEnd);

    my $infinite := nqp::istype($endpoint,Whatever) || $endpoint === Inf;
    $endpoint := False if $infinite;

    my $end_code_arity := 0;
    if nqp::istype($endpoint,Code) && nqp::not_i(nqp::istype($endpoint,Regex)) {
        $end_code_arity := $endpoint.arity;
        $end_code_arity := $endpoint.count if $end_code_arity == 0;
        $end_code_arity := -Inf if $end_code_arity == Inf;
    }

    my sub succpred($a,$b) {
        my $cmp := $a cmp $b;
        if nqp::eqaddr($a.WHAT,$b.WHAT) && nqp::eqaddr($b.WHAT,$endpoint.WHAT) {
            $cmp < 0 && nqp::istype($a,Stringy)
                ?? {
                    my $new := .succ;
                    $new after $endpoint || $new.chars > $endpoint.chars
                      ?? (last)
                      !! $new
                }
                !! $cmp < 0
                    ?? {
                        my $new := .succ;
                        $new after $endpoint
                          ?? (last)
                          !! $new
                    }
                    !! $cmp > 0
                        ?? {
                            $_ before $endpoint
                              ?? (last)
                              !! .pred
                        }
                        !! { $_ }
        }
        else {
               $cmp < 0 ?? { .succ }
            !! $cmp > 0 ?? { .pred }
            !!             { $_    }
        }
    }
    my sub unisuccpred($a,$b) {
        my $cmp := $a.ord cmp $b.ord;
           $cmp < 0 ?? { .ord.succ.chr }
        !! $cmp > 0 ?? { .ord.pred.chr }
        !!             { $_            }
    }

    my \gathered = GATHER({
        my \lefti := left.iterator;
        my &producer;
        my int $stop;
        my int $looped;
        my @tail;
        my @end_tail;
        until nqp::eqaddr((my \value := lefti.pull-one),IterationEnd) {
            $looped = 1;
            if nqp::istype(value,Code) { &producer = value; last }
            if $end_code_arity != 0 {
                @end_tail.push(value);
                if @end_tail.elems >= $end_code_arity {
                    @end_tail.shift xx (@end_tail.elems - $end_code_arity)
                        unless $end_code_arity == -Inf;

                    if $endpoint(|@end_tail) {
                        $stop = 1;
                        @tail.push(value) unless $exclude_end;
                        last;
                    }
                }
            }
            elsif $endpoint.ACCEPTS(value) {
                $stop = 1;
                @tail.push(value) unless $exclude_end;
                last;
            }
            @tail.push(value);
        }
        Any.throw-cannot-be-empty(
          'get sequence start value', 'list'
        ) unless $looped;

        if $stop {
            my $ = take $_ for @tail; # don't sink return of take()
        }
        else {
            my $badseq;
            my $a;
            my $b;
            my $c;
            unless &producer {
                my $ = take @tail.shift while @tail.elems > 3; # don't sink return of take()
                $a := @tail[0];
                $b := @tail[1];
                $c := @tail[2];
            }
            if &producer { }
            elsif @tail.grep(Real).elems != @tail.elems {
                if @tail.elems > 1 {
                    &producer = @tail.tail.WHAT === $endpoint.WHAT
                        ?? succpred(@tail.tail, $endpoint)
                        !! succpred(@tail[*-2], @tail.tail);
                }
                elsif nqp::istype($endpoint,Stringy)
                  && nqp::istype($a,Stringy)
                  && nqp::isconcrete($endpoint) {
                    if $a.codes == 1 && $endpoint.codes == 1 {
                        &producer = unisuccpred($a, $endpoint);
                    }
                    elsif $a.codes == $endpoint.codes {
                        my @a = $a.comb;
                        my @e = $endpoint.comb;
                        my @ranges;
                        for flat @a Z @e -> $from, $to {
                            @ranges.push: $($from ... $to);
                        }
                        my $ = .take for flat [X~] @ranges; # don't sink return of take()
                        $stop = 1;
                    }
                    elsif $a lt $endpoint {
                        $stop = 1 if $a gt $endpoint;
                        &producer = {
                            my $new := .succ;
                            $new gt $endpoint || $new.chars > $endpoint.chars
                              ?? (last)
                              !! $new
                        }
                    }
                    else {
                        $stop = 1 if $a lt $endpoint;
                        &producer = {
                            my $new := .pred;
                            $new lt $endpoint
                              ?? (last)
                              !! $new
                        }
                    }
                }
                elsif $infinite or nqp::istype($endpoint, Code) {
                    &producer = *.succ;
                }
                else {
                    &producer = succpred($a,$endpoint);
                }
            }
            elsif @tail.elems == 3 {
                my $ab := $b - $a;
                if $ab == $c - $b {
                    if $ab != 0
                    || nqp::istype($a,Real)
                    && nqp::istype($b,Real)
                    && nqp::istype($c,Real) {
                        if nqp::istype($endpoint, Real)
                          && nqp::not_i(nqp::istype($endpoint,Bool))
                          && nqp::isconcrete($endpoint) {
                            if $ab > 0 {
                                $stop = 1 if $a > $endpoint;
                                &producer = {
                                    my $new := $_ + $ab;
                                    $new > $endpoint
                                      ?? (last)
                                      !! $new
                                }
                            }
                            else {
                                $stop = 1 if $a < $endpoint;
                                &producer = {
                                    my $new := $_ + $ab;
                                    $new < $endpoint
                                      ?? (last)
                                      !! $new
                                }
                            }
                        }
                        else {
                            &producer = { $_ + $ab }
                        }
                    }
                    else {
                        &producer = succpred($b, $c)
                    }
                }
                elsif $a != 0 && $b != 0 && $c != 0 {
                    $ab := $b / $a;
                    if $ab == $c / $b {
                        # XXX TODO: this code likely has a 2 bugs:
                        # 1) It should check Rational, not just Rat
                        # 2) Currently Rats aren't guaranteed to be always
                        #    normalized, so denominator might not be 1, even if
                        #    it could be, if normalized
                        $ab := $ab.Int
                            if nqp::istype($ab, Rat) && $ab.denominator == 1;

                        if nqp::istype($endpoint,Real)
                          && nqp::not_i(nqp::istype($endpoint,Bool))
                          && nqp::isconcrete($endpoint) {
                            if $ab > 0 {
                                if $ab > 1  {
                                    $stop = 1 if $a > $endpoint;
                                    &producer = {
                                        my $new := $_ * $ab;
                                        $new > $endpoint
                                          ?? (last)
                                          !! $new
                                    }
                                }
                                else {
                                    $stop = 1 if $a < $endpoint;
                                    &producer = {
                                        my $new := $_ * $ab;
                                        $new < $endpoint
                                          ?? (last)
                                          !! $new
                                    }
                                }
                            }
                            else {
                                &producer = {
                                    my $new := $_ * $ab;
                                    my $absend := $endpoint.abs;
                                    sign(.abs - $absend)
                                      == -sign($new.abs - $absend)
                                      ?? (last)
                                      !! $new
                                }
                            }
                        }
                        else {
                            &producer = { $_ * $ab }
                        }
                    }
                }
                if &producer {
                    @tail.pop;
                    @tail.pop;
                }
                else {
                    $badseq := "$a,$b,$c";
                }
            }
            elsif @tail.elems == 2 {
                my $ab := $b - $a;
                if $ab != 0 || nqp::istype($a,Real) && nqp::istype($b,Real) {
                    if nqp::istype($endpoint,Real)
                      && nqp::not_i(nqp::istype($endpoint,Bool))
                      && nqp::isconcrete($endpoint) {
                        if $ab > 0 {
                            $stop = 1 if $a > $endpoint;
                            &producer = {
                                my $new := $_ + $ab;
                                $new > $endpoint
                                  ?? (last)
                                  !! $new
                            }
                        }
                        else {
                            $stop = 1 if $a < $endpoint;
                            &producer = {
                                my $new := $_ + $ab;
                                $new < $endpoint
                                  ?? (last)
                                  !! $new
                            }
                        }
                    }
                    else {
                        &producer = { $_ + $ab }
                    }
                }
                else {
                    &producer = succpred($a, $b)
                }
                @tail.pop;
            }
            elsif @tail.elems == 1 {
                if nqp::istype($endpoint,Code)
                  || nqp::not_i(nqp::isconcrete($endpoint)) {
                    &producer = *.succ
                }
                elsif nqp::istype($endpoint,Real)
                  && nqp::not_i(nqp::istype($endpoint,Bool))
                  && nqp::istype($a,Real) {
                    if $a < $endpoint {
                        &producer = {
                            my $new := .succ;
                            $new > $endpoint
                              ?? (last)
                              !! $new
                        }
                    }
                    else {
                        &producer = {
                            my $new := .pred;
                            $new < $endpoint
                              ?? (last)
                              !! $new
                        }
                    }
                }
                else {
                    &producer = *.succ;
                }
            }
            elsif @tail.elems == 0 {
                &producer = {()}
            }

            if $stop { }
            elsif &producer {
                my $ = .take for @tail; # don't sink return of take()
                my $count := &producer.count;

                until $stop {
                    @tail.shift while @tail.elems > $count;
                    my \value = producer(|@tail);

                    if $end_code_arity != 0 {
                        @end_tail.push(value);

                        if @end_tail.elems >= $end_code_arity {
                            @end_tail.shift xx (
                                @end_tail.elems - $end_code_arity
                            ) unless $end_code_arity == -Inf;

                            if $endpoint(|@end_tail) {
                                my $ = value.take unless $exclude_end; # don't sink return of take()
                                $stop = 1;
                            }
                        }
                    }
                    elsif $endpoint.ACCEPTS(value) {
                        my $ = value.take unless $exclude_end; # don't sink return of take()
                        $stop = 1;
                    }

                    if $stop { }
                    else {
                        @tail.push(value);
                        my $ = value.take; # don't sink return of take()
                    }
                }
            }
            elsif $badseq {
                X::Sequence::Deduction.new(:from($badseq)).throw;
            }
            else {
                X::Sequence::Deduction.new.throw;
            }
        }
    });
    $infinite
        ?? (gathered.Slip, Slip.from-iterator(righti)).lazy.iterator
        !! (gathered.Slip, Slip.from-iterator(righti)).iterator
}

# vim: expandtab shiftwidth=4
