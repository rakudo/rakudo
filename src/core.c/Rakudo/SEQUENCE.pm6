class Rakudo::SEQUENCE {

    method iterator(\left, Mu \right, :$exclude_end --> Iterator:D) {

    my \righti := (nqp::iscont(right) ?? right !! [right]).iterator;
    my $endpoint := righti.pull-one;
    $endpoint.throw if nqp::istype($endpoint,Failure);
    X::Cannot::Empty.new(
        :action('get sequence endpoint'),
        :what('list (use * or :!elems instead?)'),
    ).throw if nqp::eqaddr($endpoint,IterationEnd);

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
                    last if $new       after $endpoint
                         or $new.chars >     $endpoint.chars;
                    $new;
                }
                !! $cmp < 0
                    ?? {
                        my $new := .succ;
                        last if $new after $endpoint;
                        $new;
                    }
                    !! $cmp > 0
                        ?? {
                            my $new := .pred;
                            last if $_ before $endpoint;
                            $new;
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
        my $stop;
        my $looped;
        my @tail;
        my @end_tail;
        while !((my \value := lefti.pull-one) =:= IterationEnd) {
            $looped = True;
            if nqp::istype(value,Code) { &producer = value; last }
            if $end_code_arity != 0 {
                @end_tail.push(value);
                if +@end_tail >= $end_code_arity {
                    @end_tail.shift xx (@end_tail.elems - $end_code_arity)
                        unless $end_code_arity ~~ -Inf;

                    if $endpoint(|@end_tail) {
                        $stop = 1;
                        @tail.push(value) unless $exclude_end;
                        last;
                    }
                }
            }
            elsif value ~~ $endpoint {
                $stop = 1;
                @tail.push(value) unless $exclude_end;
                last;
            }
            @tail.push(value);
        }
        X::Cannot::Empty.new(
            :action('get sequence start value'), :what('list')
        ).throw unless $looped;

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
                $a = @tail[0];
                $b = @tail[1];
                $c = @tail[2];
            }
            if &producer { }
            elsif @tail.grep(Real).elems != @tail.elems {
                if @tail.elems > 1 {
                    &producer = @tail.tail.WHAT === $endpoint.WHAT
                        ?? succpred(@tail.tail, $endpoint)
                        !! succpred(@tail[*-2], @tail.tail);
                }
                elsif nqp::istype($endpoint, Stringy)
                  and nqp::istype($a, Stringy)
                  and nqp::isconcrete($endpoint) {
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
                        &producer = -> $x {
                            my $new = $x.succ;
                            last if $new       gt $endpoint
                                 or $new.chars >  $endpoint.chars;
                            $new;
                        }
                    }
                    else {
                        $stop = 1 if $a lt $endpoint;
                        &producer = -> $x {
                            my $new = $x.pred;
                            last if $new lt $endpoint;
                            $new;
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
                my $ab = $b - $a;
                if $ab == $c - $b {
                    if $ab != 0
                    || nqp::istype($a,Real)
                    && nqp::istype($b,Real)
                    && nqp::istype($c,Real) {
                        if      nqp::istype($endpoint, Real)
                        and not nqp::istype($endpoint, Bool)
                        and     nqp::isconcrete($endpoint) {
                            if $ab > 0 {
                                $stop = 1 if $a > $endpoint;
                                &producer = -> $x {
                                    my $new = $x + $ab;
                                    last if $new > $endpoint;
                                    $new;
                                }
                            }
                            else {
                                $stop = 1 if $a < $endpoint;
                                &producer = -> $x {
                                    my $new = $x + $ab;
                                    last if $new < $endpoint;
                                    $new;
                                }
                            }
                        }
                        else {
                            &producer = { $^x + $ab }
                        }
                    }
                    else {
                        &producer = succpred($b, $c)
                    }
                }
                elsif $a != 0 && $b != 0 && $c != 0 {
                    $ab = $b / $a;
                    if $ab == $c / $b {
                        # XXX TODO: this code likely has a 2 bugs:
                        # 1) It should check Rational, not just Rat
                        # 2) Currently Rats aren't guaranteed to be always
                        #    normalized, so denominator might not be 1, even if
                        #    it could be, if normalized
                        $ab = $ab.Int
                            if nqp::istype($ab, Rat) && $ab.denominator == 1;

                        if      nqp::istype($endpoint, Real)
                        and not nqp::istype($endpoint, Bool)
                        and     nqp::isconcrete($endpoint) {
                            if $ab > 0 {
                                if $ab > 1  {
                                    $stop = 1 if $a > $endpoint;
                                    &producer = -> $x {
                                        my $new = $x * $ab;
                                        last if $new > $endpoint;
                                        $new;
                                    }
                                }
                                else {
                                    $stop = 1 if $a < $endpoint;
                                    &producer = -> $x {
                                        my $new = $x * $ab;
                                        last if $new < $endpoint;
                                        $new;
                                    }
                                }
                            }
                            else {
                                &producer = -> $x {
                                    my $new = $x * $ab;
                                    my $absend = $endpoint.abs;
                                    last if sign(  $x.abs - $absend)
                                        == -sign($new.abs - $absend);
                                    $new;
                                }
                            }
                        }
                        else {
                            &producer = { $^x * $ab }
                        }
                    }
                }
                if &producer {
                    @tail.pop;
                    @tail.pop;
                }
                else {
                    $badseq = "$a,$b,$c" unless &producer;
                }
            }
            elsif @tail.elems == 2 {
                my $ab = $b - $a;
                if $ab != 0 || nqp::istype($a,Real) && nqp::istype($b,Real) {
                    if      nqp::istype($endpoint, Real)
                    and not nqp::istype($endpoint, Bool)
                    and     nqp::isconcrete($endpoint) {
                        if $ab > 0 {
                            $stop = 1 if $a > $endpoint;
                            &producer = -> $x {
                                my $new = $x + $ab;
                                last if $new > $endpoint;
                                $new;
                            }
                        }
                        else {
                            $stop = 1 if $a < $endpoint;
                            &producer = -> $x {
                                my $new = $x + $ab;
                                last if $new < $endpoint;
                                $new;
                            }
                        }
                    }
                    else {
                        &producer = { $^x + $ab }
                    }
                }
                else {
                    &producer = succpred($a, $b)
                }
                @tail.pop;
            }
            elsif @tail.elems == 1 {
                if     nqp::istype($endpoint,Code)
                or not nqp::isconcrete($endpoint) {
                    &producer = { $^x.succ }
                }
                elsif   nqp::istype($endpoint, Real)
                and not nqp::istype($endpoint, Bool)
                and     nqp::istype($a, Real) {
                    if $a < $endpoint {
                        &producer = -> $x {
                            my $new = $x.succ;
                            last if $new > $endpoint;
                            $new;
                        }
                    }
                    else {
                        &producer = -> $x {
                            my $new = $x.pred;
                            last if $new < $endpoint;
                            $new;
                        }
                    }
                }
                else {
                    &producer = { $^x.succ }
                }
            }
            elsif @tail.elems == 0 {
                &producer = {()}
            }

            if $stop { }
            elsif &producer {
                my $ = .take for @tail; # don't sink return of take()
                my $count = &producer.count;

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
                    elsif value ~~ $endpoint {
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

}

# vim: ft=perl6 expandtab sw=4
