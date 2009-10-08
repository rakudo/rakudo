# operators defined in the setting

multi sub infix:<...> (@lhs, @rhs) {
    if @rhs == 2 && @rhs[0] ~~ Code {
        &infix:<...>(@lhs, @rhs[0], :limit(@rhs[1]));
    } else {
        die "don't know how to handle a right-hand side of"
            ~ @rhs.perl
            ~ "in series operator";
    }
}

multi sub infix:<...>($lhs, @rhs) {
    my @a = $lhs;
    &infix:<...>(@a, @rhs);
}

multi sub infix:<...>($lhs, Code $generator) {
    my @a = $lhs;
    &infix:<...>(@a, $generator);
}

multi sub infix:<...> (@lhs, Code $generator, :$limit) {
    my $c = $generator.count;
    if $c > @lhs {
        fail 'the closure wants more parameters than given on the LHS';
    }
    my @result = @lhs;
    my @r;
    my $argument-indexes;
    # WhateverCode objects don't have a signature yet (RT #69362),
    # and we can't simply use a try { ... } block because its result
    # throws a "Null PMC access in get_bool()" when used in boolean context.
    # we have to use an ugly special case here.
    # and we can't even used !~~ for that (RT #69364)
    if !$generator.^isa(WhateverCode) and any( $generator.signature.params>>.slurpy ) {
        $argument-indexes = 0..*-1;
    } else {
        $argument-indexes = *-$c .. *-1;
    }

    # XXX work around http://rt.perl.org/rt3/Ticket/Display.html?id=66824
    # this is a bit ugly.. since @a[1..1] returns a single item and not
    # an array, |@result[$one-item-range] throws the error
    # "argument doesn't array"
    my $comp;
    if defined($limit) {
        $comp = @lhs[*-1] cmp $limit;
    }

    while @r = $generator(|@(@result[$argument-indexes])) {
        if (defined($limit)) {
            if (@r[*-1] cmp $limit) == 0 {
                @result.push: @r;
                last;
            } elsif (@r[*-1] cmp $limit) != $comp {
                last;
            }
        }

        @result.push: @r;
    }
    @result;
}

# the magic one that handles stuff like
# 'a' ... 'z' and 'z' ... 'a'
multi sub infix:<...>($lhs, $rhs where { !($_ ~~ Code|Whatever) }) {
    gather {
        take $lhs;
        if ($lhs cmp $rhs) == 1 {
            my $x = $lhs;
            # since my $a = 'a'; $a-- gives
            # "Decrement out of range" we can't easily
            # decrement over our target, which is why the
            # case of going backwards is slighly more complicated
            # than going forward
            while (--$x cmp $rhs) == 1 {
                # need to make a fresh copy here because of RT #62178
                my $y = $x;
                take $y;
            }
            take $x if ($x cmp $rhs) == 0;
        } elsif ($lhs cmp $rhs) == -1 {
            my $x = $lhs;
            while (++$x cmp $rhs) <= 0 {
                my $y = $x;
                take $y;
            }
        }
    }
}

multi sub infix:<eqv> (Num $a, Num $b) { $a === $b }
multi sub infix:<eqv> (Str $a, Str $b) { $a === $b }
multi sub infix:<eqv> (Code $a, Code $b) { $a === $b }
multi sub infix:<eqv> (Rat $a, Rat $b) {
    $a.numerator === $b.numerator && $a.denominator == $b.denominator
};
multi sub infix:<eqv> (Positional $a, Positional $b) {
    return Bool::False unless $a.WHAT === $b.WHAT;
    return Bool::False unless $a.elems == $b.elems;
    for @($a) Z @($b) -> $x, $y {
        return Bool::False unless $x eqv $y;
    }
    Bool::True
}

multi sub infix:<eqv>(Pair $a, Pair $b) {
    $a.key eqv $b.key && $a.value eqv $b.value;
}

multi sub infix:<eqv>(Mapping $a, Mapping $b) {
    return Bool::False if +$a != +$b;
    for $a.kv -> $k, $v {
        return Bool::False unless $b.exists($k);
        return Bool::False unless $b.{$k} eqv $v;
    }
    return Bool::True;
}

multi sub infix:<eqv>(Failure $a, Failure $b) {
    # do we have different values of undef yet?
    # if so, how do I detect them?
    Bool::True;
}

multi sub infix:<eqv> ($a, $b) {
    return Bool::False unless $a.WHAT === $b.WHAT;
    return Bool::True  if     $a      === $b;
    die "infix:<eqv> is only implemented for certain special cases yet";
}

multi sub infix:<minmax>(@a, @b) {
    (@a[0] min @b[0], @a[1] max @b[1]);
}

multi sub infix:<leg>($a, $b) {
    ~$a cmp ~$b;
}

sub prefix:<[//]>(*@a) {
    for @a -> $item {
        $item // next;
        return $item;
    }
    return ();
}

sub prefix:<[||]>(*@a) {
    for @a -> $item {
        $item || next;
        return $item;
    }
    return ();
}

sub infix:<!%>($a, $b) { ! ($a % $b) }


multi sub infix:<+>($a, $b) {
    Q:PIR {
        $P0 = find_lex '$a'
        $N0 = $P0
        $P1 = find_lex '$b'
        $N1 = $P1
        $N2 = $N0 + $N1
        %r = box $N2
    }
}

multi sub infix:<->($a, $b) {
    Q:PIR {
        $P0 = find_lex '$a'
        $N0 = $P0
        $P1 = find_lex '$b'
        $N1 = $P1
        $N2 = $N0 - $N1
        %r = box $N2
    }
}

multi sub infix:<*>($a, $b) {
    Q:PIR {
        $P0 = find_lex '$a'
        $N0 = $P0
        $P1 = find_lex '$b'
        $N1 = $P1
        $N2 = $N0 * $N1
        %r = box $N2
    }
}

multi sub infix:</>($a, $b) {
    Q:PIR {
        $P0 = find_lex '$a'
        $N0 = $P0
        $P1 = find_lex '$b'
        $N1 = $P1
        $N2 = $N0 / $N1
        %r = box $N2
    }
}

multi sub infix:<%>($a, $b) {
    Q:PIR {
        $P0 = find_lex '$a'
        $N0 = $P0
        $P1 = find_lex '$b'
        $N1 = $P1
        $N2 = mod $N0, $N1
        %r = box $N2
    }
}

multi sub infix:<**>($a, $b) {
    Q:PIR {
        $P0 = find_lex '$a'
        $N0 = $P0
        $P1 = find_lex '$b'
        $N1 = $P1
        $N2 = pow $N0, $N1
        %r = box $N2
    }
}

multi sub prefix:<->($a) {
    Q:PIR {
        $P0 = find_lex '$a'
        $N0 = $P0
        $N0 = neg $N0
        %r = box $N0
    }
}


multi sub prefix:<~>(Object $a) {
    Q:PIR {
        $P0 = find_lex '$a'
        $S0 = $P0
        %r = new ['Str']
        assign %r, $S0
    }
}


multi sub prefix:<~>(Multi $a) { $a.name }

multi sub infix:<!=>($a, $b)  { !($a == $b) }
multi sub infix:<!==>($a, $b) { !($a == $b) }
multi sub infix:<ne>($a, $b)  { !($a eq $b) }
multi sub infix:<!eq>($a, $b) { !($a eq $b) }

multi sub infix:<< < >>($a, $b) {
    ? Q:PIR {
        $P0 = find_lex '$a'
        $N0 = $P0
        $P1 = find_lex '$b'
        $N1 = $P1
        $I0 = islt $N0, $N1
        %r = box $I0
    }
}

multi sub infix:<< > >>($a, $b) {
    ? Q:PIR {
        $P0 = find_lex '$a'
        $N0 = $P0
        $P1 = find_lex '$b'
        $N1 = $P1
        $I0 = isgt $N0, $N1
        %r = box $I0
    }
}

multi sub infix:<< <= >>($a, $b) {
    ? Q:PIR {
        $P0 = find_lex '$a'
        $N0 = $P0
        $P1 = find_lex '$b'
        $N1 = $P1
        $I0 = isle $N0, $N1
        %r = box $I0
    }
}

multi sub infix:<< >= >>($a, $b) {
    ? Q:PIR {
        $P0 = find_lex '$a'
        $N0 = $P0
        $P1 = find_lex '$b'
        $N1 = $P1
        $I0 = isge $N0, $N1
        %r = box $I0
    }
}

multi sub infix:<< < >>(Whatever $a, $b) {
    Q:PIR {
        $P0 = find_lex '$a'
        $P1 = find_lex '$b'
        .tailcall 'WhateverCodeX'('infix:<', $P0, $P1)
    }
}

multi sub infix:<< < >>($a, Whatever $b) {
    Q:PIR {
        $P0 = find_lex '$a'
        $P1 = find_lex '$b'
        .tailcall 'WhateverCodeX'('infix:<', $P0, $P1)
    }
}

multi sub infix:<< > >>(Whatever $a, $b) {
    Q:PIR {
        $P0 = find_lex '$a'
        $P1 = find_lex '$b'
        .tailcall 'WhateverCodeX'('infix:>', $P0, $P1)
    }
}

multi sub infix:<< > >>($a, Whatever $b) {
    Q:PIR {
        $P0 = find_lex '$a'
        $P1 = find_lex '$b'
        .tailcall 'WhateverCodeX'('infix:>', $P0, $P1)
    }
}

multi sub infix:<< <= >>(Whatever $a, $b) {
    Q:PIR {
        $P0 = find_lex '$a'
        $P1 = find_lex '$b'
        .tailcall 'WhateverCodeX'('infix:<=', $P0, $P1)
    }
}

multi sub infix:<< <= >>($a, Whatever $b) {
    Q:PIR {
        $P0 = find_lex '$a'
        $P1 = find_lex '$b'
        .tailcall 'WhateverCodeX'('infix:<=', $P0, $P1)
    }
}

multi sub infix:<< >= >>(Whatever $a, $b) {
    Q:PIR {
        $P0 = find_lex '$a'
        $P1 = find_lex '$b'
        .tailcall 'WhateverCodeX'('infix:>=', $P0, $P1)
    }
}

multi sub infix:<< >= >>($a, Whatever $b) {
    Q:PIR {
        $P0 = find_lex '$a'
        $P1 = find_lex '$b'
        .tailcall 'WhateverCodeX'('infix:>=', $P0, $P1)
    }
}

# vim: ft=perl6
