# operators defined in the setting

multi sub infix:<...> (@lhs, Code $generator) {
    my $c = $generator.count;
    if $c > @lhs {
        fail 'the closure wants more parameters than given on the LHS';
    }
    my @result = @lhs;
    my @r;
    if ?any( $generator.signature.params>>.<slurpy> ) {
        while @r = $generator(|@result) {
            @result.push: @r;
        }
    } else {
        # XXX work around http://rt.perl.org/rt3/Ticket/Display.html?id=66824
        # this is a bit ugly.. since @a[1..1] returns a single item and not
        # an array, |@result[$one-item-range] throws the error
        # "argument doesn't array"
        while @r = $generator(|@(@result[*-$c..*-1])) {
            @result.push: @r;
        }
    }
    return @result;
}

multi sub infix:<eqv> (Num $a, Num $b) { $a === $b }
multi sub infix:<eqv> (Str $a, Str $b) { $a === $b }
multi sub infix:<eqv> (Code $a, Code $b) { $a === $b }
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

multi sub infix:<eqv> ($a, $b) {
    return Bool::False unless $a.WHAT === $b.WHAT;
    die "infix:<eqv> is only implemented for certain special cases yet";
}

multi sub infix:<minmax>(@a, @b) {
    (@a[0] min @b[0], @a[1] max @b[1]);
}

multi sub infix:<leg>($a, $b) {
    ~$a cmp ~$b;
}

# vim: ft=perl6
