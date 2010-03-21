our multi sub notresults(&op, Mu \$a, Mu \$b) {
    !(&op($a, $b));
}

our multi sub reverseargs(&op, Mu \$a, Mu \$b) {
    &op($b, $a);
}

our multi sub zipwith(&op, Iterable $a-iterable, Iterable $b-iterable) {
    my $ai = $a-iterable.iterator;
    my $bi = $b-iterable.iterator;
    gather loop {
        my $a = $ai.get;
        my $b = $bi.get;
        last if ($a ~~ EMPTY) || ($b ~~ EMPTY);
        take &op($a, $b);
    }
}

our multi sub zipwith(&op, $a, $b) {
    zipwith(&op, $a.list, $b.list);
}

our multi sub crosswith(&op, Iterable $a-iterable, Iterable $b-iterable) {
    my $ai = $a-iterable.iterator;
    my @b = $b-iterable.Seq;
    gather loop {
        my $a = $ai.get;
        last if ($a ~~ EMPTY);
        for @b -> $b {
            take &op($a, $b);
        }
    }
}

our multi sub crosswith(&op, $a, $b) {
    crosswith(&op, $a.list, $b.list);
}

our multi reduce(&op, $list) {
    $list.reduce(&op)
}

our multi sub hyper(&op, %lhs, %rhs, :$dwim-left, :$dwim-right) {
    die "Sorry, hyper operators on hashes are not yet implemented.";
}

our multi sub hyper(&op, @lhs, @rhs, :$dwim-left, :$dwim-right) {
    if $dwim-left || $dwim-right {
        die "Sorry, dwimmy cases of hyper operators are not yet implemented.";
    }
    my @result;
    for @lhs Z @rhs -> $l, $r {
        @result.push(op($l, $r));
    }
    @result
}

our multi sub hyper(&op, $lhs, $rhs, :$dwim-left, :$dwim-right) {
    hyper(&op, $lhs.list, $rhs.list, :$dwim-left, :$dwim-right);
}
