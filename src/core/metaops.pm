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

our multi sub hyper(&op, Iterable $lhs-iterable, Iterable $rhs-iterable, :$dwim-left, :$dwim-right) {
    my @lhs = $lhs-iterable.Seq;
    my @rhs = $rhs-iterable.Seq;

    if @lhs.elems != @rhs.elems {
        if @lhs.elems > @rhs.elems {
            if $dwim-right {
                if @rhs.elems > 0 {
                    @rhs.push: @rhs[@rhs.elems - 1] xx (@lhs.elems - @rhs.elems);
                } else {
                    @rhs.push: &op() xx (@lhs.elems - @rhs.elems);
                }
            } else {
                die "Sorry, right side is too short and not dwimmy.";
            }
        } else {
            if $dwim-left {
                if @lhs.elems > 0 {
                    @lhs.push: @lhs[@lhs.elems - 1] xx (@rhs.elems - @lhs.elems);
                } else {
                    @lhs.push: &op() xx (@rhs.elems - @lhs.elems);
                }
            } else {
                die "Sorry, left side is too short and not dwimmy.";
            }
        }
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

our multi sub hyper(&op, @arg) {
    my @result;
    for @arg {
        @result.push(op($_));
    }
    @result
}

our multi sub hyper(&op, $arg) {
    hyper(&op, $arg.list)
}
