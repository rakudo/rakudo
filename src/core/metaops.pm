our multi sub notresults(&op, Mu \$a, Mu \$b) {
    !(&op($a, $b));
}

our multi sub notresults(&op) {
    Bool::True;
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

our multi reduce(&op, *@list) {
    @list.reduce(&op)
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

our multi sub reducewith(&op, Iterable $an-iterable,
                         :$chaining,
                         :$right-assoc,
                         :$triangle) {
    my $ai = $an-iterable.iterator;
    $ai = $ai.Seq.reverse.iterator if $right-assoc;

    if $triangle {
        gather {
            my $result = $ai.get;
            return if $result ~~ EMPTY;

            if $chaining {
                my $bool = Bool::True;
                take Bool::True;
                loop {
                    my $next = $ai.get;
                    last if $next ~~ EMPTY;
                    $bool = $bool && ($right-assoc ?? &op($next, $result) !! &op($result, $next));
                    my $temp = $bool;
                    take $temp;
                    $result = $next;
                }
            } else {
                my $temp = $result;
                take $temp;
                loop {
                    my $next = $ai.get;
                    last if $next ~~ EMPTY;
                    $result = $right-assoc ?? &op($next, $result) !! &op($result, $next);
                    my $temp = $result;
                    take $temp;
                }
            }
        }
    } else {
        my $result = $ai.get;
        if $result ~~ EMPTY {
            return &op();
        }

        if $chaining {
            my $bool = Bool::True;
            loop {
                my $next = $ai.get;
                last if $next ~~ EMPTY;
                $bool = $bool && ($right-assoc ?? &op($next, $result) !! &op($result, $next));
                $result = $next;
            }
            return $bool;
        } else {
            loop {
                my $next = $ai.get;
                last if $next ~~ EMPTY;
                $result = $right-assoc ?? &op($next, $result) !! &op($result, $next);
            }
        }
        $result;
    }
}

our multi sub reducewith(&op, $arg,
                         :$chaining,
                         :$right-assoc,
                         :$triangle) {
    reducewith(&op, $arg.list, :$chaining, :$right-assoc, :$triangle);
}

# degenerate case of operators, to be used by reduce() for the 0-ary case
# this fails for operators defined in PIR, so some of them are commented out.
our multi sub infix:<**>() { 1 }
our multi sub infix:<*>()  { 1 }
our multi sub infix:<+&>() { +^0 }
our multi sub infix:<+>()  { 0 }
our multi sub infix:<->()  { 0 }
#our multi sub infix:<~>()  { '' }
our multi sub infix:<+|>() { 0 }
our multi sub infix:<+^>() { 0 }
our multi sub infix:<~|>() { '' }
our multi sub infix:<~^>() { '' }
#our multi sub infix:<&>()   { all() }
#our multi sub infix:<|>()   { any() }
#our multi sub infix:<^>()   { one() }

our multi sub infix:<==>()     { Bool::True }
our multi sub infix:<!=>()     { Bool::True }
our multi sub infix:«<»()      { Bool::True }
our multi sub infix:«<=»()     { Bool::True }
our multi sub infix:«>»()      { Bool::True }
our multi sub infix:«>=»()     { Bool::True }
our multi sub infix:<before>() { Bool::True }
our multi sub infix:<after>()  { Bool::True }
our multi sub infix:<~~>()     { Bool::True }
our multi sub infix:<lt>()     { Bool::True }
our multi sub infix:<le>()     { Bool::True }
our multi sub infix:<gt>()     { Bool::True }
our multi sub infix:<ge>()     { Bool::True }
our multi sub infix:<eq>()     { Bool::True }
our multi sub infix:<ne>()     { Bool::True }
#our multi sub infix:<===>()    { Bool::True }
our multi sub infix:<eqv>()    { Bool::True }
#
our multi sub infix:<||>()     { Bool::False }
our multi sub infix:<or>()     { Bool::False }
#our multi sub infix:<^^>()     { Bool::False }
#our multi sub infix:<//>()     { Any }
#our multi sub infix:<min>()    { +Inf }
#our multi sub infix:<max>()    { -Inf }
#our multi sub infix:<=>()      { Nil }
#our multi sub infix:<:=>()     { Nil }
#our multi sub infix:<,>()      { [] }
our multi sub infix:<Z>()      { [] }

our multi sub infix:</>()   { fail "No zero-arg meaning for infix:</>"; }
our multi sub infix:<%>()   { fail "No zero-arg meaning for infix:<%>"; }
our multi sub infix:<x>()   { fail "No zero-arg meaning for infix:<x>"; }
our multi sub infix:<xx>()  { fail "No zero-arg meaning for infix:<xx>"; }
our multi sub infix:«+<»()  { fail "No zero-arg meaning for infix:«+<»"; }
our multi sub infix:«+>»()  { fail "No zero-arg meaning for infix:«+>»"; }
our multi sub infix:<~&>()  { fail "No zero-arg meaning for infix:<~&>"; }

# vim: ft=perl6
