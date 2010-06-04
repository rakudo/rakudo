our multi sub negate(&op, Mu \$a, Mu \$b) {
    !(&op($a, $b));
}

our multi sub negate(&op) {
    Bool::True;
}

our multi sub reverseargs(&op, Mu \$a, Mu \$b) {
    &op($b, $a);
}

our multi sub sequentialargs(&op, Mu \$a, Mu \$b) {
    # CHEAT: this needs to do something more once Rakudo gets threading
    &op($a, $b);
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

our multi sub hyper(&op, Iterable $lhs-iterable, Iterable $rhs-iterable, :$dwim-left, :$dwim-right) {
    my sub repeating-array(@a) {
        gather loop {
            for @a -> $a {
                take $a;
            }
        }
    }

    my @lhs = $lhs-iterable.Seq;
    my @rhs = $rhs-iterable.Seq;

    my $length;
    if !$dwim-left && !$dwim-right {
        if +@lhs != +@rhs {
            die "Sorry, sides are of uneven length and not dwimmy.";
        }
        $length = +@lhs;
    } elsif !$dwim-left {
        $length = +@lhs;
    } elsif !$dwim-right {
        $length = +@rhs;
    } else {
        $length = +@lhs max +@rhs;
    }

    if $length != +@lhs {
        @lhs = repeating-array(@lhs).batch($length);
    }
    if $length != +@rhs {
        @rhs = repeating-array(@rhs).batch($length);
    }

    my @result;
    for @lhs Z @rhs -> $l, $r {
        if Associative.ACCEPTS($l) || Associative.ACCEPTS($r) {
            @result.push(hyper(&op, $l, $r, :$dwim-left, :$dwim-right));
        } elsif Iterable.ACCEPTS($l) || Iterable.ACCEPTS($r) {
            @result.push([hyper(&op, $l.list, $r.list, :$dwim-left, :$dwim-right)]);
        } else {
            @result.push(op($l, $r));
        }
    }
    @result
}

our multi sub hyper(&op, $lhs, $rhs, :$dwim-left, :$dwim-right) {
    hyper(&op, $lhs.list, $rhs.list, :$dwim-left, :$dwim-right);
}

our multi sub hyper(&op, %lhs, %rhs, :$dwim-left, :$dwim-right) {
    my %result;
    my @keys;
    if $dwim-left && $dwim-right {
        @keys = %lhs.keys.grep({ %rhs.exists($_) });
    } elsif $dwim-left {
        @keys = %rhs.keys;
    } elsif $dwim-right {
        @keys = %lhs.keys;
    } else {
        # .eagers should not be necessary in next line, but are
        # needed ATM because of the gather / take bug.
        @keys = (%lhs.keys.eager, %rhs.keys.eager).uniq;
    }

    for @keys -> $key {
        %result{$key} = &op(%lhs{$key}, %rhs{$key});
    }
    %result;
}

our multi sub hyper(&op, %arg) {
    my %result;
    for %arg.keys -> $key {
        %result{$key} = &op(%arg{$key});
    }
    %result;
}

our multi sub hyper(&op, %lhs, $rhs, :$dwim-left, :$dwim-right) {
    die "Sorry, right side is too short and not dwimmy." unless $dwim-right;
    my %result;
    for %lhs.keys -> $key {
        %result{$key} = &op(%lhs{$key}, $rhs);
    }
    %result;
}

our multi sub hyper(&op, $lhs, %rhs, :$dwim-left, :$dwim-right) {
    die "Sorry, left side is too short and not dwimmy." unless $dwim-left;
    my %result;
    for %rhs.keys -> $key {
        %result{$key} = &op($lhs, %rhs{$key});
    }
    %result;
}

our multi sub hyper(&op, @arg) {
    my @result;
    for @arg {
        # this is terribly ugly; but works
        @result.push(hyper(&op, $_)) if Associative.ACCEPTS($_);
        @result.push([hyper(&op, $_)]) if !Associative.ACCEPTS($_) && Iterable.ACCEPTS($_);
        @result.push(op($_))  if !Associative.ACCEPTS($_) && !Iterable.ACCEPTS($_);
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
our multi sub infix:<~>()  { '' }
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
our multi sub infix:<//>()     { Any }
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
