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

our multi sub zipwith(&op, $lhs, $rhs) {
    my $lhs-list = flat($lhs.list);
    my $rhs-list = flat($rhs.list);
    my ($a, $b);
    gather while ?$lhs-list && ?$rhs-list {
        $a = $lhs-list.shift unless $lhs-list[0] ~~ ::Whatever;
        $b = $rhs-list.shift unless $rhs-list[0] ~~ ::Whatever;
        take &op($a, $b);
    }
}

our multi sub crosswith(&op, $lhs, $rhs) {
    my $lhs-list = flat($lhs.list);
    my $rhs-list = flat($rhs.list);
    gather while ?$lhs-list {
        my $a = $lhs-list.shift;
        for @($rhs-list) -> $b {
            take &op($a, $b);
        }
    }
}

our multi reduce(&op, $list) {
    $list.reduce(&op)
}

our multi reduce(&op, *@list) {
    @list.reduce(&op)
}

our multi sub hyper(&op, @lhs, @rhs, :$dwim-left, :$dwim-right, :$path = '') {
    my sub repeating-array(@a) {
        gather loop {
            my $prev-a;
            for @a -> $a {
                if $a ~~ ::Whatever {
                    loop { take $prev-a }
                }
                $prev-a = take $a;
            }
        }
    }

    my $length;
    if !$dwim-left && !$dwim-right {
        if +@lhs != +@rhs {
            my $msg = "Sorry, lists on both sides of non-dwimmy hyperop are not of same length:\n"
                ~ "    left:  @lhs.elems() elements\n"
                ~ "    right: @rhs.elems() elements\n";
            $msg ~= "At .$path" if $path;
            die $msg;
        }
        $length = +@lhs;
    } elsif !$dwim-left {
        $length = +@lhs;
    } elsif !$dwim-right {
        $length = +@rhs;
    } else {
        $length =
            (@lhs - do @lhs[*-1] ~~ ::Whatever) max
            (@rhs - do @rhs[*-1] ~~ ::Whatever);
    }

    if $dwim-left && (@lhs - do @lhs[*-1] ~~ ::Whatever) != $length {
        @lhs := repeating-array(@lhs).munch($length);
    }
    if $dwim-right && (@rhs - do @rhs[*-1] ~~ ::Whatever) != $length {
        @rhs := repeating-array(@rhs).munch($length);
    }

    my @result;
    for ^$length -> $i {
        if Associative.ACCEPTS(@lhs[$i]) || Associative.ACCEPTS(@rhs[$i]) {
            @result.push(hyper(&op, @lhs[$i], @rhs[$i], :$dwim-left, :$dwim-right, path => $path ~ '[' ~ $i ~ ']').item);
        } elsif Iterable.ACCEPTS(@lhs[$i]) || Iterable.ACCEPTS(@rhs[$i]) {
            @result.push([hyper(&op, @lhs[$i].list, @rhs[$i].list, :$dwim-left, :$dwim-right, path => $path ~ '[' ~ $i ~ ']')]);
        } else {
            @result.push(op(@lhs[$i], @rhs[$i]));
        }
    }
    @result
}

our multi sub hyper(&op, ::T1 $lhs, ::T2 $rhs, :$dwim-left, :$dwim-right, :$path = '') {
    my $lhs-list = $lhs.list;
    my $rhs-list = $rhs.list;
    my $unordered = Any;
    if $lhs-list.elems != 1 and $lhs ~~ Iterable and $lhs !~~ Positional {
        $unordered = T1;
        $rhs-list.elems == 1 or die 'When one argument of a hyperoperator is an unordered data structure, the other must be scalar';
    } elsif $rhs-list.elems != 1 and $rhs ~~ Iterable and $rhs !~~ Positional {
        $unordered = T2;
        $lhs-list.elems == 1 or die 'When one argument of a hyperoperator is an unordered data structure, the other must be scalar';
    }
    my @result = hyper(&op, $lhs-list, $rhs-list, :$dwim-left, :$dwim-right, :$path);
    # If one of the arguments is unordered, we cast our return
    # value to be of its type, so set(1, 2, 3) »+» will return a Set
    # instead of an ordered type.
    $unordered !=== Any ?? $unordered.new(@result) !! @result
}

role Hash { ... }

our multi sub hyper(&op, Hash $lhs, Hash $rhs, :$dwim-left, :$dwim-right, :$path = '') {
    my %result;
    my @keys;
    if $dwim-left && $dwim-right {
        @keys = $lhs.keys.grep({ $rhs.exists($_) });
    } elsif $dwim-left {
        @keys = $rhs.keys;
    } elsif $dwim-right {
        @keys = $lhs.keys;
    } else {
        # .eagers should not be necessary in next line, but are
        # needed ATM because of the gather / take bug.
        @keys = ($lhs.keys.eager, $rhs.keys.eager).uniq;
    }

    for @keys -> $key {
        if Associative.ACCEPTS($lhs{$key}) || Associative.ACCEPTS($rhs{$key}) {
            %result{$key} = hyper(&op, $lhs{$key}, $rhs{$key}, :$dwim-left, :$dwim-right, path => $path ~ '{' ~ $key.perl ~ '}').item;
        } elsif Iterable.ACCEPTS($lhs{$key}) || Iterable.ACCEPTS($rhs{$key}) {
            %result{$key} = hyper(&op, $lhs{$key}.list, $rhs{$key}.list, :$dwim-left, :$dwim-right, path => $path ~ '{' ~ $key.perl ~ '}');
        } else {
            %result{$key} = op($lhs{$key}, $rhs{$key});
        }
    }
    %result;
}

our multi sub hyper(&op, Hash $arg) {
    my %result;
    for $arg.keys -> $key {
        %result{$key} = &op($arg{$key});
    }
    %result;
}

our multi sub hyper(&op, Hash $lhs, $rhs, :$dwim-left, :$dwim-right, :$path) {
    unless ($dwim-right) {
        my $msg = "Sorry, structures on both sides of non-dwimmy hyperop are not of same shape:\n"
            ~ "    left:  Hash\n"
            ~ "    right: $rhs.WHAT.perl()\n";
        $msg ~= "At .$path" if $path;
        die $msg;
    }
    my %result;
    for $lhs.keys -> $key {
        %result{$key} = &op($lhs{$key}, $rhs);
    }
    %result;
}

our multi sub hyper(&op, $lhs, Hash $rhs, :$dwim-left, :$dwim-right, :$path) {
    unless ($dwim-left) {
        my $msg = "Sorry, structures on both sides of non-dwimmy hyperop are not of same shape:\n"
            ~ "    left:  $lhs.WHAT.perl()\n"
            ~ "    right: Hash\n";
        $msg ~= "At .$path" if $path;
        die $msg;
    }
    my %result;
    for $rhs.keys -> $key {
        %result{$key} = &op($lhs, $rhs{$key});
    }
    %result;
}

our multi sub hyper(&op, @arg) {
    my @result;
    for @arg {
        # this is terribly ugly; but works
        @result.push(hyper(&op, $_).item) if Associative.ACCEPTS($_);
        @result.push([hyper(&op, $_)]) if !Associative.ACCEPTS($_) && Iterable.ACCEPTS($_);
        @result.push(op($_))  if !Associative.ACCEPTS($_) && !Iterable.ACCEPTS($_);
    }
    @result
}

our multi sub hyper(&op, ::T $arg) {
    my @result = hyper(&op, $arg.list);
    T ~~ Iterable && T !~~ Positional ?? T.new(@result) !! @result
}

our multi sub reducewith(&op, *@args,
                         :$chaining,
                         :$right-assoc,
                         :$triangle,
                         :$xor) {

    my $list = $right-assoc ?? @args.reverse !! @args;

    if $triangle {
        gather {
            return if !$list;
            my $result = $list.shift;

            if $xor {
                my $x = take $result;
                while ?$list {
                    my $next = $list.shift;
                    if $x {
                        if $next {
                            take False for ^(1 + $list);
                            last;
                        }
                        take $x;
                    } else {
                        $x = take (my $temp = $next);
                    }
                }
            } elsif $chaining {
                my $bool = Bool::True;
                take Bool::True;
                while ?$list {
                    my $next = $list.shift;
                    $bool = $bool && ($right-assoc ?? &op($next, $result) !! &op($result, $next));
                    my $temp = $bool;
                    take $temp;
                    $result = $next;
                }
            } else {
                my $temp = $result;
                take $temp;
                while ?$list {
                    my $next = $list.shift;
                    $result = $right-assoc ?? &op($next, $result) !! &op($result, $next);
                    my $temp = $result;
                    take $temp;
                }
            }
        }
    } else {
        return &op() if !$list;
        my $result = $list.shift;
        return &op($result) if !$list;

        if $xor {
            while ?$list {
                my $next = $list.shift;
                $next and $result and return False;
                $result ||= $next;
            }
        } elsif $chaining {
            my $bool = Bool::True;
            while ?$list {
                my $next = $list.shift;
                $bool = $bool && ($right-assoc ?? &op($next, $result) !! &op($result, $next));
                $result = $next;
            }
            return $bool;
        } else {
            while ?$list {
                my $next = $list.shift;
                $result = $right-assoc ?? &op($next, $result) !! &op($result, $next);
            }
        }
        $result;
    }
}

# degenerate case of operators, to be used by reduce() for the 0-ary case
# this fails for operators defined in PIR, so some of them are commented out.
our multi sub infix:<**>($x = 1) { +$x }
our multi sub infix:<*>($x = 1)  { +$x }
our multi sub infix:<?&>($x = Bool::True) { ?$x }
our multi sub infix:<+&>() { +^0 }
our multi sub infix:<+>($x = 0)  { +$x }
our multi sub infix:<->($x = 0)  { +$x }
our multi sub infix:<~>($x = '')  { ~$x }
our multi sub infix:<?|>($x = Bool::False) { ?$x }
our multi sub infix:<?^>($x = Bool::False) { ?$x }
our multi sub infix:<+|>() { 0 }
our multi sub infix:<+^>() { 0 }
our multi sub infix:<~|>() { '' }
our multi sub infix:<~^>() { '' }
#our multi sub infix:<&>()   { all() }
#our multi sub infix:<|>()   { any() }
#our multi sub infix:<^>()   { one() }

our multi sub infix:<==>($x?)     { Bool::True }
our multi sub infix:<!=>($x?)     { Bool::True }
our multi sub infix:«<»($x?)      { Bool::True }
our multi sub infix:«<=»($x?)     { Bool::True }
our multi sub infix:«>»($x?)      { Bool::True }
our multi sub infix:«>=»($x?)     { Bool::True }
our multi sub infix:<before>($x?) { Bool::True }
our multi sub infix:<after>($x?)  { Bool::True }
our multi sub infix:<~~>($x?)     { Bool::True }
our multi sub infix:<lt>($x?)     { Bool::True }
our multi sub infix:<le>($x?)     { Bool::True }
our multi sub infix:<gt>($x?)     { Bool::True }
our multi sub infix:<ge>($x?)     { Bool::True }
our multi sub infix:<eq>($x?)     { Bool::True }
our multi sub infix:<ne>($x?)     { Bool::True }
#our multi sub infix:<===>($x?)    { Bool::True }
our multi sub infix:<eqv>($x?)    { Bool::True }
#
our multi sub infix:<&&>(Mu $x = Bool::True)      { $x }
our multi sub infix:<and>(Mu $x = Bool::True)     { $x }
our multi sub infix:<||>(Mu $x = Bool::False)     { $x }
our multi sub infix:<or>(Mu $x = Bool::False)     { $x }
our multi sub infix:<^^>(Mu $x = Bool::False)     { $x }
our multi sub infix:<xor>(Mu $x = Bool::False)    { $x }
our multi sub infix:<//>()     { Any }
our multi sub infix:<orelse>() { Any }
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

our multi sub infix:</>($x)   { $x }
our multi sub infix:<%>($x)   { $x }
our multi sub infix:<x>($x)   { $x }
our multi sub infix:<xx>($x)  { $x }
our multi sub infix:«+<»($x)  { $x }
our multi sub infix:«+>»($x)  { $x }
our multi sub infix:<~&>($x)  { $x }

# negated smart-matching needs to be handled syntactically,
# but somebody might still call it by name.
our multi sub infix:<!~~>(Mu $a, Mu $b) { !($a ~~ $b) }
our multi sub infix:<!~~>(Mu $x?) { True }

# vim: ft=perl6
