use Test;

plan 21;

given 100 {
    is (42 andthen 0 || $_ + 1), 43,
        'a constant || right of andthen sees the topic andthen gives';
    is (42 andthen 1 && $_ + 1), 43,
        'a constant && right of andthen sees the topic andthen gives';
    is (42 andthen True ?? $_ + 1 !! 0), 43,
        'a constant ternary right of andthen sees the topic andthen gives';
    is (Any orelse 0 || $_.raku), 'Any',
        'a constant || right of orelse sees the topic orelse gives';
    is ([andthen] 42, 0 || $_ + 1), 43,
        'a constant || in a reduce with andthen sees the topic andthen gives';
    is-deeply ((42,) Zandthen (0 || $_ + 1,)).List, (43,),
        'a constant || in a list under Zandthen sees the topic andthen gives';
}
given [7, 8, 9] {
    is-deeply ([1, 2, 3] andthen $_[0, 1]).List, (1, 2),
        'a slice right of andthen sees the topic andthen gives';
}
is ([||] 1, 0 || die "evaluated"), 1,
    'a constant || in a reduce with || is not evaluated after a true operand';
{
    sub f($c = 0 || $*X) { $c }
    my $*X = 7;
    is (try f()), 7, 'a constant || as a parameter default is evaluated when the default is';
}
{
    sub f($c = True ?? $*X !! 0) { $c }
    my $*X = 7;
    is (try f()), 7, 'a constant ternary as a parameter default is evaluated when the default is';
}
{
    my class C { method m($c = 0 || self) { $c } }
    isa-ok (try C.new.m), C, 'a constant || naming self as a parameter default is evaluated when the default is';
}
{
    sub g(&c = 0 || *.succ) { c(1) }
    is (try g()), 2, 'a constant || as a parameter default gives its WhateverCode';
}
{
    my @a = 1, 2, 3;
    sub f($c = @a[0, 1]) { $c }
    is-deeply (try f()), (1, 2), 'a slice as a parameter default is evaluated when the default is';
}
{
    sub f(@a, :$c = @a[0, 1]) { $c }
    is-deeply (try f([1, 2, 3])), (1, 2), 'a slice of an earlier parameter as a named default is evaluated';
}
{
    my Int $n = 3;
    sub f($c = $n ** 2) { $c }
    is (try f()), 9, 'a square as a parameter default is evaluated when the default is';
}
isa-ok (42 andthen 0 || *.succ), WhateverCode,
    'a constant || right of andthen gives the WhateverCode its topic block evaluates to';
given 100 {
    isa-ok (42 andthen 0 || -> $x { $x + 1 }), Block,
        'a constant || right of andthen gives the block its topic block evaluates to';
    my $b = (42 andthen 1 && { $_ });
    is $b(), 42, 'a constant && right of andthen gives a block that sees the topic andthen gives';
}
{
    constant F = &uc;
    is-deeply ((0 || F) xx 2).List, (&uc, &uc),
        'a constant || giving code on the left of xx is evaluated for each repetition';
}
{
    my class C does Callable { method CALL-ME(|) { 'called' } }
    constant F = C.new;
    is-deeply ((False || F) xx 1).map(*.^name).List, ('C',),
        'a constant || giving a Callable object on the left of xx gives the object';
    is (42 andthen (True ?? F !! 0)).^name, 'C',
        'a constant ternary right of andthen gives the Callable object its topic block evaluates to';
}
