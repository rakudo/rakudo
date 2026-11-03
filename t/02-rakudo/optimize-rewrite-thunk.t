use Test;
use nqp;

plan 68;

my $rakuast := nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';

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
is-deeply (try (1 + 2 for 1..3).List), (3, 3, 3),
    'a folded constant as a for modifier statement is evaluated each iteration';
is-deeply (try (-1 for 1..2).List), (-1, -1),
    'a folded prefix as a for modifier statement is evaluated each iteration';
is-deeply (try (1 + 2 if False for 1..3).List), (),
    'a folded constant with a false if and a for modifier gives no values';
{
    my $i = 0;
    is-deeply (try (1 + 2 while $i++ < 3).List), (3, 3, 3),
        'a folded constant as a while modifier statement is evaluated each iteration';
}
{
    my $i = 0;
    is-deeply (try (1 + 2 until $i++ >= 3).List), (3, 3, 3),
        'a folded constant as an until modifier statement is evaluated each iteration';
}
{
    my $i = 0;
    is-deeply (try ($i++ while 1 - 1).List), (),
        'a folded false condition of a while modifier is tested';
}
is-deeply (try (do while 1 - 1 { 7 }).List), (),
    'a folded false condition of a while loop giving a value is tested';
is-deeply (try (do loop (my $i = 0; 1 - 1; $i++) { 7 }).List), (),
    'a folded false condition of a loop giving a value is tested';
{
    $_ = "a5b";
    s[\d] += 1 + 2;
    is $_, "a8b", 'a folded constant replacement of s[] += is added to the match';
}
{
    $_ = "a5b5";
    s:g[\d] += 1 + 2;
    is $_, "a8b8", 'a folded constant replacement of s:g[] += is added to each match';
}
{
    $_ = "a5b";
    s[\d] ~= "x" ~ "y";
    is $_, "a5xyb", 'a folded constant replacement of s[] ~= is appended to the match';
}
{
    $_ = "a5b";
    is (S[\d] += 1 + 2), "a8b", 'a folded constant replacement of S[] += is added to the match';
}
is-deeply (try (1 + 2 with $_ for 1, Any, 3).List), (3, 3),
    'a folded constant with a with and a for modifier is evaluated for each defined topic';
is-deeply (try (1 + 2 without $_ for 1, Any, 3).List), (3,),
    'a folded constant with a without and a for modifier is evaluated for each undefined topic';
is-deeply (try (1 + 2 when 3 for 3, 4).List), (3,),
    'a folded constant with a when and a for modifier is evaluated for each matching topic';
is-deeply (try (do repeat { 7 } while 1 - 1).List), (7,),
    'a folded false condition of a repeat loop giving a value is tested';
{
    my $n = 0;
    todo 'gives no values on the legacy frontend', 2 unless $rakuast;
    is-deeply (try (do repeat { NEXT { $n++ }; 7 } while 1 - 1).List), (7,),
        'a folded false condition of a repeat loop with a NEXT phaser is tested';
    is $n, 1, 'the NEXT phaser of a repeat loop with a folded condition runs';
}
{
    no worries;
    my $i = 0;
    is-deeply (try (do loop (; $i++ < 3; 1 + 2) { $i }).List), (1, 2, 3),
        'a loop giving a value with a folded constant increment runs its body each iteration';
}
{
    no worries;
    my $i = 0;
    my $n = 0;
    is (try { (do loop (; $i++ < 3; 1 + 2) { NEXT { $n++ }; $i }).eager; $n }), 3,
        'the NEXT phaser of a loop with a folded constant increment runs each iteration';
}
{
    my $n = 0;
    is (try { while 1 - 1 { UNDO { }; last if ++$n > 5; 1 }; $n }), 0,
        'a folded false condition of a while loop with an UNDO phaser stops it before its body';
}
{
    no worries;
    my $i = 0;
    is (try { loop (; $i < 3; 1 + 2) { UNDO { }; $i++ }; $i }), 3,
        'a loop with a folded constant increment and an UNDO phaser runs its body each iteration';
}
{
    my $i = 0;
    is (try { repeat { UNDO { }; $i++ } while 1 - 1; $i }), 1,
        'a folded false condition of a repeat loop with an UNDO phaser is tested';
}
{
    my $i = 0;
    is-deeply (try (do loop (; 1 + 0; $i++) { last if $i > 2; $i }).List), (0, 1, 2),
        'a loop giving a value with an increment and a folded true condition runs';
}
is-deeply (try EVAL(q[(0 || :($a) for 1..2)]).map(*.^name).List), ('Signature', 'Signature'),
    'a constant || giving a signature literal as a for modifier statement gives the signature each iteration';
ok (try EVAL(q[my @s = (0 || :($a, $b) for 1..2); \(1, 2) ~~ @s[0]])),
    'a constant || giving a signature literal as a for modifier statement gives a signature that binds';
given 100 {
    is (42 andthen 0 || (my $x = $_ + 1)), 43,
        'a constant || giving a declaration right of andthen is evaluated in the topic block';
}
{
    my $n = 0;
    my ($a, $b = 0 || (my $x = ++$n)) := \(1);
    my ($c, $d = 0 || (my $y = ++$n)) := \(1);
    is-deeply ($b, $d), (1, 2),
        'a constant || giving a declaration as a signature binding default is evaluated on each bind';
}
{
    my $n = 0;
    sub f($c = 0 || (my $x = ++$n)) { $c }
    my $default = &f.signature.params[0].default;
    is-deeply (+$default(), +$default()), (1, 2),
        'a constant || giving a declaration as a parameter default introspects as code evaluating it';
}
is (try EVAL(q[sub f($c = 0 || my class Foo { }) { $c }; f().^name])), 'Foo',
    'a constant || giving a class declaration as a parameter default gives the class';
{
    my $i = 0;
    is-deeply (try (1 + 2 if True while $i++ < 3).List), (3, 3, 3),
        'a folded constant with a true if and a while modifier is evaluated each iteration';
}
{
    my $i = 0;
    is (try { loop (; 1 + 0; $i++) { UNDO { }; last if $i > 2; 1 }; $i }), 3,
        'a loop with a folded true condition, an increment and an UNDO phaser runs its body';
}
{
    my $i = 0;
    is (try { loop (; 1 - 1; $i++) { UNDO { }; 1 }; $i }), 0,
        'a loop with a folded false condition, an increment and an UNDO phaser stops it before its body';
}
is (try EVAL(q[role R { method m { 42 andthen 1 + 2 } }; (Any but R).m])), 3,
    'a folded constant right of andthen in a role method is evaluated';
is (try EVAL(q[role R { method m($x = 1 + 2) { $x } }; (Any but R).m])), 3,
    'a folded constant parameter default of a role method is evaluated';
given 100 {
    is (try (42 andthen 0 || :a{ $_ }).value.()), 42,
        'a constant || giving a pair holding a block right of andthen gives a block that sees the topic';
}
{
    my $ran = 0;
    Nil andthen 0 || my class C { $ran++ };
    is $ran, 0, 'a constant || giving a class declaration right of andthen is not run for an undefined left side';
}
# These hold without the rewrite taking the thunks over too, and guard what it keeps.
{
    constant F = &uc;
    is-deeply (0 || F for 1..2).List, (&uc, &uc),
        'a constant || giving code as a for modifier statement gives the code each iteration';
    my class C does Callable { method CALL-ME(|) { 'called' } }
    constant G = C.new;
    is-deeply (0 || G for 1..2).map(*.^name).List, ('C', 'C'),
        'a constant || giving a Callable object as a for modifier statement gives the object each iteration';
}
{
    my @seen;
    0 || -> $x { @seen.push($x) } for 1..2;
    is-deeply @seen, [], 'a constant || giving a block as a sunk for modifier statement does not run the block';
}
given 100 {
    is-deeply (0 || { $_ } for 1..2).map({ $_() }).List, (1, 2),
        'a constant || giving a block as a for modifier statement gives a block that sees each topic';
}
ok (try EVAL(q[$_ = "a5"; s[\d] = True ?? :($a) !! 0; $_])).starts-with('aSignature'),
    'a constant ternary giving a signature literal as an s[] replacement replaces the match';
is (try EVAL(q[sub f($c = 0 || constant V = 9) { $c }; f()])), 9,
    'a constant || giving a constant declaration as a parameter default gives its value';
is-deeply (try EVAL(q[sub f($c = 0 || (1, 2)) { $c }; f()])), (1, 2),
    'a constant || giving a list as a parameter default gives the list';
is-deeply (try EVAL(q[my ($a, $b = 0 || (1, 2)) := \(1); $b])), (1, 2),
    'a constant || giving a list as a signature binding default gives the list';
ok (try EVAL(q[my @r = ((1, :($a)) xx 2 for 1..2); \(5) ~~ @r[0][0][1]])),
    'a signature literal in a list on the left of xx in a for modifier statement gives a signature that binds';
is (try EVAL(q[subset S of Int where 0 || { $_ > 0 }; 5 ~~ S])), True,
    'a constant || giving a block as the where of a subset is smartmatched';
