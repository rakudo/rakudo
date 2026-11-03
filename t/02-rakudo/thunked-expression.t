use Test;
use nqp;

plan 250;

my $rakuast := nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';

is-deeply (try EVAL(q[(my $x = 7 for 1..2)]).List), (7, 7),
    'a declaration as a for modifier statement is initialized each iteration';
is-deeply (try EVAL(q[(our $y = 7 for 1..2)]).List), (7, 7),
    'an our declaration as a for modifier statement is initialized each iteration';
is-deeply (try EVAL(q[my $i = 0; (my $x = ++$i if True for 1..3)]).List), (3, 3, 3),
    'a declaration with an if and a for modifier is initialized each iteration';
if $rakuast {
    is-deeply (try EVAL(q[my $i = 0; (my $x = 7 while $i++ < 2)]).List), (7, 7),
        'a declaration as a while modifier statement is initialized each iteration';
}
else {
    skip 'dies on the legacy frontend', 1;
}
is-deeply (try EVAL(q[(my @a = 1, 2 for 1..2)]).map(*.List).List), ((1, 2), (1, 2)),
    'an array declaration as a for modifier statement is initialized each iteration';
is-deeply (try EVAL(q[(my ($a, $b) = 1, 2 for 1..2)]).map(*.join(',')).List), ('1,2', '1,2'),
    'a signature declaration as a for modifier statement is initialized each iteration';
is-deeply (try EVAL(q[(my \x = 7 for 1..2)]).List), (7, 7),
    'a sigilless declaration as a for modifier statement is initialized each iteration';
is-deeply (try EVAL(q[(constant X = 5 for 1..2)]).List), (5, 5),
    'a constant declaration as a for modifier statement gives its value each iteration';
todo 'gives the variable before initializing it on the legacy frontend' unless $rakuast;
is-deeply (try EVAL(q[my $i = 0; (state $x = ++$i for 1..3)]).List), (1, 1, 1),
    'a state declaration as a for modifier statement is initialized once';
is (try EVAL(q[$_ = "a5"; s[\d] = my $x = "z"; $_])), 'az',
    'a declaration as an s[] replacement is initialized';
is-deeply (try EVAL(q[{ ($^a for 1..2) }(5)]).List), (5, 5),
    'a positional placeholder as a for modifier statement gives its value each iteration';
is-deeply (try EVAL(q[{ ($:n for 1..2) }(:n(5))]).List), (5, 5),
    'a named placeholder as a for modifier statement gives its value each iteration';
is-deeply (try EVAL(q[sub { (@_ for 1..2) }(1, 2)]).map(*.List).List), ((1, 2), (1, 2)),
    'a slurpy placeholder as a for modifier statement gives its value each iteration';
is-deeply (try EVAL(q[my $n = 0; my $x = $n++ for 1..3; ($n, $x)])), (3, 2),
    'a sunk declaration as a for modifier statement is initialized each iteration';
is-deeply (try EVAL(q[my @q = 1, 2, 3; (do while my $x = @q.shift { $x * 2 }).List])), (2, 4, 6),
    'a declaration as the condition of a while loop giving a value is initialized each time';
is (try EVAL(q[my @q = 1, 2, 3; my $n = 0; while my $x = @q.shift { UNDO { }; $n += $x; 1 }; $n])), 6,
    'a declaration as the condition of a while loop with an UNDO phaser is initialized each time';
todo 'binds in the frame of the thunk on the legacy frontend', 3 unless $rakuast;
is (try EVAL(q[42 andthen my ($a, $b) := ($_, 1); $a])), 42,
    'a signature declaration that binds right of andthen sees the topic andthen gives';
is-deeply (try EVAL(q[sub f($x) { $x andthen my ($a, $b) := ($_, 2); $a }; (f(5), f(6))])), (5, 6),
    'a signature declaration that binds right of andthen in a routine binds in each call';
is (try EVAL(q[(my ($a, $b) := ($_, 2) for 1..2); $a])), 2,
    'a signature declaration that binds as a for modifier statement binds each iteration';
is (try EVAL(q[Nil andthen my ($a, $b) := die "boom"; "end"])), 'end',
    'a signature declaration that binds right of andthen is not evaluated for an undefined left side';
given 100 {
    is-deeply (42 andthen my ($p, $q) = $_ + 1, 2).List, (43, 2),
        'a signature declaration that assigns right of andthen is evaluated in the topic block';
}
is (try EVAL(q[try { !!! * ~ "x" }; $!.message.^name])), 'WhateverCode',
    'a stub given a WhateverCode as its message keeps it';
is (try EVAL(q[try { !!! { 42 } }; $!.message.^name])), 'Block',
    'a stub given a block as its message keeps it';
is (try EVAL(q[my $x = 5; try { !!! "m $x" }; $!.message])), 'm 5',
    'a stub message using a variable of an outer block is built';
is-deeply (try EVAL(q[(:($a) for 1..2)]).map(*.^name).List), ('Signature', 'Signature'),
    'a signature literal as a for modifier statement gives the signature each iteration';
ok (try EVAL(q[my @s = (:($a, $b) for 1..2); \(1, 2) ~~ @s[0]])),
    'a signature literal as a for modifier statement gives a signature that binds';
is-deeply (try EVAL(q[my $n = 5; (:($a = $n) for 1..2).map({ .params[0].default.() }).List])), (5, 5),
    'a signature literal as a for modifier statement gives a default that evaluates';
lives-ok { EVAL(q[(Nil andthen !!!)]) },
    'a stub right of andthen is not evaluated for an undefined left side';
throws-like q[42 andthen !!!], X::StubCode,
    'a stub right of andthen is evaluated for a defined left side';
lives-ok { EVAL(q[(... for ^0)]) },
    'a stub as a for modifier statement over nothing is not evaluated';
throws-like q[my @r = (... for 1..2)], X::StubCode,
    'a stub as a for modifier statement is evaluated';
is (try EVAL(q[my $ran = 0; my $r = (Nil andthen race for 1..2 { $ran++ }); $ran])), 0,
    'a race for loop right of andthen is not run for an undefined left side';
is (try EVAL(q[my $ran = 0; my $r = (42 andthen race for 1..2 { $ran++ }); $ran])), 2,
    'a race for loop right of andthen runs for a defined left side';
is (try EVAL(q[try { 42 andthen !!! "got $_" }; $!.message])), 'got 42',
    'a stub message right of andthen sees the topic andthen gives';
isa-ok (try EVAL(q[sub f { 42 andthen ... }; f().exception])), X::StubCode,
    'a fail stub right of andthen in a routine returns its Failure';
{
    use experimental :rakuast;
    my $ast := RakuAST::Circumfix::Parentheses.new(RakuAST::SemiList.new(
      RakuAST::Statement::Expression.new(
        expression    => RakuAST::Signature.new(parameters => (
          RakuAST::Parameter.new(target => RakuAST::ParameterTarget::Var.new(name => '$a')),
        )),
        loop-modifier => RakuAST::StatementModifier::For.new(RakuAST::ApplyInfix.new(
          left  => RakuAST::IntLiteral.new(1),
          infix => RakuAST::Infix.new('..'),
          right => RakuAST::IntLiteral.new(2)
        ))
      )
    ));
    is-deeply (try EVAL($ast).map(*.^name).List), ('Signature', 'Signature'),
        'a signature built through the RakuAST API as a for modifier statement gives the signature each iteration';
}
ok (try EVAL(q[my @s = ((:($a, $b) for 1..2) for 1..2); \(1, 2) ~~ @s[1][1]])),
    'a signature literal in nested for modifier statements gives a signature that binds';
ok (try EVAL(q[my $x = 1; my @r = (($x, :($a)) xx 2 for 1..2); \(5) ~~ @r[0][0][1]])),
    'a signature literal in a thunked list in a for modifier statement gives a signature that binds';
given 100 {
    is (42 andthen try $_ + 1), 43,
        'a try right of andthen sees the topic andthen gives';
    is-deeply (42 andthen gather take $_ + 1).List, (43,),
        'a gather right of andthen sees the topic andthen gives';
    is (42 andthen once $_ + 1), 43,
        'a once right of andthen sees the topic andthen gives';
    is (await (42 andthen start $_ + 1)), 43,
        'a start right of andthen sees the topic andthen gives';
    is (Nil orelse try $_.raku), 'Nil',
        'a try right of orelse sees the topic orelse gives';
    is (Nil notandthen try $_.raku), 'Nil',
        'a try right of notandthen sees the topic notandthen gives';
}
if $rakuast {
    given 100 {
        my $r = (42 andthen /a$_/);
        ok "a42" ~~ $r, 'a regex right of andthen sees the topic andthen gives';
    }
}
else {
    skip 'dies on the legacy frontend', 1;
}
is-deeply (try EVAL(q[sub g($i) { ((my sub foo { $i * 10 }; $i) xx 1); foo() }; (g(1), g(2), g(3))])), (10, 20, 30),
    'a sub declared in parenthesized statements on the left of xx closes over each call of its routine';
todo 'closes over the previous call on the legacy frontend' unless $rakuast;
is-deeply (try EVAL(q[sub g($i) { try my sub foo { $i * 10 }; foo() }; (g(1), g(2), g(3))])), (10, 20, 30),
    'a sub declared under try closes over each call of its routine';
is-deeply (try EVAL(q[sub g($i) { once ((my sub foo { $i * 10 }; $i) xx 1); foo() }; (g(1), g(2), g(3))])), (10, 20, 30),
    'a sub declared on the left of xx under once closes over each call of its routine';
todo 'closes over the previous call on the legacy frontend', 2 unless $rakuast;
is-deeply (try EVAL(q[sub g($i) { (gather take my sub foo { $i * 10 }).eager; foo() }; (g(1), g(2), g(3))])), (10, 20, 30),
    'a sub declared under gather closes over each call of its routine';
is-deeply (try EVAL(q[sub g($i) { (try my sub foo { $i * 10 }) xx 1; foo() }; (g(1), g(2), g(3))])), (10, 20, 30),
    'a sub declared under try on the left of xx closes over each call of its routine';
is (try EVAL(q[constant K = (42 andthen try $_ + 1); K])), 43,
    'a try right of andthen in the value of a constant sees the topic andthen gives';
is (try EVAL(q[my $x is default(42 andthen try $_ + 1); $x])), 43,
    'a try right of andthen in a trait argument sees the topic andthen gives';
ok (try EVAL(q[my $s = try :($a, $b); \(1, 2) ~~ $s])),
    'a signature literal under try gives a signature that binds';
ok (try EVAL(q[my $s = (gather take :($a, $b))[0]; \(1, 2) ~~ $s])),
    'a signature literal under gather gives a signature that binds';
todo 'sees the first call on the legacy frontend' unless $rakuast;
is-deeply (try EVAL(q[sub g($n) { my $s = try :($a where * > $n); \(5) ~~ $s }; (g(3), g(7))])), (True, False),
    'a signature literal under try has a where clause that sees each call of its routine';
ok (try EVAL(q[my $s; { $s = :($a, $b) }; \(1, 2) ~~ $s])),
    'a signature literal in a bare block gives a signature that binds';
ok (try EVAL(q[my @s; for 1..2 { @s.push: :($a, $b) }; \(1, 2) ~~ @s[1]])),
    'a signature literal in the body of a for loop gives a signature that binds';
ok (try EVAL(q[my $s = BEGIN :($a, $b); \(1, 2) ~~ $s])),
    'a signature literal under BEGIN gives a signature that binds';
is-deeply (try EVAL(q[sub g($i) { ((my proto foo($x) { $i * 10 + $x }; $i) xx 1); foo(1) }; (g(1), g(2), g(3))])), (11, 21, 31),
    'a proto with a body declared on the left of xx closes over each call of its routine';
is-deeply (try EVAL(q[sub h($y) { my $x = $y; ENTER (0 || my sub foo { $x }); foo() }; (h(1), h(2))])), (1, 2),
    'a sub declared under ENTER closes over each call of its routine';
is-deeply (try EVAL(q[sub h($y) { my $x = $y; LEAVE (0 || my sub foo { $x }); foo() }; (h(1), h(2))])), (1, 2),
    'a sub declared under LEAVE closes over each call of its routine';
is-deeply (try EVAL(q[sub h($y) { my $x = $y; PRE (my sub foo { $x }; 1); foo() }; (h(1), h(2))])), (1, 2),
    'a sub declared under PRE closes over each call of its routine';
todo 'runs on each call on the legacy frontend' unless $rakuast;
is-deeply (try EVAL(q[sub g($i) { ((once $i) for 1) }; (g(1), g(2)).map(*.List).List])), ((1,), (1,)),
    'a once as a for modifier statement runs once per closure of its routine';
todo 'sees the first call on the legacy frontend' unless $rakuast;
is-deeply (try EVAL(q[sub g($i) { my $r = (try ENTER $i * 10); $r }; (g(1), g(2))])), (10, 20),
    'an ENTER phaser under try sees each call of its routine';
todo 'gives Mu on the legacy frontend' unless $rakuast;
is-deeply (try EVAL(q[my @s; for 1..3 -> $i { try LAST @s.push: $i }; @s.List])), (3,),
    'a LAST phaser under try sees the last iteration';
todo 'sees the first call on the legacy frontend' unless $rakuast;
is (try EVAL(q[my $s = ''; sub g($i) { try my $x will enter { $s ~= "e$i " }; 1 }; g(1); g(2); $s])), 'e1 e2 ',
    'a will enter trait under try sees each call of its routine';
todo 'gives Mu on the legacy frontend' unless $rakuast;
is-deeply (try EVAL(q[my @s; for 1..3 -> $i { try my $x will last { @s.push: $i } }; @s.List])), (3,),
    'a will last trait under try sees the last iteration';
todo 'gives 1 on the legacy frontend' unless $rakuast;
is (try EVAL(q[constant K = * + (state $x = 5); K.(1)])), 6,
    'a state declaration in a Whatever curry as the value of a constant is initialized';
is (try EVAL(q[my $s = ''; sub g($i) { ENTER { $s ~= "e$i " } xx 1; 1 }; g(1); g(2); $s])), 'e1 e2 ',
    'an ENTER block as the left of xx sees each call of its routine';
is-deeply (try EVAL(q[sub g($i) { [ENTER { $i * 10 }] xx 1 }; (g(1), g(2)).map(*[0][0]).List])), (10, 20),
    'an ENTER block in an array on the left of xx sees each call of its routine';
is (try EVAL(q[my $s = ''; for 1..3 -> $i { 1 andthen FIRST $s ~= "f$i" }; $s])), 'f1',
    'a FIRST phaser right of andthen runs on the first iteration';
todo 'sees the first call on the legacy frontend', 3 unless $rakuast;
is (try EVAL(q[my $s = ''; sub g($i) { 1 andthen ENTER { $s ~= "e$i " }; 1 }; g(1); g(2); $s])), 'e1 e2 ',
    'an ENTER block right of andthen sees each call of its routine';
is (try EVAL(q[my $s = ''; sub g($i) { try ENTER { $s ~= "e$i " }; 1 }; g(1); g(2); $s])), 'e1 e2 ',
    'an ENTER block under try sees each call of its routine';
is (try EVAL(q[sub g($i) { 1 andthen ENTER $i * 10 }; BEGIN g(1); g(2)])), 20,
    'an ENTER phaser right of andthen in a routine called at BEGIN time sees each call';
todo 'closes over the first call on the legacy frontend' unless $rakuast;
is (try EVAL(q[sub g($i) { my $s = ''; ("x" ~~ / :my sub foo { $i }; x { $s ~= foo() } /) xx 1; $s }; g(1) ~ g(2)])), '12',
    'a sub declared in a regex on the left of xx closes over each call of its routine';
todo 'sees the first call on the legacy frontend' unless $rakuast;
is (try EVAL(q[sub g($i) { my $s = "x"; ($s ~~ s[x] = ENTER "e$i") xx 1; $s }; g(1) ~ g(2)])), 'e1e2',
    'an ENTER phaser in a substitution on the left of xx gives its value';
is (try EVAL(q[my $s = ''; sub g($i) { my token tk { x { $s ~= $i } }; -> $t { $t ~~ &tk } }; my $a = g(1); my $b = g(2); $a("x"); $b("x"); $s])), '12',
    'a lexical token closes over each call of its routine';
is-deeply (try EVAL(q[sub g($i) { my method m { $i }; -> { 5.&m } }; my $a = g(1); my $b = g(2); ($a(), $b())])), (1, 2),
    'a lexical method closes over each call of its routine';
is-deeply (try EVAL(q[sub g($i) { my module M { our method m { $i }; our sub k { &m } }; M::k() }; my $a = g(1); my $b = g(2); (5.$a, 5.$b)])), (1, 2),
    'an our method closes over each call of its routine';
is (try EVAL(q[my $s = ''; sub g($i) { my module M { our token t { x { $s ~= $i } }; our sub k { &t } }; M::k() }; my $a = g(1); my $b = g(2); "x" ~~ $a; "x" ~~ $b; $s])), '12',
    'an our token closes over each call of its routine';
is-deeply (try EVAL(q[sub h($y) { my $x = $y; POST (my sub foo { $x }; 1); foo() }; (h(1), h(2), h(3))])), (1, 2, 3),
    'a sub declared under POST closes over each call of its routine';
is-deeply (try EVAL(q[sub g($i) { my $r; for 1 { FIRST (0 || my sub foo { $i }); $r = foo() }; $r }; (g(1), g(2), g(3))])), (1, 2, 3),
    'a sub declared right of || under FIRST closes over each call of its routine';
todo 'closes over the previous call on the legacy frontend' unless $rakuast;
is-deeply (try EVAL(q[sub g($i) { my $r; for 1 { FIRST my sub foo { $i }; $r = foo() }; $r }; (g(1), g(2), g(3))])), (1, 2, 3),
    'a sub declared under FIRST closes over each call of its routine';
is-deeply (try EVAL(q[sub h($y) { my $x = $y; BEGIN my sub foo { $x }; foo() }; (h(1), h(2))])), (1, 2),
    'a sub declared under BEGIN in a routine closes over each call of the routine';
is-deeply (try EVAL(q[sub h($y) { my $x = $y; CHECK my sub foo { $x }; foo() }; (h(1), h(2))])), (1, 2),
    'a sub declared under CHECK in a routine closes over each call of the routine';
if $rakuast {
    is-deeply (try EVAL(q[sub h($y) { my $x = $y; constant c = try my sub foo { $x }; foo() }; (h(1), h(2))])), (1, 2),
        'a sub declared in the value of a constant in a routine closes over each call of the routine';
}
else {
    skip 'dies on the legacy frontend', 1;
}
is-deeply (try EVAL(q[sub h($y) { my $x = $y; my enum E (a => ((my sub foo { $x }) xx 1)); foo() }; (h(1), h(2))])), (1, 2),
    'a sub declared in an enum value in a routine closes over each call of the routine';
is-deeply (try EVAL(q[sub a($y) { my $x = $y; FIRST (FIRST (my sub foo { $x }; 1); 1); foo() }; (a(1), a(2), a(3))])), (1, 2, 3),
    'a sub declared under a FIRST in the statement of a FIRST closes over each call of its routine';
is-deeply (try EVAL(q[sub b($y) { my $x = $y; POST (FIRST (my sub foo { $x }; 1); 1); foo() }; (b(1), b(2), b(3))])), (1, 2, 3),
    'a sub declared under a FIRST in the statement of a POST closes over each call of its routine';
todo 'sees the first call on the legacy frontend' unless $rakuast;
is (try EVAL(q[my $s = ''; sub g($i) { FIRST my $x will enter { $s ~= "en$i " }; 1 }; g(1); g(2); $s])), 'en1 en2 ',
    'a will enter trait under FIRST sees each call of its routine';
todo 'sees the first iteration on the legacy frontend' unless $rakuast;
is (try EVAL(q[my $s = ''; for 1..2 -> $i { FIRST my $x will leave { $s ~= "L$i " }; 1 }; $s])), 'L1 L2 ',
    'a will leave trait under FIRST sees each iteration';
is (try EVAL(q[my $s = ''; sub f($p) { LEAVE (multi g(Int) { $s ~= "l$p" }); g(1) }; f(1); f(2); $s])), 'l1l2',
    'a multi declared under LEAVE closes over each call of its routine';
todo 'closes over the first call on the legacy frontend', 2 unless $rakuast;
is-deeply (try EVAL(q[sub g($i) { my ($a, $b = (my sub foo { $i * 10 })()) := \(1); $b + foo() }; (g(1), g(2))])), (20, 40),
    'a sub declared in a default of a signature declaration closes over each call of its routine';
is-deeply (try EVAL(q[sub f($y, $x = (my sub g { $y })()) { g() }; (f(1), f(2, 0), f(3, 0))])), (1, 2, 3),
    'a sub declared in a parameter default is called in the body of a later call not evaluating the default';
is-deeply (try EVAL(q[sub g($i) { my ($a where (my sub w($v) { $v == $i })($_)) := \($i); w($i) }; (g(1), g(2))])), (True, True),
    'a sub declared in the where of a signature declaration closes over each call of its routine';
is (try EVAL(q[my $ran = 0; Nil andthen my class C { $ran++ }; $ran])), 0,
    'a class declared right of andthen is not run for an undefined left side';
is (try EVAL(q[my $seen; 42 andthen my class C { $seen = $_ }; $seen])), 42,
    'a class declared right of andthen runs in the topic block';
is (try EVAL(q[my $i = 0; my @a = (my class C { $i++ }) xx 3; $i])), 3,
    'a class declared on the left of xx runs for each repetition';
todo 'the where clause misses the topic on the legacy frontend' unless $rakuast;
ok (try EVAL(q[$_ = 7; my $s = (42 andthen :($x where * == $_)); \(42) ~~ $s])),
    'a signature literal right of andthen has a where clause that sees the topic';
given 100 {
    is (42 andthen my $y = $_ + 1), 43,
        'a declaration right of andthen is evaluated in the topic block';
    is (42 andthen (my $z = $_ + 1)), 43,
        'a parenthesized declaration right of andthen is evaluated in the topic block';
    is ([andthen] 42, (my $x = $_ + 1)), 43,
        'a declaration in a reduce with andthen is evaluated in the topic block';
    is-deeply ((42,) Zandthen ((my $w = $_ + 1),)).List, (43,),
        'a declaration in a list under Zandthen is evaluated in the topic block';
    is-deeply (42 andthen my @a = $_ + 1), [43],
        'an array declaration right of andthen is evaluated in the topic block';
    is (42 andthen (try $_ + 1)), 43,
        'a parenthesized try right of andthen sees the topic andthen gives';
    is-deeply (42 andthen (gather take $_ + 1)).List, (43,),
        'a parenthesized gather right of andthen sees the topic andthen gives';
}
{
    my $i = 0;
    is-deeply ((my $x = ++$i) xx 3).List, (1, 2, 3),
        'a declaration on the left of xx is evaluated for each repetition';
}
{
    my $i = 0;
    is-deeply (((my $x = ++$i), 5) xx 2).map(*.List).List, ((2, 5), (2, 5)),
        'a list holding a declaration on the left of xx is evaluated for each repetition';
}
is (try (42 andthen Block)), Block,
    'a Callable type object right of andthen gives the type object';
if $rakuast {
    is-deeply (try (Block xx 2).List), (Block, Block),
        'a Callable type object on the left of xx gives the type object for each repetition';
}
else {
    skip 'dies on the legacy frontend', 1;
}
lives-ok { Nil andthen my $v = die "evaluated" },
    'a declaration right of andthen is not evaluated for an undefined left side';
{
    my ($a, $b = my $y = 5) := \(1);
    is $b, 5, 'a declaration as a signature binding default is initialized';
}
is (try EVAL(q[$_ = 100; (42 andthen my $x = $_ + 1)])), 43,
    'a declaration right of andthen at unit scope is initialized in the topic block';
is (try EVAL(q[sub f(Int $c = my $x = 5) { $c }; f()])), 5,
    'a declaration as a typed parameter default gives its value';
is (try EVAL(q[sub f(int $c = my $x = 5) { $c }; f()])), 5,
    'a declaration as a native parameter default gives its value';
throws-like q[sub f(Str $c = my $x = 5) { $c }; f()], X::TypeCheck::Binding::Parameter,
    'a declaration as a typed parameter default is type checked when it is evaluated';
is (try ([||] 1, (my $z = die "evaluated"))), 1,
    'a declaration in a reduce with || is not evaluated after a true operand';
{
    my $i = 0;
    is-deeply ([xx] (my $x = ++$i), 3).List, (1, 2, 3),
        'a declaration in a reduce with xx is evaluated for each repetition';
}
is (try ([andthen] 42, Block)), Block,
    'a Callable type object in a reduce with andthen gives the type object';
lives-ok { 5 orelse my $x = die "evaluated" },
    'a declaration right of orelse is not evaluated for a defined left side';
is (try (Any orelse Block)), Block,
    'a Callable type object right of orelse gives the type object';
is (try EVAL(q[sub f([$a, $b = my $y = 5]) { $b }; f([1])])), 5,
    'a declaration as a sub-signature parameter default is initialized';
is (try EVAL(q[sub f(Map $x = (my enum E <a b>)) { $x }; f().^name])), 'Map',
    'an enum declared as a parameter default gives its Map';
is (try EVAL(q[(42 andthen my class C does Callable { }).^name])), 'C',
    'a Callable class declared right of andthen gives the class';
is (try EVAL(q[sub f(Int $x = try 42) { $x }; f()])), 42,
    'a try as a typed parameter default gives the value of its statement';
is (try EVAL(q[sub f(Int $x = BEGIN 42) { $x }; f()])), 42,
    'a BEGIN as a typed parameter default gives the value of its statement';
{
    my ($a, $b = try 42) := \(1);
    is $b, 42, 'a try as a signature binding default gives the value of its statement';
}
{
    sub f([$a, $b = BEGIN 42]) { $b }
    is f([1]), 42, 'a BEGIN as a sub-signature parameter default gives the value of its statement';
}
{
    sub f($x = try 42) { $x }
    is &f.signature.params[0].default.(), 42,
        'a try as a parameter default introspects as code giving the value of its statement';
}
{
    my $n = 0;
    sub f($c = (0 || (my $x = ++$n))) { $c }
    is-deeply (f(), f()), (1, 2),
        'a constant || giving a declaration as a parameter default is evaluated on each call';
}
is-deeply (try EVAL(q[sub g($y) { my ($a, $b = (0 || { $y })) := \(1); $b() }; (g(1), g(2))])), (1, 2),
    'a constant || giving a block as a signature binding default gives a block for each bind';
is (try EVAL(q[sub f($x = (0 || (my enum F <a b>))) { $x }; f().^name])), 'Map',
    'a constant || giving an enum declaration as a parameter default gives its Map';
{
    my $i = 0;
    my @r = (5 if $i++ < 1) xx 3;
    is $i, 3, 'parenthesized statements with an if modifier on the left of xx are evaluated for each repetition';
}
{
    my $i = 0;
    my @r = (5 for ++$i) xx 3;
    is $i, 3, 'parenthesized statements with a for modifier on the left of xx are evaluated for each repetition';
}
{
    my $i = 0;
    Nil andthen (5 if $i++ < 1);
    is $i, 0, 'parenthesized statements with an if modifier right of andthen are not evaluated for an undefined left side';
}
{
    my $i = 0;
    is-deeply ((try ++$i) xx 3).List, (1, 2, 3),
        'a parenthesized try on the left of xx is evaluated for each repetition';
}
{
    my class C does Callable {
        method CALL-ME(|) { 'called' }
        method right-of-andthen() { 42 andthen self }
        method right-of-orelse() { Nil orelse self }
        method left-of-xx() { (self xx 2).List }
    }
    is (try C.new.right-of-andthen).^name, 'C',
        'self right of andthen in a method of a Callable class gives the invocant';
    is (try C.new.right-of-orelse).^name, 'C',
        'self right of orelse in a method of a Callable class gives the invocant';
    is-deeply (try C.new.left-of-xx).map(*.^name).List, ('C', 'C'),
        'self on the left of xx in a method of a Callable class gives the invocant for each repetition';
}
is-deeply (try EVAL(q[$_ = 1; my $r = 5 ~~ (my $y = $_); ($r, $y)])), (True, 5),
    'a declaration as the right side of a smartmatch sees the topic the smartmatch gives';
is (try EVAL(q[$_ = 1; 5 ~~ (try $_)])), True,
    'a try as the right side of a smartmatch sees the topic the smartmatch gives';
is (try EVAL(q[$_ = 1; 5 ~~ -> $a { $_ == 5 }])), True,
    'a block as the right side of a smartmatch sees the topic the smartmatch gives';
is (try EVAL(q[$_ = 1; 5 ~~ (5 if $_ == 5)])), True,
    'parenthesized statements with an if modifier as the right side of a smartmatch see the topic the smartmatch gives';
is (try EVAL(q[$_ = 1; (5, 5) ~~ (5 for ^$_.elems)])), True,
    'parenthesized statements with a for modifier as the right side of a smartmatch see the topic the smartmatch gives';
is (try EVAL(q[$_ = 1; 5 ~~ ((my $y = $_), 5); $y])), 5,
    'a list holding a declaration as the right side of a smartmatch sees the topic the smartmatch gives';
is (try EVAL(q[$_ = 1; my $seen; 5 ~~ my class C { $seen = $_ }; $seen])), 5,
    'a class declared as the right side of a smartmatch runs with the topic the smartmatch gives';
is-deeply (try EVAL(q[$_ = 1; 5 ~~ (my @a = $_); @a])), [5],
    'an array declaration as the right side of a smartmatch sees the topic the smartmatch gives';
is (try EVAL(q[$_ = 1; 5 ~~ (my %h = a => $_); %h<a>])), 5,
    'a hash declaration as the right side of a smartmatch sees the topic the smartmatch gives';
is-deeply (try EVAL(q[$_ = 1; my $r = 5 !~~ (my $y = $_); ($r, $y)])), (False, 5),
    'a declaration as the right side of a negated smartmatch sees the topic the smartmatch gives';
is (try EVAL(q[sub f($x = my class C { }) { $x }; f().^name])), 'C',
    'a class declared as a parameter default gives the class';
is (try EVAL(q[my ($a, $b = my class C { }) := \(1); $b.^name])), 'C',
    'a class declared as a signature binding default gives the class';
is (try EVAL(q[my $n = 0; sub f($x = my class C { $n++ }) { $x }; f(); f(); $n])), 2,
    'a class declared as a parameter default runs for each call';
is-deeply (try EVAL(q[sub g($y) { my ($a, &c = { $y }) := \(1); &c }; (g(1), g(2)).map({ $_() }).List])), (1, 2),
    'a block as a signature binding default gives a block for each bind';
is-deeply (try EVAL(q[sub g($y) { my ($a, $b = sub { $y }) := \(1); $b }; (g(1), g(2)).map({ $_() }).List])), (1, 2),
    'an anonymous sub as a signature binding default gives a sub for each bind';
is-deeply (try EVAL(q[sub f($y, [$a, &c = { $y }]) { &c }; (f(1, [0]), f(2, [0])).map({ $_() }).List])), (1, 2),
    'a block as a sub-signature parameter default gives a block for each call';
is-deeply (try EVAL(q[sub g($y) { my ($a, $p = :a{ $y }) := \(1); $p }; (g(1), g(2)).map({ .value.() }).List])), (1, 2),
    'a pair holding a block as a signature binding default gives a block for each bind';
is (try EVAL(q[my $n = 0; sub g { my ($a, $b = my class C { $n++ }) := \(1) }; g(); g(); $n])), 2,
    'a class declared as a signature binding default runs for each bind';
is (try EVAL(q[sub f([$a, $b = my class C { }]) { $b }; f([1]).^name])), 'C',
    'a class declared as a sub-signature parameter default gives the class';
is (try EVAL(q[role R[$t = my class C { }] { method m { $t.^name } }; R.new.m])), 'C',
    'a class declared as a role parameter default gives the class';
is (try EVAL(q[my class A { }; sub f(A $x = my class B is A { }) { $x }; f().^name])), 'B',
    'a class declared as a parameter default of a parent type gives the class';
is (try EVAL(q[(-> $x = my class C { } { $x })().^name])), 'C',
    'a class declared as a pointy block parameter default gives the class';
# These hold without the thunk too, and guard what it keeps.
todo 'binds in the frame of the thunk on the legacy frontend', 2 unless $rakuast;
is (try EVAL(q[1 andthen my ($a, $b) := (1, 2); $a + $b])), 3,
    'a signature declaration that binds right of andthen binds its variables';
is (try EVAL(q[Nil orelse my (:$a) := \(:a(5)); $a])), 5,
    'a signature declaration that binds right of orelse binds its variables';
is (try EVAL(q[1 orelse (my ($a, $b) := die "boom"); "end"])), 'end',
    'a parenthesized signature declaration that binds right of orelse is not evaluated for a defined left side';
is (try EVAL(q[my $n = 0; (my ($a, $b) := do { $n++; (1, 2) }) xx 0; $n])), 0,
    'a signature declaration that binds on the left of xx 0 is not evaluated';
{
    my \l = (1, 2) xx 2;
    ok l[0] =:= l[1], 'a list of constants on the left of xx is the same list for each repetition';
    my \e = () xx 2;
    ok e[0] =:= e[1], 'an empty list on the left of xx is the same list for each repetition';
    my \s = (1; 2) xx 2;
    ok s[0] =:= s[1], 'parenthesized statements of constants on the left of xx are the same list for each repetition';
}
{
    my class Callable { }
    constant F = &uc;
    ok (42 andthen F) === &uc, 'a lexical Callable does not change what andthen gives for code';
}
todo 'binds in the frame of the thunk on the legacy frontend' unless $rakuast;
is (try EVAL(q[1 andthen (my ($a, $b) := (1, 2)); $a + $b])), 3,
    'a parenthesized signature declaration that binds right of andthen binds its variables';
is (try (5 andthen :a{ $_ }).value.()), 5,
    'a pair holding a block right of andthen gives a block that sees the topic';
is-deeply ((:a{ $++ }) xx 3).map({ .value.() }).List, (0, 0, 0),
    'a pair holding a block on the left of xx gives a new block for each repetition';
is ((Empty) xx 3).elems, 0,
    'a parenthesized Slip on the left of xx is slipped for each repetition';
throws-like q[sub foo(Bool $b = sub { False }) { }], X::Parameter::Default::TypeCheck,
    'an anonymous sub as a parameter default of another type fails to compile';
throws-like q[sub f(Str $x = constant C = 5) { }], X::Parameter::Default::TypeCheck,
    'a constant declared as a parameter default of another type fails to compile';
is-deeply (try EVAL(q[my @r; for (int, num) -> $T { @r.push: array[$T] ~~ Positional[$T] }; @r])), [True, True],
    'a smartmatch against a parameterization with a loop variable is checked at runtime';
todo 'fails only when called on the legacy frontend', 2 unless $rakuast;
throws-like q[sub f(Int $x = my class C { }) { }], X::Parameter::Default::TypeCheck,
    'a class declared as a parameter default of another type fails to compile';
throws-like q[sub f(Int $x = (sub { })) { }], X::Parameter::Default::TypeCheck,
    'a parenthesized anonymous sub as a parameter default of another type fails to compile';
todo 'the where clause misses the variable of its block on the legacy frontend' unless $rakuast;
ok (try EVAL(q[sub f(&c = { my $n = 7; :($a where * eq $n) }) { c() }; \(7) ~~ f()])),
    'a signature literal in a block as a parameter default gives a signature that binds';
todo 'closes over the first call on the legacy frontend' unless $rakuast;
is-deeply (try EVAL(q[sub f($y, $x = (my sub g { $y })) { g() }; (f(1), f(2, 0), f(3, 0))])), (1, 2, 3),
    'a sub declared as a parameter default is called in the body of a call not evaluating the default';
is-deeply (try EVAL(q[sub f($x = CHECK (my sub g { 42 })()) { $x + g() }; (f(), f(1))])), (84, 43),
    'a sub declared under CHECK in a parameter default is called in the body';
# These hold whichever block declares the code, and guard what it keeps.
ok (try EVAL(q[my @s = ((try :($a, $b)) for 1..2); \(1, 2) ~~ @s[0]])),
    'a signature literal under try in a for modifier statement gives a signature that binds';
ok (try EVAL(q[my @s = ((gather take :($a, $b)) for 1..2); \(1, 2) ~~ @s[0][0]])),
    'a signature literal under gather in a for modifier statement gives a signature that binds';
is-deeply (try EVAL(q[sub g($i) { (my sub foo { $i }) xx 2; foo() }; (g(1), g(2), g(3))])), (1, 2, 3),
    'a sub declared on the left of xx closes over each call of its routine';
is-deeply (try EVAL(q[sub g($i) { my $k = 0; (my sub foo { $i * 10 } while $k++ < 1); foo() }; (g(1), g(2), g(3))])), (10, 20, 30),
    'a sub declared as a while modifier statement closes over each call of its routine';
is-deeply (try EVAL(q[constant X = ((my sub foo { 42 }; foo() + 1) xx 2).List; X.map(*[1]).List])), (43, 43),
    'a sub declared on the left of xx in the value of a constant is called';
is (try EVAL(q[constant K = * + ((my sub foo { 7 }; foo()))[*-1]; K.(1)])), 8,
    'a sub declared in a Whatever curry as the value of a constant is called';
is-deeply (try EVAL(q[my $x is default(((my sub foo { 42 }; foo()) xx 2).List); $x.map(*[1]).List])), (42, 42),
    'a sub declared on the left of xx in a trait argument is called';
is (try EVAL(q[constant K = try (my sub foo { 7 }; foo())[1]; K])), 7,
    'a sub declared under try in the value of a constant is called';
is (try EVAL(q[BEGIN my sub foo { 42 }; foo()])), 42,
    'a sub declared under BEGIN is called';
is (try EVAL(q[CHECK my sub foo { 42 }; foo()])), 42,
    'a sub declared under CHECK is called';
is (try EVAL(q[sub g { (BEGIN my sub foo { 42 }; 1) xx 1; foo() }; g()])), 42,
    'a sub declared under BEGIN on the left of xx is called';
todo 'sees the first call on the legacy frontend', 2 unless $rakuast;
is-deeply (try EVAL(q[sub g($i) { my $r = (1 andthen ENTER $i * 10); $r }; (g(1), g(2))])), (10, 20),
    'an ENTER phaser right of andthen sees each call of its routine';
is (try EVAL(q[my $s = ''; sub g($i) { 1 andthen PRE { $s ~= "p$i "; True }; 1 }; g(1); g(2); $s])), 'p1 p2 ',
    'a PRE phaser right of andthen sees each call of its routine';
todo 'misses the loop variable on the legacy frontend' unless $rakuast;
is (try EVAL(q[my $s = ''; for 1..3 -> $i { 1 andthen LAST $s ~= "l$i" }; $s])), 'l3',
    'a LAST phaser right of andthen sees the last iteration';
is (try EVAL(q[my $s = ''; sub g($i) { (ENTER $s ~= "e$i "; 1) xx 2; 1 }; g(1); g(2); $s])), 'e1 e2 ',
    'an ENTER phaser on the left of xx sees each call of its routine';
is (try EVAL(q[my $s = ''; for 1..3 -> $i { (LAST $s ~= "l$i"; 1) xx 1 }; $s])), 'l3',
    'a LAST phaser on the left of xx sees the last iteration';
is (try EVAL(q[my $s = ''; sub g($i) { (my $x will enter { $s ~= "e$i " }) xx 1; 1 }; g(1); g(2); $s])), 'e1 e2 ',
    'a will enter trait on the left of xx sees each call of its routine';
is (try EVAL(q[my $s = ''; for 1..3 -> $i { (my $x will last { $s ~= "l$i" }) xx 1 }; $s])), 'l3',
    'a will last trait on the left of xx sees the last iteration';
todo 'gives Mu on the legacy frontend', 2 unless $rakuast;
is-deeply (try EVAL(q[sub g($i) { (once $i) xx 1 }; (g(1), g(2)).map(*.List).List])), ((1,), (1,)),
    'a once on the left of xx runs once per closure of its routine';
is-deeply (try EVAL(q[my $n = 0; my &f = -> $x = once ++$n { $x }; (f(), f(), f())])), (1, 1, 1),
    'a once as a pointy block parameter default runs once per closure of the block';
if $rakuast {
    is-deeply (try EVAL(q[my $n = 0; sub f($x = once ++$n) { $x }; (f(), f(), f())])), (1, 1, 1),
        'a once as a parameter default runs once per closure of its routine';
}
else {
    skip 'dies on the legacy frontend', 1;
}
if $rakuast {
    is (try EVAL(q[constant K = * + once 5; K.(1)])), 6,
        'a once in a Whatever curry as the value of a constant gives its value';
}
else {
    skip 'dies on the legacy frontend', 1;
}
if $rakuast {
    is-deeply (try EVAL(q[my $n = 0; sub g { * + once ++$n }; (g()(1), g()(1))])), (2, 3),
        'a once in a Whatever curry runs once per closure of the curry';
}
else {
    skip 'dies on the legacy frontend', 1;
}
is (try EVAL(q[my $s = ''; sub g($i) { (ENTER { $s ~= "e$i " }; 1) xx 2; 1 }; g(1); g(2); $s])), 'e1 e2 ',
    'an ENTER block on the left of xx sees each call of its routine';
is (try EVAL(q[my $s = ''; sub g($i) { for 1..2 { (NEXT { $s ~= "n$i " }; 1) xx 1 } }; g(1); g(2); $s])), 'n1 n1 n2 n2 ',
    'a NEXT block on the left of xx sees each call of its routine';
is-deeply (try EVAL(q[sub g($i) { (ENTER $i * 10) xx 1 }; BEGIN g(1); g(2)])), (20,),
    'an ENTER phaser on the left of xx in a routine called at BEGIN time gives its value';
todo 'gives nothing on the legacy frontend' unless $rakuast;
is (try EVAL(q[my $s = ''; sub g($i) { for 1..2 { $s ~= (FIRST $i) xx 1 } }; g(1); g(2); $s])), '1122',
    'a FIRST phaser on the left of xx sees each call of its routine';
is (try EVAL(q[sub g($i) { my $s = "x"; 1 andthen $s ~~ s[x] = (my sub f { "r$i" })(); $s }; g(1) ~ g(2)])), 'r1r2',
    'a sub declared in a substitution right of andthen closes over each call of its routine';
is (try EVAL(q[my $s = ''; sub g($i) { ("x" ~~ / :my $y will leave { $s ~= "l$i" } = 1; x /) xx 1; 1 }; g(1); g(2); $s])), 'l1l2',
    'a will leave trait in a regex on the left of xx runs';
is-deeply (try EVAL(q[sub g($i) { * + (my sub foo { $i })() }; my $a = g(1); my $b = g(2); ($a(10), $b(10))])), (11, 12),
    'a sub declared in a Whatever curry closes over the call that made the curry';
is-deeply (try EVAL(q[sub g($i) { gather take my sub foo { $i } }; my $a = g(1); my $b = g(2); g(3); ($a[0](), $b[0]())])), (1, 2),
    'a sub declared under a lazy gather closes over the call that made the gather';
is (try EVAL(q[my $s = ''; sub g($i) { * ~~ (my token tk { x { $s ~= $i } }) }; my $a = g(1); my $b = g(2); $a("x"); $b("x"); $s])), '12',
    'a token declared in a Whatever curry closes over the call that made the curry';
throws-like q[sub g { my $v = sub foo {...}; sub foo { "real" }; $v() }; g()], X::StubCode,
    'the value of a stub declaration is the stub';
is (try EVAL(q[constant C = (FIRST my sub d { 5 }; d()); C[1]])), 5,
    'a sub declared under FIRST in the value of a constant is called';
is (try EVAL(q[my $x = BEGIN (FIRST my sub d { 5 }; d()); $x[1]])), 5,
    'a sub declared under FIRST under BEGIN is called';
is (try EVAL(q[my @s; multi trait_mod:<will>(Routine:D $r, $b, :$foo!) { @s.push: $b }; my $c = sub () will foo { "f" } { }; @s[0]()])), 'f',
    'a will trait on an anonymous sub runs its block';
is (try EVAL(q[multi trait_mod:<will>(Routine:D $r, $b, :$foo!) { $r.wrap(-> |c { $b() ~ callsame }) }; my class K { method m will foo { "p-" } { "m" } }; K.m])), 'p-m',
    'a will trait on a method runs its block';
is (try EVAL(q[my @s; multi trait_mod:<will>(Routine:D $r, $b, :$foo!) { @s.push: $b }; multi g(Int) will foo { "f" } { }; @s[0]()])), 'f',
    'a will trait on a multi candidate runs its block';
is (try EVAL(q[my @s; multi trait_mod:<will>(Routine:D $r, $b, :$foo!) { @s.push: $b }; my grammar G { token t will foo { "f" } { x } }; @s[0]()])), 'f',
    'a will trait on a token runs its block';
is (try EVAL(q[my $t; multi trait_mod:<is>(Mu:U $c, :$tagged!) { $t = $tagged }; my class C is tagged(sub g { 42 }) { }; $t()])), 42,
    'a sub declared in a trait argument of a class is called';
is (try EVAL(q[my role P[&c] { method pm { c() } }; my class C does P[sub g { 42 }] { }; C.pm])), 42,
    'a sub declared in a role argument of a class is called';
is-deeply (try EVAL(q[my role P[&c] { method pm { c() } }; sub f($v) { my class C does P[sub g { $v }] { }; C.pm }; (f(1), f(2))])), (1, 2),
    'a sub declared in a role argument of a class in a routine closes over each call of the routine';
is (try EVAL(q[sub f($p) { (multi g(Int) { "i$p" }) xx 0; g(1) }; f(1) ~ f(2)])), 'i1i2',
    'a multi declared on the left of xx 0 closes over each call of its routine';
is-deeply (try EVAL(q[sub g($i) { *.&(my multi mf($x) { $x + $i }) }; my $a = g(1); my $b = g(2); ($a(10), $b(10))])), (11, 12),
    'a multi declared in a Whatever curry closes over the call of its routine that made the curry';
is-deeply (try EVAL(q[sub g($i) { sub (\d = ((my multi mm { $i * 10 }; mm()))) { d[1] }() }; (g(1), g(2))])), (10, 20),
    'a multi declared in a parameter default closes over each call of the routine around';
todo 'the where clause misses the variable of its block on the legacy frontend' unless $rakuast;
ok (try EVAL(q[my @r = { my $n = 7; :($a where * eq $n) } xx 2; \(7) ~~ @r[0]()])),
    'a signature literal in a block on the left of xx gives a signature that binds';
is (try EVAL(q[constant K = try 42; K])), 42,
    'a try as the value of a constant gives the value of its statement';
is-deeply (try EVAL(q[sub g($i) { my ($q, $d = ((my method m { $i * 10 }; 5.&m))) := \(1); $d[1] }; (g(1), g(2), g(3))])), (10, 20, 30),
    'a lexical method declared in a default of a signature declaration closes over each call of its routine';
is-deeply (try EVAL(q[sub g($i) { my ($q, $d = ((my regex rr { $i }; ~("x{$i}y" ~~ &rr)))) := \(1); $d[1] }; (g(1), g(2), g(3))])), ('1', '2', '3'),
    'a regex declared in a default of a signature declaration closes over each call of its routine';
is-deeply (try EVAL(q[sub f($x where BEGIN (my sub w($v) { $v > 0 })) { $x }; (f(1), (try f(-1)) // 'rej')])), (1, 'rej'),
    'a sub declared under BEGIN as the where of a parameter is smartmatched';
if $rakuast {
    is-deeply EVAL(q[sub f($i) { :($a, $b where (my sub w($v) { $v == $i })($_)) }; ((\(1, 1) ~~ f(1)), (\(1, 2) ~~ f(2)))]), (True, True),
        'a sub declared in the where of a signature literal closes over the call of its routine that made the literal';
}
else {
    skip 'dies on the legacy frontend', 1;
}
sub routine-in-default($y, $x = (sub g(Int) { "d$y" })(1)) { $x }
BEGIN routine-in-default(1);
is routine-in-default(2), 'd2',
    'a sub declared in a parameter default closes over each call of its routine after a call at BEGIN time';
sub routine-in-default-and-body($y, $x = (my sub g(Int) { "d$y" })(1)) { $x ~ g(1) }
BEGIN routine-in-default-and-body(1);
is routine-in-default-and-body(2), 'd2d2',
    'a sub declared in a parameter default and called in the body closes over each call of its routine after a call at BEGIN time';
sub multi-in-default($y, $x = (my multi g(Int) { "m$y" })(1)) { $x }
BEGIN multi-in-default(1);
is multi-in-default(2), 'm2',
    'a multi declared in a parameter default closes over each call of its routine after a call at BEGIN time';
todo 'closes over an earlier bind on the legacy frontend' unless $rakuast;
is-deeply (try EVAL(q[sub f($y, $x where (my sub w($v) { $v > $y })($_)) { 1 }; (\(1, 2) ~~ &f.signature, \(5, 2) ~~ &f.signature)])), (True, False),
    'a sub declared in the where of a parameter closes over the frame of a trial bind';
is-deeply (try EVAL(q[sub f($y, Int $x = (my sub g { $y > 3 ?? "s" !! 1 })()) { 1 }; (\(1) ~~ &f.signature, \(5) ~~ &f.signature)])), (True, False),
    'a sub declared in a parameter default closes over the frame of a trial bind';
todo 'closes over an earlier bind on the legacy frontend' unless $rakuast;
is-deeply (try EVAL(q[multi f($y, $x where (my sub w($v) { $v > $y })($_)) { "a" }; multi f($y, $x) { "b" }; (&f.cando(\(1, 2)).elems, &f.cando(\(5, 2)).elems)])), (2, 1),
    'a sub declared in the where of a multi candidate closes over the frame of the trial bind of cando';
is (try EVAL(q[role RV[$p, $q where (my sub w($v) { $v > $p })($_)] { method m { "a" } }; role RV[$p, $q] { method m { "o" } }; ((5 but RV[1, 2]).m, (5 but RV[5, 2]).m, (5 but RV[1, 2]).m).join])), 'aoa',
    'a sub declared in the where of a role parameter closes over the frame of each variant selection';
if $rakuast {
    is-deeply EVAL(q[my class P { has $.v; method COERCE(Int $v where (my sub w($x) { $v > 0 })($_)) { self.new(:$v) } }; sub f(P() $p) { $p.v }; (f(5), (try f(-5)) // 'rej', f(6))]), (5, 'rej', 6),
        'a sub declared in the where of a COERCE method closes over the frame of the trial bind of a coercion';
}
else {
    skip 'dies on the legacy frontend', 1;
}
todo 'closes over the first instantiation on the legacy frontend' unless $rakuast;
is-deeply (try EVAL(q[role RM[$p, $q = (my multi g(Int) { $p })(1)] { method m { $q ~ g(1) } }; ((5 but RM[1]).m, (6 but RM[2]).m)])), ('11', '22'),
    'a multi declared in a role parameter default closes over each instantiation of the role';
if $rakuast {
    is EVAL(q[role RS[$p, ($a, $b where (my sub w($v) { $v > $p })($_))] { method m { "a" } }; role RS[$p, $q] { method m { "o" } }; ((1 but RS[1, (1, 2)]).m, (1 but RS[5, (1, 2)]).m).join]), 'ao',
        'a sub declared in the where of a role parameter sub-signature closes over the frame of each variant selection';
}
else {
    skip 'dies on the legacy frontend', 1;
}
is (try EVAL(q[role RD[$p, ($a, Int $b = (my sub g { $p > 3 ?? "s" !! 1 })())] { method m { "a" } }; role RD[$p, $q] { method m { "o" } }; ((1 but RD[5, (1,)]).m, (1 but RD[1, (1,)]).m).join])), 'oa',
    'a sub declared in a default of a role parameter sub-signature closes over the frame of each variant selection';
is-deeply (try EVAL(q[my $c; multi trait_mod:<is>(Routine $r, :$checked!) { $c = $r.signature.params[0].constraint_list[0](5) }; sub f($x where (my sub w($v) { $v > 0 })($_)) is checked { 1 }; ($c, f(1))])), (True, 1),
    'a where holding a sub called by a trait of its routine at BEGIN time is smartmatched';
{
    my $c;
    multi trait_mod:<is>(Routine $r, :$checked!) { $c = $r.signature.params[1].constraint_list[0](5) }
    sub checked-where($y, $x where (my sub w($v) { $v > ($y // 0) })($_)) is checked { 1 }
    todo 'closes over the call at BEGIN time on the legacy frontend' unless $rakuast;
    is-deeply ($c, (try checked-where(-3, -1)), (try checked-where(5, 2)) // 'rej'), (True, 1, 'rej'),
        'a where holding a sub called by a trait at BEGIN time closes over the frame bound at runtime';
}
{
    sub default-at-begin($y, $x = (my sub g { "g" ~ ($y // "U") })()) { $x }
    BEGIN &default-at-begin.signature.params[1].default.();
    todo 'closes over the call at BEGIN time on the legacy frontend' unless $rakuast;
    is default-at-begin(1), 'g1',
        'a default holding a sub called at BEGIN time closes over the frame bound at runtime';
}
if $rakuast {
    is EVAL(q[role RB[$p, $b = (my sub g { "g" ~ ($p // "U") })()] { method m { $b } }; BEGIN RB.^candidates[0].^body_block.signature.params[2].default.(); (1 but RB[5]).m]), 'g5',
        'a role parameter default holding a sub called at BEGIN time is compiled on its own';
    is EVAL(q[role RW[$p] { method m($y, $x where (my sub w($v) { $v > ($y // 0) })($_)) { "ok" } }; BEGIN RW.^candidates[0].^method_table<m>.signature.params[2].constraint_list[0](5); my class A does RW[1] { }; A.m(-3, -1)]), 'ok',
        'a role method where holding a sub called at BEGIN time is compiled on its own';
}
else {
    skip 'dies on the legacy frontend', 2;
}
is (try EVAL(q[sub f($x = &?BLOCK.^name) { $x }; BEGIN f(); f()])), 'Code',
    'a default reading &?BLOCK in a routine called at BEGIN time gives the code object';
{
    sub where-at-begin($y, $x where (my sub w($v) { $v > ($y // 0) })($_)) { 'ok' }
    BEGIN &where-at-begin.signature.params[1].constraint_list[0](5);
    BEGIN where-at-begin(1, 2);
    is-deeply ((try where-at-begin(-3, -1)), (try where-at-begin(5, 1)) // 'rej'), ('ok', 'rej'),
        'a where holding a sub called at BEGIN time and then by a call at BEGIN time closes over the frame bound at runtime';
}
