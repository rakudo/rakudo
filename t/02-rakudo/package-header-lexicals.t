use Test;
use nqp;

plan 98;

my $rakuast := nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';

my multi trait_mod:<is>(Mu:U $c, :$header-cb!) { $c.^add_method("cb", method { $header-cb() }) }
my class TopOfUnit is header-cb(-> { $?PACKAGE.^name }) { }
is TopOfUnit.cb, 'GLOBAL',
    'a compile-time variable in a closure in a trait argument of a class at the top of a unit is that of the unit';
my role HeaderRole { my class Inner is header-cb(-> { $?CLASS.^name ~ ::?CLASS.^name }) { }; method m { Inner.cb } }
my class HeaderRoleUser does HeaderRole { }
is HeaderRoleUser.m, 'HeaderRoleUserHeaderRoleUser',
    'the class of a role in a closure in a trait argument of a class in the role is that of the last specialization of the role';
my multi trait_mod:<is>(Mu:U $c, :$header-tag!) { }
my role StubHeaderRole { my class Inner is header-tag(my $v = 5) is header-cb(-> { $?CLASS.^name ~ $v }) { ... }; my class Inner { }; method m { Inner.cb } }
my class StubHeaderRoleUser does StubHeaderRole { }
if $rakuast {
    is StubHeaderRoleUser.m, 'StubHeaderRoleUser5',
        'the class of a role in a closure in a trait argument of a class stub in the role is that of the last specialization of the role';
}
else {
    skip 'the legacy frontend reads the generic class', 1;
}

is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; my class C is tagged(my sub s { 7 }) { method m { s() } }; C.m])), 7,
    'a sub declared in a trait argument of a class is seen by its methods';
is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; my class C is tagged(my multi sub f(Int) { 6 }) { method m { f(1) } }; C.m])), 6,
    'a multi sub declared in a trait argument of a class is seen by its methods';
is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; my class C is tagged(my $v = 5) { method m { $v } }; C.m])), 5,
    'a variable declared in a trait argument of a class is seen by its methods with its value';
is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; my class C is tagged(my constant K = 5) { method m { K } }; C.m])), 5,
    'a constant declared in a trait argument of a class is seen by its methods';
is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; my class C is tagged(my enum E <ea eb>) { method m { eb.value } }; C.m])), 1,
    'an enum declared in a trait argument of a class is seen by its methods';
is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; my $got; my class C is tagged(my constant K = 5) { $got = K }; $got])), 5,
    'a constant declared in a trait argument of a class is seen by its body';
is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; my class C is tagged(my constant K = 5) is tagged(my role R1 { method r { K } }) { method m { R1.r } }; C.m])), 5,
    'a role declared in a trait argument of a class sees a constant declared in another';
is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; my module M is tagged(my constant K = 5) { our sub m { K } }; M::m()])), 5,
    'a constant declared in a trait argument of a module is seen by its subs';
is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; my grammar G is tagged(my constant K = "k") { token TOP { <{ K }> } }; ?G.parse("k")])), True,
    'a constant declared in a trait argument of a grammar is seen by its tokens';
is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; my role R2 is tagged(my constant K = 5) { method m { K } }; my class D does R2 { }; D.m])), 5,
    'a constant declared in a trait argument of a role is seen by its methods';
is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; my role R3 is tagged(my $v = 5) { method m { $v } }; my class D does R3 { }; D.m])), 5,
    'a variable declared in a trait argument of a role is seen by its methods with its value';
is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; my role R4[$t] is tagged(my $v = 5) { method m { $v ~ $t } }; (1 but R4[2]).m])), '52',
    'a variable declared in a trait argument of a parametric role is seen by a runtime specialization';
is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; my role R5 is tagged(my class E { method x { 3 } }) { method m { E.x } }; my class D does R5 { }; D.m])), 3,
    'a class declared in a trait argument of a role is seen by its methods';
is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; sub f($n) { my class C is tagged(my $v = 5) { method m { $v++ } }; C.m }; f(1) ~ f(2)])), '55',
    'a variable declared in a trait argument of a class in a routine starts with its value on each call';
is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; sub f($n) { my class C is tagged(my sub s { $n }) { method m { s() } }; C.m }; f(1) ~ f(2)])), '12',
    'a sub declared in a trait argument of a class in a routine sees the lexicals of each call';
is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; sub f($n) { my class C is tagged(my sub s { $n }) { has $.a = s() }; C.new.a }; f(1) ~ f(2)])), '12',
    'a sub declared in a trait argument of a class is seen by an attribute default with the lexicals of each call';
is-deeply (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; my @r; (1, 2) ==> map({ (my class C is tagged(my $v = 7) { method m { $v } }).m + $_ }) ==> @r; @r])), [8, 9],
    'a variable declared in a trait argument of a class in a stage of a feed is seen by its methods';
is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; sub f($x) { $x || (my class C is tagged(my $v = 7) { method m { $v } }).m }; f(0)])), 7,
    'a variable declared in a trait argument of a class in a thunk is seen by its methods';
is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; my $n = 0; sub g { my class D is tagged(my $v = ++$n) { method m { $v } }; D.m }; g() ~ g()])), '11',
    'a trait argument of a class runs once, at compile time, and not on each evaluation of the class';
is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; (my class :: is tagged(my $v = 5) { method m { $v } }).m])), 5,
    'a variable declared in a trait argument of an anonymous class is seen by its methods';
is-deeply (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; ((my class C is tagged(my $k = 1) { method m { $k++ } }).m xx 3).List])), (1, 1, 1),
    'a variable declared in a trait argument of a class at the top of a unit starts with its value on each evaluation';
is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; ((my class C is tagged(my $v = 5) { method m { $v } }) xx 2).map(*.m).join])), '55',
    'a variable declared in a trait argument of a class on the left of xx is seen by its methods';
is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; (my class C is tagged(my $v = 5) { method m { $v } } xx 2).map(*.m).join])), '55',
    'a variable declared in a trait argument of a class without parentheses on the left of xx is seen by its methods';
is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; constant X = my class C is tagged(my $v = 5) { method m { $v } }; X.m])), 5,
    'a variable declared in a trait argument of a class as the value of a constant is seen by its methods';
is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; my $v = 1; my class C is tagged(my $v = 5) { method m { $v } }; C.m ~ $v])), '51',
    'a variable declared in a trait argument of a class does not clash with one of the same name around it';
{
    my $got;
    try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; unit class UC is tagged(my $v = 5) is tagged(my sub s { 6 }); method m { $v ~ s() }; $got = UC.m]);
    is $got, '56', 'a variable and a sub declared in trait arguments of a unit scoped class are seen by its methods';
}
{
    my $got;
    try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; unit module UM is tagged(my $v = 5); $got = $v]);
    is $got, 5, 'a variable declared in a trait argument of a unit scoped module is seen by its body';
}
is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; Q[my class C is tagged(my $v = 5) { method m { $v } }; C.m].AST.EVAL])), 5,
    'a variable declared in a trait argument of a class compiled from an AST is seen by its methods';
is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, Mu :$tagged!) { }; my class C is tagged(my class D { ... }) { my class D { method x { 3 } }; method m { D.x } }; C.m])), 3,
    'a class stubbed in a trait argument of a class is defined by its body';
is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, Mu :$tagged!) { }; my class C is tagged(my class D { ... }) { my class D { }; method m { D.^name } }; C.m])), 'D',
    'a class stubbed in a trait argument of a class and defined by its body keeps the name of the stub';
is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, Mu :$tagged!) { }; my class C is tagged(our class HeaderStubOD { ... }) { our class HeaderStubOD { } }; GLOBAL::HeaderStubOD.^name ~ C::.keys.elems])), 'HeaderStubOD0',
    'an our class stubbed in a trait argument of a class and defined by its body stays in the package around the class';
is (try EVAL(q[my role Rx[::T] { method t { T.^name } }; my class O { my class C does Rx[$?CLASS] { }; method t { C.t } }; O.t])), 'O',
    'the class in a trait argument of a class declared in a class is the class around it';
is (try EVAL(q[my role Rq[$t] { }; my class C does Rq[my sub s { 5 }] { method m { s() } }; C.m])), 5,
    'a sub declared in the argument of a role a class does is seen by its methods';
is (try EVAL(q[my @k; my multi trait_mod:<is>(Mu:U $c, Mu :$keep!) { @k.push: $keep }; my role O { my role I is keep($?ROLE) { } }; @k[0].^name])), 'O',
    'the role in a trait argument of a role declared in a role is the role around it';
is (try EVAL(q[my @k; my multi trait_mod:<is>(Mu:U $c, Mu :$keep!) { @k.push: $keep }; my module O { my module I is keep($?MODULE) { } }; @k[0].^name])), 'O',
    'the module in a trait argument of a module declared in a module is the module around it';
is (try EVAL(q[Q[my role Rx[::T] { method t { T.^name } }; my class O { my class C does Rx[$?CLASS] { }; method t { C.t } }; O.t].AST.EVAL])), 'O',
    'the class in a trait argument of a class declared in a class compiled from an AST is the class around it';
{
    try EVAL q[my multi trait_mod:<is>(Mu:U $c, Mu :$tagged!) { }; my class C is tagged($?CLASS) { }];
    isa-ok $!, X::Undeclared, 'the class of a class is not yet a variable in its own trait argument';
}
throws-like q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; my class C is tagged(-> { $?CLAS }) { }], X::Undeclared, suggestions => [],
    'an undeclared compile-time variable in a trait argument of a class is not offered the class of the class';
is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, Mu :$tagged!) { }; for ^2 { my class D is tagged(FIRST 1) { } }; 'ok'])), 'ok',
    'a FIRST phaser in a trait argument of a class compiles';
is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, Mu :$tagged!) { }; my class C is tagged(my role D { ... }) { my role D { }; method m { D.^name } }; C.m])), 'D',
    'a role stubbed in a trait argument of a class and defined by its body keeps the name of the stub';
is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; my $y = 1; my class C is tagged(my sub s { $y }) { my $y = 2; method m { s() } }; C.m])), 2,
    'a sub declared in a trait argument of a class sees a variable its body declares, as one in a routine trait does';
is (try EVAL(q[my @s; my multi trait_mod:<is>(Mu:U $c, :$tagged!) { @s.push($tagged) }; my class C is tagged(my proto f(|) {*}) { my multi f(Str) { 2 } }; @s[0].candidates.elems])), 1,
    'a multi candidate declared in the body of a class joins a proto declared in its trait argument';
is (try EVAL(q[use fatal; my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; my class C is tagged(my multi f(Int) { 1 }) { my multi f(Str) { 2 }; method m { f(1) ~ f("a") } }; C.m])), '12',
    'multi candidates declared in a trait argument and the body of a class are no redeclaration';
is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$ev!) { EVAL q[{ 1 }] }; my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; my class C is tagged(my proto f(|) {*}) is ev { my multi f(Str) { 2 }; method m { f("a") } }; C.m])), 2,
    'a multi candidate declared in the body of a class joins a proto declared in its trait argument when another trait compiles code';
is (try EVAL(q[my $got; my multi trait_mod:<is>(Mu:U $c, :$ev!) { $got = EVAL q[my $x = 1; sub s { my $x = 2; $x }; s()] }; my class C is ev { }; $got])), 2,
    'a block in code a trait of a class compiles has a scope of its own';
is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; my class C is tagged(my $v = 5) { method m { OUTER::<$v> } }; C.m])), 5,
    'a variable declared in a trait argument of a class is in the outer scope of its methods';
is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; my role R9[$p] is tagged(my $v = 5) { $v += $p; method r { $v } }; my class X does R9[1] { }; my class Y does R9[2] { }; X.r ~ Y.r])), '67',
    'a variable declared in a trait argument of a role is fresh in each specialization';

is-deeply (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; my @l; sub f($n) { f($n - 1) if $n > 0; my class C is tagged(my $k = 1) { @l.push($n + $k) } }; f(2); @l])), [1, 2, 3],
    'the body of a class with a trait argument declaring a variable sees the lexicals of each call of a recursive routine';
is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; my $b = BEGIN do { my class C is tagged(my $v = 5) { method m { $v } }; C.m }; $b])), 5,
    'a variable declared in a trait argument of a class declared under BEGIN is seen by its methods';
is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; my role R6 { method r { my class C is tagged(my $v = 5) { method m { $v } }; C.m } }; (class :: does R6 { }).r])), 5,
    'a variable declared in a trait argument of a class declared in a role method is seen by its methods';
is (try EVAL(q[my $out = ''; my multi trait_mod:<is>(Mu:U $c, :$cb!) { $c.^add_method("cb", method { $cb() }) }; my class C is cb(-> { (1..3).map({ $out ~= $_ }); 1 }) { }; C.cb; $out])), '123',
    'a statement in a block in a trait argument of a class is sunk';
is (try EVAL(q[my $got; my multi trait_mod:<is>(Mu:U $c, :$cb!) { $c.^add_method("cb", method { $cb() }) }; my module MH { my class D is cb(-> { $?PACKAGE.^name }) { }; $got = D.cb }; $got])), 'MH',
    'a compile-time variable in a closure in a trait argument of a class is that of the scope around the class';
is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$cb!) { $c.^add_method("cb", method { $cb() }) }; my class C is cb(-> { $?PACKAGE.^name ~ ::?PACKAGE.^name }) { }; C.cb])), 'GLOBALGLOBAL',
    'a compile-time variable in a closure in a trait argument of a class in EVAL code is that of the code around the EVAL';
is (try EVAL(q[my role Rx14[::T] { method t { T.^name } }; my class C does Rx14[$?PACKAGE] { }; C.t])), 'GLOBAL',
    'a compile-time variable in a trait argument of a class in EVAL code is that of the code around the EVAL';
throws-like q[my multi trait_mod:<is>(Mu:U $c, Mu :$tagged!) { }; my class K is tagged(::?CLASS) { }], X::NoSuchSymbol,
    'the class of a class is not yet a name in its own trait argument';
is (try EVAL(q[my @k; my multi trait_mod:<is>(Mu:U $c, :$keep!) { @k.push: $keep }; sub f { my class D is keep(-> { $?PACKAGE.^name }) { } }; @k[0]()])), 'GLOBAL',
    'a compile-time variable in a closure in a trait argument of a class in EVAL code is that of the code around the EVAL before the class runs';

# These guard the frame that holds what the header of a package declares, and
# what sees it.
is (try EVAL(q[my $got; my $x = 5; my class C { has $.v = 1 + 1; my $x = 6; $got = OUTER::<$x> }; $got])), 5,
    'the body of a class with an attribute default has the scope around the class as its outer';
is (try EVAL(q[my $x is dynamic = 5; my $got; my class C { has $.v = 1 + 1; $got = CALLER::<$x> }; $got])), 5,
    'the body of a class with an attribute default has the scope around the class as its caller';
is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; my $got; my $x = 5; my class C is tagged(my constant K = 1) { my $x = 6; $got = OUTER::<$x> }; $got])), 5,
    'the body of a class with a trait argument declaring a constant has the scope around the class as its outer';
is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; my $x is dynamic = 5; my $got; my class C is tagged(my constant K = 1) { $got = CALLER::<$x> }; $got])), 5,
    'the body of a class with a trait argument declaring a constant has the scope around the class as its caller';
is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; my $got; sub f { my class C is tagged(my constant K = 1) { $got = callframe(1).code.name } }; f(); $got])), 'f',
    'the frame calling the body of a class with a trait argument declaring a constant is the routine around the class';
is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; my class C is tagged(my constant K = 5) { has $.a = K }; C.new.a])), 5,
    'a constant declared in a trait argument of a class is seen by an attribute default';
is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; my class C is tagged(my constant K = 5) { method m { K } }; my $b = BEGIN C.m; $b])), 5,
    'a constant declared in a trait argument of a class is seen by a method called at BEGIN time';
if $rakuast {
    is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; my role R7 is tagged(my sub s { 7 }) { method m { s() } }; my class D does R7 { }; D.m])), 7,
        'a sub declared in a trait argument of a role is seen by its methods';
    is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; my role R8 is tagged(my multi sub f(Int) { 6 }) { method m { f(1) } }; my class D does R8 { }; D.m])), 6,
        'a multi sub declared in a trait argument of a role is seen by its methods';
    is (try EVAL(q[my $out = ''; my multi trait_mod:<is>(Mu:U $c, :$cb!) { $c.^add_method("cb", method { $cb() }) }; my role R10 is cb(-> { (1..3).map({ $out ~= $_ }); 1 }) { }; my class D does R10 { }; D.cb; $out])), '123',
        'a statement in a block in a trait argument of a role is sunk';
    is (try EVAL(q[my $got; my role R15 { BEGIN { my @k; my multi trait_mod:<is>(Mu:U $c, :$keep!) { @k.push: $keep }; my class C is keep(-> { $?CLASS }) { }; $got = @k[0]().^name } }; $got])), '$?CLASS',
        'the class of a role in a closure in a trait argument of a class in a BEGIN block of the role is the generic class';
    is (try EVAL(q[my @k; my multi trait_mod:<is>(Mu:U $c, :$keep!) { @k.push: $keep }; my class D is keep(-> { $?PACKAGE.^name }) { }; BEGIN @k[0]()])), 'GLOBAL',
        'a compile-time variable in a closure in a trait argument of a class called at BEGIN time is that of the scope around the class';
    is (try EVAL(q[my @j; my multi trait_mod:<is>(Mu:U $c, :$keep!) { @j.push: $keep }; my module EMO { our sub e { EVAL q[my module Inner is keep(-> { $?MODULE.^name }) { }; 1]; @j[0]() } }; EMO::e()])), 'EMO',
        'the module around a module in EVAL code is what a closure in a trait argument of the module reads';
    is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; (my class O is tagged(my class I is tagged(my $v = 7) { method m { $v } }) { method m { I.m } }).m])), 7,
        'a variable declared in a trait argument of a class declared in a trait argument of a class is seen by its methods';
    is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; my multi trait_mod:<is>(Mu:U $c, :$cb!) { $c.^add_method("cb", method { $cb() }) }; my class O is tagged(my class I is cb(-> { $?PACKAGE.^name }) { }) { method m { I.cb } }; O.m])), 'GLOBAL',
        'a compile-time variable in a closure in a trait argument of a class in a trait argument of a class is that of the scope around both';
    is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; my role R16[$a] is tagged(my sub s { "one:$a" }) { method m { s() } }; my role R16[$a, $b] is tagged(my sub s { "two:$a$b" }) { method m { s() } }; my class X does R16[1] { }; my class Y does R16[1, 2] { }; X.m ~ "|" ~ Y.m])), 'one:1|two:12',
        'each candidate of a role group sees its own sub declared in a trait argument';
    is (try EVAL(q[use MONKEY-SEE-NO-EVAL; use experimental :rakuast; EVAL('use experimental :rakuast; ' ~ Q[my module M { my class D { ... }; my class D { method x { 5 } }; our sub m { D.x } }; M::m()].AST.raku).EVAL])), 5,
        'a class stubbed and then defined in the body of a module in a tree built from its raku has its methods';
    is (try EVAL(q[use MONKEY-SEE-NO-EVAL; use experimental :rakuast; my multi trait_mod:<is>(Mu:U $c, :$cb!) { $c.^add_method("cb", method { $cb() }) }; EVAL('use experimental :rakuast; ' ~ Q[my role R { my class C is cb(-> { $?CLASS.^name ~ ::?CLASS.^name }) { }; method r { C.cb } }; (1 but R).r].AST.raku).EVAL])), 'Int+{R}Int+{R}',
        'the class of a role in a closure in a trait argument of a class in the role in a tree built from its raku is that of the specialization';
    is (try EVAL(q[use MONKEY-SEE-NO-EVAL; use experimental :rakuast; EVAL('use experimental :rakuast; ' ~ Q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; my class C is tagged(my $v = 5) is tagged(my sub s { 6 }) { method m { $v ~ s() } }; C.m].AST.raku).EVAL])), '56',
        'a variable and a sub declared in trait arguments of a class in a tree built from its raku are seen by its methods';
    is (try EVAL(q[(1 but EVAL q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; unit role UR is tagged(my $v = 5) is tagged(my sub s { 6 }); method m { $v ~ s() }]).m])), '56',
        'a variable and a sub declared in trait arguments of a unit scoped role are seen by its methods';
    is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$cb!) { $c.^add_method("cb", method { $cb() }) }; my role R[$n] { my class C is cb(-> { $?CLASS.^name }) { }; method r { C.cb } }; (1 but R[1]).r.substr(0, 3) ~ (2 but R[2]).r.substr(0, 3) ~ ("a" but R[3]).r.substr(0, 3)])), 'IntIntStr',
        'the class of a role in a closure in a trait argument of a class in the role is that of the specialization last made at runtime';
    is (try EVAL(q[use MONKEY-SEE-NO-EVAL; use experimental :rakuast; my multi trait_mod:<is>(Mu:U $c, :$cb!) { $c.^add_method("cb", method { $cb() }) }; EVAL('use experimental :rakuast; ' ~ Q[my module MH { my class D is cb(-> { $?PACKAGE.^name }) { }; our sub g { D.cb } }; MH::g()].AST.raku).EVAL])), 'MH',
        'a compile-time variable in a closure in a trait argument of a class in a tree built from its raku is that of the scope around the class';
    is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; my role R11[$n] is tagged(my sub s { $n }) { method m { s() } }; my class X does R11[3] { }; my class Y does R11[4] { }; X.m ~ Y.m])), '34',
        'a sub declared in a trait argument of a role sees the parameter of each specialization';
    is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; my role R12[::T] is tagged(my sub s { T.^name }) { method m { s() } }; my class X does R12[Int] { }; my class Y does R12[Str] { }; X.m ~ Y.m])), 'IntStr',
        'a sub declared in a trait argument of a role sees the type capture of each specialization';
}
else {
    skip 'dies on the legacy frontend', 17;
}
is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; my class C is tagged(my $v = 5) { my $v = 6; method m { $v } }; C.m])), 6,
    'a variable declared in the body of a class takes the place of one of the same name declared in its trait argument';
throws-like q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; my class C is tagged(my sub s { 1 }) { my sub s { 2 } }], X::Redeclaration,
    'a sub declared in the body of a class redeclares one of the same name declared in its trait argument';
throws-like q[my multi trait_mod:<is>(Mu:U $c, Mu :$tagged!) { }; my class C is tagged(my enum ER <ra rb>) { my enum ER <rc rd> }], X::Redeclaration,
    'an enum declared in the body of a class redeclares one of the same name declared in its trait argument';
throws-like q[my multi trait_mod:<is>(Mu:U $c, Mu :$tagged!) { }; my class C is tagged(my constant KR = 1) { my class KR { } }], X::Redeclaration,
    'a class declared in the body of a class redeclares a constant of the same name declared in its trait argument';
throws-like q[my multi trait_mod:<is>(Mu:U $c, Mu :$tagged!) { }; my class C is tagged(my class StubbedInHeader { ... }) { }], X::Package::Stubbed,
    'a class stubbed in a trait argument of a class and never defined is reported';
is (try EVAL(q[my $r; my multi trait_mod:<is>(Mu:U $c, Mu :$tagged!) { $r = $tagged }; my class C is tagged(my role R { method x { 4 } }) { ... }; my class C { }; (1 but $r).x])), 4,
    'a role declared in a trait argument of a class stub has its methods';
is (try EVAL(q[my $r; my multi trait_mod:<is>(Mu:U $c, Mu :$tagged!) { $r = $tagged }; my role R is tagged(my role Q { method x { 4 } }) { ... }; my role R { }; (1 but $r).x])), 4,
    'a role declared in a trait argument of a role stub has its methods';
is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, Mu :$tagged!) { }; my multi trait_mod:<is>(Mu:U $c, :$cb!) { $c.^add_method("cb", method { $cb() }) }; my class C is tagged(my enum E <ea eb>) is cb(-> { eb.value }) { ... }; my class C { }; C.cb])), 1,
    'an enum value declared in a trait argument of a class stub is seen by a closure in another trait argument';
throws-like q[my multi trait_mod:<is>(Mu:U $c, :$cb!) { }; my role R is cb(-> { $?NOPE }) { }], X::Undeclared,
    'an undeclared compile-time variable in a closure in a trait argument of a role is reported';
throws-like q[my role R { method m { $?NOPE } }], X::Undeclared,
    'an undeclared compile-time variable in a method of a role is reported';
throws-like q[my role Rx[::T] { }; my role R does Rx[::?CLASS] { }], X::NoSuchSymbol,
    'the class of a role is not yet a name in its own trait argument';
throws-like q[my multi trait_mod:<is>(Mu:U $c, Mu :$tagged!) { }; my role R[::T] is tagged(my class T { }) { }], X::Redeclaration,
    'a class declared in a trait argument of a role redeclares a type capture of the same name in its signature';
throws-like q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; my $y = 1; my class C is tagged($y) { my $y = 2 }], X::Redeclaration::Outer,
    'a variable declared in the body of a class after a trait argument of the class used one of the same name from outside is an outer redeclaration';
throws-like q[no worries; my multi trait_mod:<is>(Mu:U $c, Mu :$tagged!) { }; my class C is tagged(my enum E1 <pa pb>) { my enum F1 <pa pc>; method m { pa } }; C.m.Str], X::PoisonedAlias,
    'an enum value declared in the body of a class poisons one of the same name declared in its trait argument';
throws-like q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; my class C is tagged(my constant K = 5) { }; K], X::Undeclared::Symbols,
    'a constant declared in a trait argument of a class is not visible after the class';
