use Test;
use nqp;

plan 54;

my $rakuast := nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';

is (try EVAL(q[my $x is default(my class D { method x { 3 } }); D.x])), 3,
    'a class declared in a trait argument of a variable has its methods';
is (try EVAL(q[my $x is default((my class D { method x { 3 } }).new); $x.x])), 3,
    'an instance of a class declared in a trait argument of a variable has its methods';
is (try EVAL(q[my class C { has $.a is default(my class D { method x { 3 } }) }; C.new.a.x])), 3,
    'a class declared in a trait argument of an attribute has its methods';
is (try EVAL(q[my multi trait_mod:<is>(Routine $r, :$tagged!) { }; sub f is tagged(my class D { method x { 3 } }) { D.x }; f()])), 3,
    'a class declared in a trait argument of a routine has its methods';
is (try EVAL(q[my multi trait_mod:<is>(Parameter $p, :$tagged!) { }; sub f($a is tagged(my class D { method x { 3 } })) { D.x }; f(1)])), 3,
    'a class declared in a trait argument of a parameter has its methods';
is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; my class C is tagged(my class D { method x { 3 } }) { method m { D.x } }; C.m])), 3,
    'a class declared in a trait argument of a class has its methods';
is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$tagged!) { }; my class C { also is tagged(my class D { method x { 3 } }); method m { D.x } }; C.m])), 3,
    'a class declared in the trait argument of an also statement has its methods';
is (try EVAL(q[my Positional[my class D { method x { 3 } }] $p; D.x])), 3,
    'a class declared in a parameterized type of a variable has its methods';
is (try EVAL(q[my $x of Positional[my class D { method x { 3 } }]; D.x])), 3,
    'a class declared in a parameterized type in an of trait of a variable has its methods';
is (try EVAL(q[sub f(--> Positional[my class D { method x { 3 } }]) { my D @a }; f().of.x])), 3,
    'a class declared in a parameterized return type has its methods';
is (try EVAL(q[Positional[my class D { method x { 3 } }].^name ~ D.x])), 'Positional[D]3',
    'a class declared in a parameterized type term has its methods';
is (try EVAL(q[my role R1[$t] { method m { $t.x } }; my class C does R1[my class D { method x { 3 } }] { }; C.m])), 3,
    'a class declared in an argument of a role a class does has its methods';
is (try EVAL(q[my role R2[$t] { method m { $t.x } }; my class C does R2[(my class D { method x { 3 } }).new] { }; C.m])), 3,
    'an instance of a class declared in an argument of a role a class does has its methods';
is (try EVAL(q[my enum E (a => (my class D { method x { 3 } }).x); a.value ~ D.x])), '33',
    'a class declared in the value of an enum has its methods';
is (try EVAL(q[constant DEBUG = False; if DEBUG { class DeadIf { method x { 3 } } }; DeadIf.x])), 3,
    'a class declared in a branch a constant condition never takes has its methods';
is (try EVAL(q[constant DEBUG = False; unless !DEBUG { class DeadUnless { method x { 3 } } }; DeadUnless.x])), 3,
    'a class declared in an unless branch a constant condition never takes has its methods';
is (try EVAL(q[constant DEBUG = True; if DEBUG { 1 } else { class DeadElse { has $.a = 4 } }; DeadElse.new.a])), 4,
    'a class declared in an else branch a constant condition never takes has its default';
is (try EVAL(q[constant DEBUG = False; if DEBUG { class DeadEmpty { } }; DeadEmpty.^name])), 'DeadEmpty',
    'an empty class declared in a branch a constant condition never takes is a type';
is (try EVAL(q[constant DEBUG = False; my class Q { }; if DEBUG { class DeadAfterClass { method z { 5 } } }; DeadAfterClass.z])), 5,
    'a class declared after another class in a branch a constant condition never takes has its methods';
is (try EVAL(q[constant DEBUG = False; if DEBUG { module DeadModule { our sub s { 3 } } }; DeadModule::s()])), 3,
    'a sub of a module declared in a branch a constant condition never takes can be called';
is (try EVAL(q[my role R4 { my $x is default(my class D { method x { 3 } }); method m { D.x } }; (class :: does R4 { }).m])), 3,
    'a class declared in a trait argument of a variable in a role body has its methods';
is (try EVAL(q[my subset S of Positional[my class D { method x { 3 } }]; D.x])), 3,
    'a class declared in a parameterized type of a subset has its methods';
is (try EVAL(q[sub f(Positional[my class D { method x { 3 } }] $p?) { D.x }; f()])), 3,
    'a class declared in a parameterized type of a parameter has its methods';
is (try EVAL(q[my multi trait_mod:<is>(Method $m, :$tagged!) { }; my class C { method m is tagged(my class D { method x { 3 } }) { D.x } }; C.m])), 3,
    'a class declared in a trait argument of a method has its methods';
is (try EVAL(q[constant DEBUG = False; if DEBUG { class DeadDefault { has $.a = 1 } }; DeadDefault.new.a])), 1,
    'a class with an attribute default declared in a branch a constant condition never takes has its default';
is (try EVAL(q[constant DEBUG = False; if DEBUG { class DeadAccessor { has $.a } }; DeadAccessor.new(a => 4).a])), 4,
    'a class with an attribute declared in a branch a constant condition never takes has its accessor';
is (try EVAL(q[Q[my $x is default(my class D { method x { 3 } }); D.x].AST.EVAL])), 3,
    'a class declared in a trait argument of a variable compiled from an AST has its methods';

# These guard where and how often the body of a package runs, and what it sees.
is-deeply (try EVAL(q[my @l; @l.push(1); my class C { @l.push(2) }; @l.push(3); @l])), [1, 2, 3],
    'the body of a class runs where the class is declared';
is-deeply (try EVAL(q[my @l; for ^2 -> $i { my class C { @l.push($i) } }; @l])), [0, 1],
    'the body of a class declared in a loop runs on each iteration';
is (try EVAL(q[sub f($n) { my class C { method m { $n } }; C.m }; f(1) ~ f(2)])), '12',
    'a class declared in a routine sees the lexicals of each call';
is-deeply (try EVAL(q[((my class C { method m { 7 } }) xx 2).map(*.m).List])), (7, 7),
    'a class declared on the left of xx has its methods';
is-deeply (try EVAL(q[(my class C { method m { 7 } } xx 2).map(*.m).List])), (7, 7),
    'a class declared without parentheses on the left of xx has its methods';
is (try EVAL(q[use nqp; nqp::handle(die("x"), 'CATCH', (my class C { method m { 7 } }).m)])), 7,
    'a class declared in a handler of nqp::handle has its methods';
is (try EVAL(q[sub f { my class C { return 5 }; 6 }; f()])), 5,
    'a return in the body of a class returns from the routine around it';
is-deeply (try EVAL(q[my @l; for 1..3 { my class C { last if $_ == 2 }; @l.push($_) }; @l])), [1],
    'a last in the body of a class leaves the loop around it';
is (try EVAL(q[my $v = 7; (try my class C { method m { $v } }).m])), 7,
    'a class declared under try sees the lexicals around it';
is (try EVAL(q[my $v = 7; (gather take my class C { method m { $v } })[0].m])), 7,
    'a class declared under gather sees the lexicals around it';
is (try EVAL(q[constant X = my class C { method m { 7 } }; X.m])), 7,
    'a class declared as the value of a constant has its methods';
is (try EVAL(q[BEGIN my class C { method m { 7 } }; C.m])), 7,
    'a class declared under BEGIN has its methods';
is-deeply (try EVAL(q[my @l; sub f($n) { f($n - 1) if $n > 0; my class C { @l.push($n) } }; f(2); @l])), [0, 1, 2],
    'the body of a class declared in a recursive routine sees the lexicals of each call';
is-deeply (try EVAL(q[my @r; sub f($n) { my $v = $n * 10; f($n - 1) if $n > 0; @r.push: (class { method m { $v } }).m }; f(2); @r])), [0, 10, 20],
    'a class declared in a recursive routine has methods that see the lexicals of each call';
is-deeply (try EVAL(q[my @l; sub g($n) { gather { take 0; my class C { @l.push($n) }; take 1 } }; my $a = g(1).iterator; my $b = g(2).iterator; $a.pull-one; $b.pull-one; $a.pull-one; $b.pull-one; @l])), [1, 2],
    'the body of a class declared in interleaved gathers sees the lexicals of each';
is-deeply (try EVAL(q[my @l; for ^3 { my class C { state $n = 0; @l.push(++$n) } }; @l])), [1, 1, 1],
    'a state variable in the body of a class declared in a loop is fresh on each iteration';
is (try EVAL(q[my $c = 0; for ^2 { my class C { once $c++ } }; $c])), 2,
    'a once in the body of a class declared in a loop runs on each iteration';
is-deeply (try EVAL(q[my role R3[$x] { my class C { has @.v = 1, 2 }; method m { C.new.v } }; (class :: does R3[1] { }).m])), [1, 2],
    'a class with an attribute default declared in a role body has its default';
is-deeply (try EVAL(q[my $t = BEGIN (class { has @.x = 1, 2 }).new; $t.x])), [1, 2],
    'a class with an attribute default declared under BEGIN has its default';
is-deeply (try EVAL(q[sub make { my class C { has @.x = 1, 2 }; C.new }; constant T = make(); T.x])), [1, 2],
    'a class with an attribute default declared in a routine called at BEGIN time has its default';
is-deeply (try EVAL(q[my @r; (class { method m { (1, 2, 3) } }).m ==> map { $_ * 2 } ==> @r; @r])), [2, 4, 6],
    'a class declared in the first stage of a feed has its methods';
is-deeply (try EVAL(q[my @r; (1, 2, 3) ==> map((class { method m { * * 2 } }).m) ==> @r; @r])), [2, 4, 6],
    'a class declared in a middle stage of a feed has its methods';
is-deeply (try EVAL(q[my @r <== map { $_ * 2 } <== (my class C { method m { (1, 2, 3) } }).m; @r])), [2, 4, 6],
    'a class declared in the first stage of a leftward feed has its methods';
if $rakuast {
    is (try EVAL(q[my $got; "ab" ~~ / :my class C { method m { 7 } }; a { $got = C.m } /; $got])), 7,
        'a class declared in a statement of a regex has its methods';
    is (try EVAL(q[~("ab" ~~ / a $((class { method m { "b" } }).m) /)])), 'ab',
        'a class declared in an interpolation of a regex has its methods';
    is (try EVAL(q[~("ab" ~~ / a ( $((class { method m { "b" } }).m) ) /)])), 'ab',
        'a class declared in an interpolation in a group of a regex has its methods';
    is (try EVAL(q[my $got; "a" ~~ / ( :my class C { method m { 7 } }; a { $got = C.m } ) /; $got])), 7,
        'a class declared in a statement in a group of a regex has its methods';
}
else {
    skip 'dies on the legacy frontend', 4;
}
