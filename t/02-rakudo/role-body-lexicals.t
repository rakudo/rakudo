use Test;
use nqp;

plan 34;

my $rakuast := nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';

is (try EVAL(q[my $v = 7; (my role R { method m { $v } } for 1..1)[0].m])), 7,
    'a role declared as a for modifier statement sees the lexicals around it';
is (try EVAL(q[sub f { my $v = 7; (my role R { method m { $v } } for 1..1)[0].m }; f()])), 7,
    'a role declared as a for modifier statement in a routine sees its lexicals';
is (try EVAL(q[my $v = 7; my $i = 0; (my role R { method m { $v } } while $i++ < 1)[0].m])), 7,
    'a role declared as a while modifier statement sees the lexicals around it';
is (try EVAL(q[my $v = 7; my role R { method m { $v } } for 1..1; R.m])), 7,
    'a role declared as a sunk for modifier statement sees the lexicals around it';
is (try EVAL(q[my $v = 7; my $i = 0; my role R { method m { $v } } while $i++ < 1; R.m])), 7,
    'a role declared as a sunk while modifier statement sees the lexicals around it';
is (try EVAL(q[my $v = 7; my $i = 0; $i++ until [(my role R { method m { $v } }), $i > 2][1]; R.m])), 7,
    'a role declared in the condition of a sunk until modifier statement sees the lexicals around it';
is (try EVAL(q[my $v = 7; constant K = (my role R { method m { $v } }); K.m])), 7,
    'a role declared in a constant sees the lexicals around it';
is (try EVAL(q[my $v = 7; constant K = [my role R { method m { $v } }][0]; K.m])), 7,
    'a role declared in an array literal in a constant sees the lexicals around it';
is (try EVAL(q[my $v = 7; my $r = try my role R { method m { $v } }; $r.m])), 7,
    'a role declared under try sees the lexicals around it';
is (try EVAL(q[my $v = 7; ((gather take my role R { method m { $v } })[0]).m])), 7,
    'a role declared under gather sees the lexicals around it';
is (try EVAL(q[my $v = 7; (await start my role R { method m { $v } }).m])), 7,
    'a role declared under start sees the lexicals around it';
is (try EVAL(q[my $v = 7; my $x is default(my role R { method m { $v } }); $x.m])), 7,
    'a role declared in a trait argument sees the lexicals around it';
is (try EVAL(q[my $v = 7; my $r = (42 andthen (Nil andthen my role R { method m { $v } })); R.m])), 7,
    'a role declared in a thunk that does not run inside one that does sees the lexicals around it';
is-deeply (try EVAL(q[sub o($w, @l) { my role R { method m { $w } } for @l; my class C does R { }; C.new.m }; (o(8, ()), o(9, ()))])), (8, 9),
    'a role declared as a for modifier statement over nothing sees the lexicals of each call';
is-deeply (try EVAL(q[sub o($w) { my $x = g(); my role R { method m { $w } } for 1; my class C does R { }; sub g { C.new.m }; $x }; (o(8), o(9))])), (8, 9),
    'a role declared as a for modifier statement sees the lexicals of each call before the statement runs';
is (try EVAL(q[my $v = 7; my role R { my $u = $v + 1; method m { $u } } for 1..1; R.m])), 8,
    'a role declared as a for modifier statement has a body that sees the lexicals around it';
is (try EVAL(q[my $v = 7; my role R[::T] { method m { $v } } for 1..1; R[Int].new.m])), 7,
    'a parametric role declared as a for modifier statement sees the lexicals around it';
is-deeply (try EVAL(q[sub o($w) { my role R { method m { $w } } for 1..1; R }; (o(8).m, o(9).m)])), (8, 9),
    'a role declared as a for modifier statement and returned from a routine sees the lexicals of each call';
is (try EVAL(q[my $v = 7; my $r = once my role R { method m { $v } }; $r.m])), 7,
    'a role declared under once sees the lexicals around it';
is (try EVAL(q[my $v = 7; my $r = ENTER my role R { method m { $v } }; $r.m])), 7,
    'a role declared as an ENTER phaser sees the lexicals around it';
is (try EVAL(q[my $x is default(my role R { }); $x.^name])), 'R',
    'a role with an empty body declared in a trait argument gives the role';
is (try EVAL(q[my $got; multi trait_mod:<is>(Mu:U $c, :$tagged!) { $got = $tagged }; my $v = 7; my class C is tagged(my role R { method m { $v } }) { }; $got.new.m])), 7,
    'a role declared in a trait argument of a class sees the lexicals around it';
is (try EVAL(q[my multi trait_mod:<is>(Mu:U $c, :$mixr!) { $c.^add_role($mixr) }; my role R { method m { "outer" } }; my class C is mixr(my role R { method m { "inner" } }) { }; C.m ~ " " ~ R.m])), 'inner outer',
    'a role declared in a trait argument of a class does not clash with a role of the same name around it';
throws-like q[multi trait_mod:<is>(Mu:U $c, :$foo!) { }; my class C is foo(my enum E <ea eb>) { }; ea], X::Undeclared::Symbols,
    'an enum declared in a trait argument of a class is not visible after the class';
is (try EVAL(q[my $v = 7; ({ a => (role { method m { $v } }) })<a>.new.m])), 7,
    'a role declared in a hash composer sees the lexicals around it';
todo 'gives (Mu) on the legacy frontend' unless $rakuast;
is-deeply (try EVAL(q[sub f($x) { { a => (role { method m { $x } }) } }; (f(3)<a>.new.m, f(4)<a>.new.m)])), (3, 4),
    'a role declared in a hash composer in a routine sees the lexicals of each call';
todo 'gives (Mu) on the legacy frontend' unless $rakuast;
is-deeply (try EVAL(q[sub f($x) { { a => { b => (role { method m { $x } }) } } }; (f(3)<a><b>.new.m, f(4)<a><b>.new.m)])), (3, 4),
    'a role declared in a nested hash composer in a routine sees the lexicals of each call';
is-deeply (try EVAL(q[my $got; multi trait_mod:<is>(Mu:U $c, :$tagged!) { $got = $tagged }; sub o($w) { my class C is tagged(my role R { method m { $w } }) { }; $got.new.m }; (o(8), o(9))])), (8, 9),
    'a role declared in a trait argument of a class in a routine sees the lexicals of each call';
is (try EVAL(q[my $v = 7; my ($a, $b = my role R { method m { $v } }) := \(1); $b.m])), 7,
    'a role declared as a signature binding default sees the lexicals around it';
# These hold without the thunk too, and guard what it keeps.
is (try EVAL(q[my $v = 7; 42 andthen my role R { method m { $v } }; R.m])), 7,
    'a role declared right of andthen sees the lexicals around it';
is-deeply (try EVAL(q[my $v = 7; ((my role R { method m { $v } }) xx 2).map(*.m).List])), (7, 7),
    'a role declared on the left of xx sees the lexicals around it';
is-deeply (try EVAL(q[sub o($w) { Nil andthen my role R { method m { $w } }; R.m }; (o(8), o(9))])), (8, 9),
    'a role declared right of andthen with an undefined left side sees the lexicals of each call';
is-deeply (try EVAL(q[sub o($w) { 42 orelse my role R { method m { $w } }; my class C does R { }; C.new.m }; (o(8), o(9))])), (8, 9),
    'a role declared right of orelse with a defined left side sees the lexicals of each call';
is-deeply (try EVAL(q[sub o($w) { my @a = (my role R { method m { $w } }) xx 0; my class C does R { }; C.new.m }; (o(8), o(9))])), (8, 9),
    'a role declared on the left of xx 0 sees the lexicals of each call';
