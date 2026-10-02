use Test;

plan 6;

# A role body is compiled when the role is composed, which can be before a
# routine declared later in the file is resolved. A .&name call to such a
# routine looks it up by name at run time.

is EVAL(q:to/CODE/), 2, 'a .&name call to a routine declared after the role';
    my role R { method m { 1.&later } }
    sub later($x) { $x + 1 }
    R.new.m
    CODE

is-deeply EVAL(q:to/CODE/), (2, 3), 'a hyper >>.&name call to a routine declared after the role';
    my role R { method m { (1, 2)>>.&later } }
    sub later($x) { $x + 1 }
    R.new.m.List
    CODE

is EVAL(q:to/CODE/), 6, 'a .&name call with arguments to a routine declared after the role';
    my role R { method m { 1.&later(5) } }
    sub later($x, $y) { $x + $y }
    R.new.m
    CODE

is EVAL(q:to/CODE/), 2, 'a .&name call in a role composed before the routine is declared';
    my role R { method m { 1.&later } }
    my class C does R { }
    sub later($x) { $x + 1 }
    C.m
    CODE

is-deeply EVAL(q:to/CODE/), (2, 3), 'a hyper >>.&name call in a role composed before the routine is declared';
    my role R { method m { (1, 2)>>.&later } }
    my class C does R { }
    sub later($x) { $x + 1 }
    C.m.List
    CODE

throws-like 'my role R { method m { 1.&no-such-routine } }; my class C does R { }',
    X::Undeclared::Symbols,
    'an undeclared .&name callee in a role is reported as undeclared';

# vim: expandtab shiftwidth=4
