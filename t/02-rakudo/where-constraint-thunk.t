use Test;
use nqp;

plan 35;

my $rakuast := nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';

is (try EVAL(q[my subset P of Int where (sub g($v) { $v > 0 })($_); my P $y = 5; g(3)])), True,
    'a sub declared in the where of a subset is called after the subset';
is (try EVAL(q[my Str $s where /^ a / = "abc"; $s])), 'abc',
    'a regex as the where of a variable is smartmatched';
is (try EVAL(q[my Str $s where /^ a / = "abc"; try $s = "x"; $s])), 'abc',
    'a regex as the where of a variable rejects what it does not match';
is (try EVAL(q[my Int $y where 0 || { $_ > 0 } = 5; $y])), 5,
    'a constant || giving a block as the where of a variable is smartmatched';
is (try EVAL(q[my class A { has Int $.x where 0 || { $_ > 0 } }; A.new(x => 3).x])), 3,
    'a constant || giving a block as the where of an attribute is smartmatched';
is (try EVAL(q[my class A { has Str $.s where /^a/ }; A.new(s => "ab").s])), 'ab',
    'a regex as the where of an attribute is smartmatched';
is (try EVAL(q[sub f($x = rand < 2) { $x }; BEGIN &f.signature.params[0].default.()])), True,
    'a thunked parameter default called at BEGIN time gives its value';
is (try EVAL(q[constant C = (state $ = 5); C])), 5,
    'an anonymous state declaration as the value of a constant is initialized';
todo 'gives the variable before initializing it on the legacy frontend' unless $rakuast;
is (try EVAL(q[BEGIN (state $ = 5)])), 5,
    'an anonymous state declaration under BEGIN is initialized';
todo 'gives the variable before initializing it on the legacy frontend' unless $rakuast;
is (try EVAL(q[my $v = CHECK (state $ = 5); $v])), 5,
    'an anonymous state declaration under CHECK is initialized';
todo 'gives the variable before initializing it on the legacy frontend', 4 unless $rakuast;
is (try EVAL(q[constant C = try (state $n = 5); C])), 5,
    'a state declaration under try as the value of a constant is initialized';
is (try EVAL(q[constant C = try 1 + (state $ = 5); C])), 6,
    'an anonymous state declaration under try as the value of a constant is initialized';
is (try EVAL(q[constant C = once (state $ = 5); C])), 5,
    'an anonymous state declaration under once as the value of a constant is initialized';
is (try EVAL(q[constant C = (gather take (state $ = 5))[0]; C])), 5,
    'an anonymous state declaration under gather as the value of a constant is initialized';
todo 'gives the variable before initializing it on the legacy frontend', 2 unless $rakuast;
is-deeply (try EVAL(q[my subset P of Int where * > (state $n = 5); BEGIN (7 ~~ P, 3 ~~ P)])), (True, False),
    'a state declaration in a WhateverCode as the where of a subset used at BEGIN time is initialized';
is-deeply (try EVAL(q[my subset P of Int where * > (state $ = 5); BEGIN (7 ~~ P, 3 ~~ P)])), (True, False),
    'an anonymous state declaration in a WhateverCode as the where of a subset used at BEGIN time is initialized';
# These hold without the thunk too, and guard what it keeps.
is (try EVAL(q[my Int $y where (sub g($v) { $v > 0 })($_) = 5; $y])), 5,
    'a sub declared in the where of a variable is called';
is-deeply (try EVAL(q[sub f($n) { my Int $y where (sub g($v) { $v > $n })($_) = 5; $y }; (f(1), (try f(9)) // 'no')])), (5, 'no'),
    'a sub declared in the where of a variable closes over each call of its routine';
is (try EVAL(q[my Int $y where (my $v will leave { } = $_) > 0 = 5; $y])), 5,
    'a declaration with a will trait in the where of a variable is checked';
is (try EVAL(q[class A { has Int $.x where (sub g($v) { $v > 0 })($_) }; A.new(x => 3).x])), 3,
    'a sub declared in the where of an attribute is called';
todo 'takes its topic as a plain parameter on the legacy frontend' unless $rakuast;
ok (try EVAL(q[sub f($x where $_ > 0) { $x }; &f.signature.params[0].constraint_list[0].signature.params[0].raw])),
    'the code of a smartmatched where takes its topic raw, as a block does';
is (try EVAL(q:to/CODE/)), True,
    my subset P of Str where $_ eq q:to/END/.chomp;
        abc
        END
    BEGIN "abc" ~~ P
    CODE
    'a heredoc in the where of a subset used at BEGIN time is smartmatched';
todo 'gives the variable before initializing it on the legacy frontend', 2 unless $rakuast;
is-deeply (try EVAL(q[my subset P of Int where $_ > (state $n = 5); BEGIN (7 ~~ P, 3 ~~ P)])), (True, False),
    'a state declaration in the where of a subset used at BEGIN time is initialized';
is-deeply (try EVAL(q[my subset P of Int where $_ > (state $ = 5); BEGIN (7 ~~ P, 3 ~~ P)])), (True, False),
    'an anonymous state declaration in the where of a subset used at BEGIN time is initialized';
if $rakuast {
    is EVAL(q[constant C = (my Int $x where $_ < (state $ = 5) = 3); C]), 3,
        'an anonymous state declaration in the where of a variable in the value of a constant is initialized';
}
else {
    skip 'dies on the legacy frontend', 1;
}
is (try EVAL(q[my subset P of Int where (my ($z, $w) := ($_, 1)) && $z > 0; BEGIN 5 ~~ P])), True,
    'a signature declaration in the where of a subset used at BEGIN time binds';
if $rakuast {
    is EVAL(q[my $v = BEGIN (my Int $x where $_ < (state $ = 5) = 3); $v]), 3,
        'an anonymous state declaration in the where of a variable under BEGIN is initialized';
}
else {
    skip 'dies on the legacy frontend', 1;
}
is (try EVAL(q[my ($a where 0 || 5) := (5,); $a])), 5,
    'a constant || as the where of a signature declaration binds';
is-deeply (try EVAL(q[my ($a where 0 || { $_ > 0 }, $b) := (5, 2); ($a, $b)])), (5, 2),
    'a constant || giving a block as the where of a signature declaration binds';
# These hold whichever block declares the code, and guard what it keeps.
is (try EVAL(q[my subset P of Str where /^a/; BEGIN "ab" ~~ P])), True,
    'a regex as the where of a subset used at BEGIN time is smartmatched';
is (try EVAL(q[my subset P of Int where (multi g(Int $v) { $v > 0 })($_); BEGIN 3 ~~ P])), True,
    'a multi declared in the where of a subset used at BEGIN time is called';
is (try EVAL(q[sub f { my subset P of Int where (multi g(Int $v) { $v > 0 })($_); BEGIN 3 ~~ P }; f()])), True,
    'a multi declared in the where of a subset in a routine used at BEGIN time is called';
is (try EVAL(q[my class A { has Int $.x where (multi g(Int $v) { $v > 0 })($_) }; BEGIN A.new(x => 3).x])), 3,
    'a multi declared in the where of an attribute used at BEGIN time is called';
sub routine-in-where($y, $x where (sub w($v) { $v eq "d$y" })($_)) { $x }
BEGIN routine-in-where(1, 'd1');
is (try routine-in-where(2, 'd2')), 'd2',
    'a sub declared in the where of a parameter closes over each call of its routine after a call at BEGIN time';
sub multi-in-where($y, $x where (multi w($v) { $v eq "m$y" })($_)) { $x }
BEGIN multi-in-where(1, 'm1');
is (try multi-in-where(2, 'm2')), 'm2',
    'a multi declared in the where of a parameter closes over each call of its routine after a call at BEGIN time';

# vim: expandtab shiftwidth=4
