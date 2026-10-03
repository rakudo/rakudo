use Test;
use nqp;

plan 25;

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
