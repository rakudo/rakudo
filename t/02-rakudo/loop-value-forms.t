use Test;
use nqp;

plan 3;

my $rakuast := nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';

is-deeply (try EVAL(q[my $i = 0; (do loop (; ; $i++) { last if $i > 2; $i }).List])), (0, 1, 2),
    'a loop with an increment and no condition giving a value runs';
is-deeply (try EVAL(q[my $s = do loop (my $i = 0; ; $i++) { last if $i > 2; $i }; $s.List])), (0, 1, 2),
    'the setup of a loop with an increment and no condition giving a value runs';
if $rakuast {
    is (try EVAL(q[my $n = 0; loop (my $i = 5; ; $i++) { UNDO { }; $n++; last if $i > 6 }; $n])), 3,
        'the setup of a loop with an increment, no condition and an UNDO phaser runs';
}
else {
    skip 'dies on the legacy frontend', 1;
}
