use Test;
use nqp;

plan 7;

my $rakuast := nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';

{
    my $s = do loop (my $i = 5; True;) { last if $i++ > 7; $i };
    is-deeply $s.head(10).List, (6, 7, 8),
        'the setup of a loop with a constant true condition giving a value runs';
}
{
    my $s = do loop (my $i = 5;;) { last if $i++ > 7; $i };
    is-deeply $s.List, (6, 7, 8),
        'the setup of a loop with no condition giving a value runs';
}
is-deeply (try EVAL(q[my $i = 0; (do loop (; ; $i++) { last if $i > 2; $i }).List])), (0, 1, 2),
    'a loop with an increment and no condition giving a value runs';
is-deeply (try EVAL(q[my $s = do loop (my $i = 0; ; $i++) { last if $i > 2; $i }; $s.List])), (0, 1, 2),
    'the setup of a loop with an increment and no condition giving a value runs';
{
    my $i = 0;
    while True { UNDO { }; last if ++$i > 2; 1 }
    is $i, 3, 'a while loop with a constant true condition and an UNDO phaser runs its body';
}
{
    my $i = 0;
    loop { UNDO { }; last if ++$i > 2 }
    is $i, 3, 'a loop with no condition and an UNDO phaser runs its body';
}
if $rakuast {
    is (try EVAL(q[my $n = 0; loop (my $i = 5; ; $i++) { UNDO { }; $n++; last if $i > 6 }; $n])), 3,
        'the setup of a loop with an increment, no condition and an UNDO phaser runs';
}
else {
    skip 'dies on the legacy frontend', 1;
}
