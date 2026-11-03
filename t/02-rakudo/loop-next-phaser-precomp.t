use lib <t/02-rakudo/test-packages>;
use Test;
use nqp;
use LoopNextPhasers;

plan 6;

my $rakuast := nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';

is-deeply LoopNextPhasers::while-next(), ((1, 2), <N N>),
    'a NEXT phaser of a while loop giving values in a precompiled module runs';
is-deeply LoopNextPhasers::while-two-nexts(), ((1, 2), <M N M N>),
    'two NEXT phasers of a while loop giving values in a precompiled module run';
todo 'runs NEXT on last as well on the legacy frontend' unless $rakuast;
is-deeply LoopNextPhasers::while-true-next(), ((1, 2), <N N>),
    'a NEXT phaser of a while loop with a true condition giving values in a precompiled module runs';
is-deeply LoopNextPhasers::loop-next(), ((1, 2), <N N>),
    'a NEXT phaser of a loop giving values in a precompiled module runs';
is-deeply LoopNextPhasers::while-undo-next(), (1, 'N', 2, 'N'),
    'a NEXT phaser of a while loop with an UNDO phaser in a precompiled module runs';
is-deeply LoopNextPhasers::while-pointy-last-next(), (1, 'N', 2, 'N', 'L'),
    'a pointy block with LAST and NEXT phasers of a while loop in a precompiled module gets the value of the condition';

# vim: expandtab shiftwidth=4
