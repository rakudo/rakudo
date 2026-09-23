use lib <t/02-rakudo/test-packages>;
use Test;
use nqp;
use RoleMethodDroppedCode;

plan 9;

# A role body runs at BEGIN time, so its methods are compiled before the
# optimize walk of the unit, and the thunks and blocks in them get code
# objects then. The precompiled module must still have a frame for each
# of them, so the role lives in a module under test-packages.

class C does RoleMethodDroppedCode { }

is C.dead-if, 'if',
    'a thunk in the block of a constant false if';
is C.dead-nested, 'nested',
    'a thunk in a pointy block in the block of a constant false if';
is C.dead-else, 42,
    'a thunk in the else block of a constant true if';
is C.dead-unless, 'unless',
    'a thunk in the block of a constant true unless';
is C.dead-modifier, 'modifier',
    'a thunk under a constant false if statement modifier';
is C.dead-and, 'and',
    'a thunk on the right of a constant false and';
is C.dead-sub, 'sub',
    'a named sub in the block of a constant false if';
is C.dead-begin, 'dead-begin',
    'a BEGIN block in the block of a constant false if';
# The try keeps the legacy death from ending the file. Remove it along
# with the todo.
todo 'legacy loses $_ in a closure a BEGIN block made in a role method'
    unless nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';
is (try C.begin-closure), 'begin',
    'a closure that a BEGIN block in a constant false if stored still runs';
