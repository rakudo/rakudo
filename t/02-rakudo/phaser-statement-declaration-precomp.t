use lib <t/02-rakudo/test-packages>;
use Test;
use nqp;
use PhaserStatementDeclaration;

plan 5;

unless nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    skip-rest 'a phaser statement declares for its block only with the RakuAST frontend';
    exit;
}

is-deeply PhaserStatementDeclaration::first-scalar(), [5, Any, Any],
    'a scalar declared by FIRST in a precompiled module is visible in the loop body';
is-deeply PhaserStatementDeclaration::first-native(), [5, 0, 0],
    'a native declared by FIRST in a precompiled module is visible in the loop body';
is-deeply PhaserStatementDeclaration::first-sub(), [42, 42],
    'a sub declared by FIRST in a precompiled module is callable in the loop body';
is-deeply PhaserStatementDeclaration::first-state(), [6, 7, 8],
    'a state variable declared by FIRST in a precompiled module is initialized once';
is PhaserStatementDeclaration::post-closure(), 7,
    'a closure in a precompiled module sees the value POST assigns';
