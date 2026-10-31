use Test;
use nqp;

plan 12;

is Q|my $x = 0; my @a = do until $x > 2 { $x++ }|.AST.DEPARSE,
    "my \$x = 0;\nmy \@a = do until \$x > 2 \{\n    \$x++\n}\n",
    'an until loop in value context deparses its condition as written';

is Q|my $x = 0; my @a = do repeat { $x++ } until $x > 2|.AST.DEPARSE,
    "my \$x = 0;\nmy \@a = do repeat \{\n    \$x++\n} until \$x > 2\n",
    'a repeat until loop in value context deparses its condition as written';

is Q|my $x = 0; my @a = ($x++ until $x > 2)|.AST.DEPARSE,
    "my \$x = 0;\nmy \@a = (\$x++ until \$x > 2)\n",
    'an until modifier in value context deparses its condition as written';

is Q|my $x = 0; until $x > 2 { UNDO { }; $x++ }|.AST.DEPARSE,
    "my \$x = 0;\nuntil \$x > 2 \{\n    UNDO \{ }\n    \$x++\n}\n",
    'a sunk until loop with an UNDO phaser deparses its condition as written';

is-deeply EVAL(q[my $x = 0; (do until $x > 2 { $x++ }).List]), (0, 1, 2),
    'an until loop in value context runs until its condition holds';

is-deeply EVAL(q[my $x = 0; (do repeat { $x++ } until $x > 2).List]), (0, 1, 2),
    'a repeat until loop in value context runs until its condition holds';

is-deeply EVAL(q[my $x = 0; ($x++ until $x > 2).List]), (0, 1, 2),
    'an until modifier in value context runs until its condition holds';

is EVAL(q[my $x = 0; until $x > 2 { UNDO { }; $x++ }; $x]), 3,
    'a sunk until loop with an UNDO phaser runs until its condition holds';

is EVAL(q[my $n = 0; until * > 2 { last if ++$n > 5 }; $n]), 0,
    'an until loop with a WhateverCode condition takes it as true';

is EVAL(q[my $n = 0; (do until * > 2 { last if ++$n > 5; 1 }).elems]), 0,
    'an until loop in value context with a WhateverCode condition takes it as true';

is EVAL(q[my $n = 0; $n++ until * > 2; $n]), 0,
    'an until modifier with a WhateverCode condition takes it as true';

if nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast' {
    is EVAL(q[my $x = 0; my @n; my @a = do until $x > 2 { NEXT @n.push($x); $x++ }; "@a[] | @n[]"]),
        '0 1 2 | 1 2 3',
        'an until loop in value context with a NEXT phaser runs until its condition holds';
}
else {
    skip 'a NEXT phaser in an until loop in value context dies on the legacy frontend';
}

# vim: expandtab shiftwidth=4
