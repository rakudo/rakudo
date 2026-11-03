use Test;

plan 16;

is ((42 andthen *.succ) + *)(1), 44,
    'a WhateverCode called by andthen keeps its parameter under an outer +';
is ((42 andthen * + 1) + *)(1), 44,
    'a WhateverCode built from an operator and called by andthen keeps its parameter under an outer +';
is ((Any orelse *.raku) ~ *)(1), 'Any1',
    'a WhateverCode called by orelse keeps its parameter under an outer ~';
{
    my &f = (42 andthen *.succ) ~ *;
    is &f.arity, 1, 'the outer WhateverCode over an andthen takes one parameter';
}
{
    my &f = (5 ~~ *.succ) + *;
    is &f.arity, 1, 'a WhateverCode right of ~~ keeps its parameter under an outer +';
}
is ((5 ~~ *.succ) + *)(1), 2,
    'the outer + receives the result of a ~~ against a WhateverCode';
{
    my &f = (1 || *.succ) ~ *;
    is &f.arity, 1, 'a WhateverCode right of || keeps its parameter under an outer ~';
}
is ((1 || *.succ) ~ *)(2), '12',
    'the outer ~ receives the result of a || before a WhateverCode';
{
    my &f = (1 ?? *.succ !! 2) ~ *;
    is &f.arity, 1, 'a WhateverCode in a ternary branch keeps its parameter under an outer ~';
}
{
    my &f = ((* + 1)) + *;
    is &f.arity, 1, 'a WhateverCode in doubled parentheses keeps its parameter under an outer +';
}
is ((* + 1) * 2 + *)(1, 2), 6,
    'a WhateverCode each enclosing operator absorbs joins the outer one';
{
    my &f = (* + 1; * + 2) + *;
    is &f.arity, 1, 'WhateverCodes in a list of statements keep their parameters under an outer +';
}

# A statement with a modifier in parentheses is not an operand the outer
# prime can absorb.
{
    my &f = (* + 1 if 1) + *;
    is &f.arity, 1, 'a WhateverCode under an if modifier keeps its parameter under an outer +';
}
{
    my &f = (* + 1 unless 0) + *;
    is &f.arity, 1, 'a WhateverCode under an unless modifier keeps its parameter under an outer +';
}
{
    my &f = (* + 1 with 1) + *;
    is &f.arity, 1, 'a WhateverCode under a with modifier keeps its parameter under an outer +';
}
{
    my &f = (* + 1 for 1) + *;
    is &f.arity, 1, 'a WhateverCode under a for modifier keeps its parameter under an outer +';
}
