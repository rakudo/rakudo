use Test;

plan 12;

# A WhateverCode is a thunk around its expression. Something that wraps
# the expression in a thunk of its own, like a constant's value, a
# parameter default or a statement modifier, stacks that thunk on top. Code
# inside the expression must then be declared in the WhateverCode only, or
# it cannot be called. Each case is compiled as its own unit.

is-deeply EVAL('my constant $m = *.map({ $_ * 2 }); $m((1, 2)).List'), (2, 4),
    'a block in a constant WhateverCode';
is-deeply EVAL('my constant $m = *.map(-> $x { $x * 2 }); $m((1, 2)).List'), (2, 4),
    'a pointy block in a constant WhateverCode';
is-deeply EVAL('my constant $m = *.grep(/b/); $m(<a b>).List'), ("b",),
    'a regex in a constant WhateverCode';
is-deeply EVAL('my constant $m = *.map(:(Int $x) ~~ *); $m((1,)).List'), (False,),
    'a signature literal in a constant WhateverCode';
is-deeply EVAL('my constant $m = *.map(*.succ); $m((1, 2)).List'), (2, 3),
    'a WhateverCode in a constant WhateverCode';
is-deeply EVAL('constant $m = *.map({ $_ * 2 }); $m((1, 2)).List'), (2, 4),
    'a block in a WhateverCode of a constant with no scope declarator';
is-deeply EVAL('my constant $m = *.map({ $_ * 2 }); BEGIN $m((1, 2)).List'), (2, 4),
    'a block in a constant WhateverCode called at BEGIN time';
is-deeply EVAL('sub f($m = *.map({ $_ * 2 })) { $m((1, 2)).List }; f()'), (2, 4),
    'a block in a WhateverCode parameter default';
is-deeply EVAL('sub f($k, $m = *.map({ $_ * $k })) { $m((1, 2)).List }; f(3)'), (3, 6),
    'a block closing over an earlier parameter in a WhateverCode parameter default';
is-deeply EVAL('my $i = 0; (*.map({ $_ * 2 }) while $i++ < 1)[0]((1, 2)).List'), (2, 4),
    'a block in a WhateverCode with a while modifier';
is-deeply EVAL('my $i = 0; (*.map({ $_ * 2 }) until $i++ >= 1)[0]((1, 2)).List'), (2, 4),
    'a block in a WhateverCode with an until modifier';

# A condition modifier thunk forms no block, so the loop thunk around it
# declares the code of the expression.
is-deeply EVAL('(.map({ $_ * 2 }).List if 1 for (1, 2),)'), ((2, 4),),
    'a block in an expression with if and for modifiers';

# vim: expandtab shiftwidth=4
