use Test;
use nqp;

plan 20;

if nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast' {
    {
        my &f = "a" xx * - 1;
        is &f.arity, 1, 'xx primes a WhateverCode right side built from an infix';
        is-deeply f(3).List, ("a", "a"), 'xx repeats by the result of a primed infix right side';
    }
    is-deeply ("a" xx * - 1 + 2)(1).List, ("a", "a"),
        'xx primes a WhateverCode right side built from nested infixes';
    {
        my &f = "a" xx * - *;
        is &f.arity, 2, 'xx primes a WhateverCode right side with two Whatevers';
        is-deeply f(5, 2).List, ("a", "a", "a"), 'xx passes both arguments in order to a primed right side';
    }
    is-deeply (* - 1 xx 2)(5).List, (4, 4), 'xx primes a WhateverCode left side built from an infix';
    {
        my &f = * - 1 xx * + 1;
        is &f.arity, 2, 'xx primes a WhateverCode built from an infix on each side';
        is-deeply f(5, 1).List, (4, 4),
            'xx passes its first argument to the left side and its second to the right side';
    }
    is-deeply (*.succ xx * - 1)(5, 3).List, (6, 6),
        'xx primes a method call WhateverCode left side and an infix WhateverCode right side';
    is-deeply (* - 1 xx 2 xx 2)(5).map(*.List).List, ((4, 4), (4, 4)),
        'xx primes a WhateverCode left side that is itself a primed xx';
    {
        my &f = 1 Rxx * xx 2;
        is &f.arity, 1, 'xx primes a WhateverCode left side built from a meta-operator';
        is-deeply f(3).map(*.List).List, ((3,), (3,)),
            'xx repeats the result of a primed meta-operator left side';
    }
    {
        my &f = (* > 1) xx * - 1;
        is &f.arity, 1, 'xx primes an infix right side and leaves a parenthesized WhateverCode left side alone';
        is-deeply f(3).map({ $_(2) }).List, (True, True),
            'xx repeats a parenthesized WhateverCode left side by a primed infix right side';
    }
    {
        my &f = (* > 1) xx *.succ;
        is &f.arity, 1, 'xx primes only its right side when the left side is a parenthesized WhateverCode';
        is-deeply f(1).map({ $_(2) }).List, (True, True),
            'xx repeats a parenthesized WhateverCode left side while priming its right side';
    }
    is-deeply ((* > 1) [xx] *.succ)(1).map({ $_(2) }).List, (True, True),
        '[xx] repeats a parenthesized WhateverCode left side while priming its right side';
    {
        my @a = 1, 2, 3;
        is-deeply @a[0 xx * - 1].List, (1, 1), 'a subscript calls a primed xx with the number of elements';
    }
}
else {
    skip 'the legacy frontend does not prime xx with a WhateverCode', 18;
}

is-deeply ("a" [xx] * - 1)(3).List, ("a", "a"), '[xx] primes a WhateverCode right side built from an infix';
is-deeply ((* > 1) xx 3).map({ $_(2) }).List, (True, True, True),
    'xx repeats a parenthesized WhateverCode left side';
