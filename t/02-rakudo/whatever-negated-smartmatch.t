use Test;
use nqp;

plan 15;

is-deeply (5 !~~ *.succ), False, 'a WhateverCode right of !~~ is the matcher';
is (* !~~ Int)(5), False, 'a Whatever left of !~~ primes';
is-deeply (1..6).grep(* % 2 !~~ 0).List, (1, 3, 5), 'a WhateverCode left of !~~ primes';
is (*.succ !~~ Int)(5), False, 'a WhateverCode left of !~~ primes against a type';
{
    my subset Odd of Int where * % 2 !~~ 0;
    nok 4 ~~ Odd, 'a subset constraint with a WhateverCode left of !~~ primes';
}
{
    my &f = (5 !~~ *.succ) + *;
    is &f.arity, 1, 'a WhateverCode right of !~~ keeps its parameter under an outer +';
}
todo 'the legacy frontend primes a WhateverCode right of !~~ beside a Whatever', 1
    unless nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';
{
    my &f = * !~~ *.succ;
    is &f.arity, 1, 'only a Whatever left of !~~ becomes a parameter, as with ~~';
}
is-deeply (1..6).grep(* % 2 [!~~] 0).List, (1, 3, 5), 'a WhateverCode left of [!~~] primes';
is (5 !~~ *)(41), True, 'a Whatever right of !~~ primes';
todo 'the legacy frontend primes a WhateverCode matcher of a bracketed !~~', 1
    unless nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';
is-deeply (5 [!~~] *.succ), False, 'a WhateverCode right of [!~~] is the matcher';
is-deeply (5 Z!~~ *.succ)(41).List, (True,), 'Z!~~ primes a WhateverCode right side';
is-deeply (5 X!~~ *.succ)(41).List, (True,), 'X!~~ primes a WhateverCode right side';
is (5 »!~~» *.succ)(41), True, '»!~~» primes a WhateverCode right side';
is (*.succ R!~~ 5)(41), True, 'R!~~ primes a WhateverCode left side';
if nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast' {
    is (5 S!~~ *.succ)(41), True, 'S!~~ primes a WhateverCode right side';
}
else {
    skip 'the legacy frontend cannot run a sequenced operator', 1;
}
