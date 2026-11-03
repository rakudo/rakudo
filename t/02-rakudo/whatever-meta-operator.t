use Test;
use nqp;
use MONKEY-SEE-NO-EVAL;

plan 43;

is-deeply (1 R[xx] *)(2).List, (2,), 'R[xx] primes a Whatever right side';
is-deeply (1 Rxx *)(2).List, (2,), 'Rxx primes a Whatever right side';
is-deeply (1 RRxx *)(3).List, (1, 1, 1), 'RRxx primes a Whatever right side';
is-deeply (1 R[Rxx] *)(3).List, (1, 1, 1), 'R[Rxx] primes a Whatever right side';
is-deeply (1 [Rxx] *)(2).List, (2,), '[Rxx] primes a Whatever right side';
is-deeply ((1, 2) Xxx *)(2).map(*.List).List, ((1, 1), (2, 2)),
    'Xxx primes a Whatever right side';
is-deeply ((1, 2) Zxx *)(2).map(*.List).List, ((1, 1),),
    'Zxx primes a Whatever right side';
is-deeply (* Zxx 2)(3).map(*.List).List, ((3, 3),),
    'Zxx primes a Whatever left side';
is-deeply ((1, 2) »xx» *)(2).map(*.List).List, ((1, 1), (2, 2)),
    '»xx» primes a Whatever right side';
{
    no worries;
    my $x = 1;
    isa-ok ($x xx= *), WhateverCode, 'xx= primes a Whatever right side';
}

is-deeply (3 R.. *)(1), 1..3, 'R.. primes a Whatever right side';
is-deeply (3 ZR.. *)(1).List, (1..3,), 'ZR.. primes a Whatever right side';
is-deeply ((1, 2) X.. *)(3).List, (1..3, 2..3), 'X.. primes a Whatever right side';
is-deeply (* X.. 3)(1).List, (1..3,), 'X.. primes a Whatever left side';
is-deeply ((1, 2) Z.. *)(3).List, (1..3,), 'Z.. primes a Whatever right side';
is-deeply ((1, 2) »..» *)(3).List, (1..3, 2..3), '»..» primes a Whatever right side';
is-deeply (* R... 1)(3).List, (1, 2, 3), 'R... primes a Whatever left side';
is-deeply (*.succ R... 1)(3).List, (1, 2, 3, 4), 'R... primes a WhateverCode left side';
isa-ok (1 X... *), WhateverCode, 'X... primes a Whatever right side';

is (Int R~~ *.succ)(5), True, 'R~~ primes a WhateverCode right side';
is-deeply (5 X~~ *.succ)(41).List, (False,), 'X~~ primes a WhateverCode right side';
is-deeply (5 Z~~ *.succ)(41).List, (False,), 'Z~~ primes a WhateverCode right side';
if nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast' {
    is-deeply (5 ![~~] *.succ)(41), True, '![~~] primes a WhateverCode right side';
}
else {
    skip 'the legacy frontend does not prime the right side of a negated chaining operator', 1;
}
is-deeply (*.succ ![~~] Int)(1), False, '![~~] primes a WhateverCode left side';

{
    my @a = 1, 2;
    (@a Z= *)(5);
    is-deeply @a, [5, 2], 'Z= primes a Whatever right side';
}
{
    my @a = 1, 2;
    (@a Z= *.succ)(4);
    is-deeply @a, [5, 2], 'Z= primes a WhateverCode right side';
}
{
    my $x;
    (* R= $x)(5);
    is $x, 5, 'R= primes a Whatever left side';
}
{
    my @a = 1, 2;
    (@a »=» *)(5);
    is-deeply @a, [5, 5], '»=» primes a Whatever right side';
}

{
    my &c = *.succ Ro *.pred;
    is &c.arity, 2, 'Ro primes a WhateverCode on each side';
}
{
    my &f = *.pred;
    isa-ok (&f o= *.succ), WhateverCode, 'o= primes a WhateverCode right side';
}
is-deeply (1 Rxx **)((2, 3)).map(*.List).List, ((2,), (3,)), 'Rxx primes a HyperWhatever right side';

is (1 R[&infix:<+>] *)(2), 3, 'R[&infix:<+>] primes a Whatever right side';
{
    sub f($a, $b) { $a - $b }
    is (1 R[&f] *)(5), 4, 'R[&f] primes a Whatever right side';
    is-deeply ((1, 2) Z[&f] *)(5).List, (-4,), 'Z[&f] primes a Whatever right side';
    is-deeply ((1, 2) X[&f] *)(5).List, (-4, -3), 'X[&f] primes a Whatever right side';
    is-deeply ((1, 2) »[&f]» *)(5).List, (-4, -3), '»[&f]» primes a Whatever right side';
}

if nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast' {
    is-deeply (1 Sxx *)(2).List, (1, 1), 'Sxx primes a Whatever right side';
    is-deeply (1 S.. *)(3), 1..3, 'S.. primes a Whatever right side';
}
else {
    skip 'the legacy frontend cannot run a sequenced operator', 2;
}

{
    my $x;
    $x //= *;
    isa-ok $x, Whatever, '//= assigns a Whatever right side';
}
{
    my $x = 0;
    $x ||= *;
    isa-ok $x, Whatever, '||= assigns a Whatever right side';
}
{
    my $x;
    $x [//]= *;
    isa-ok $x, Whatever, '[//]= assigns a Whatever right side';
}
{
    my $x;
    ($x [R//]= *)(5);
    is $x, 5, '[R//]= primes a Whatever right side';
}
if nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast' {
    is EVAL(｢my $x; ($x S//= *)(5); $x｣), 5, 'S//= primes a Whatever right side';
}
else {
    skip 'the legacy frontend cannot compile a sequenced assign', 1;
}
