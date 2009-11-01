#!./parrot perl6.pbc

# check array variables

use v6;

say '1..11';

my @a = (1, 2, 3);

say 'ok ' ~ @a[0];
say 'ok ' ~ @a[1];
say 'ok ' ~ @a[2];
3 == @a.elems and say 'ok 4';

my @b = <5 6>;

say 'ok ' ~ @b[0];
say 'ok ' ~ @b[1];
2 == @b.elems and say 'ok 7';

my @c = <ok 8>; say ~@c;

2 == (1, 2).elems and say 'ok 9';
3 == <a b c>.elems and say 'ok 10';
3 == ['a', <2 three>].elems and say 'ok 11';

