#!./parrot perl6.pbc

# check array variables

use v6;

say '1..11';

my @a = (1, 2, 3);

say 'ok ' ~ @a[0];
say 'ok ' ~ @a[1];
say 'ok ' ~ @a[*-1];
say 'ok 4' if 3 == @a.elems;

my @b = <5 6>;

say 'ok ' ~ @b[0];
say 'ok ' ~ @b[*-1];
say 'ok 7' if 2 == @b.elems;

my @c = <ok 8>; say ~@c;

say 'ok 9' if 2 == (1, 2).elems;
say 'ok 10' if 3 == <a b c>.elems;
say 'ok 11' if 3 == ['a', <2 three>].elems;

