#!./parrot

# check array variables

use v6;

say '1..11';

my @a = (1, 2, 3);

say 'ok ' ~ @a[0];
say 'ok ' ~ @a[1];
say 'ok ' ~ @a[-1];
say 'ok 4' if 3 eq @a.elems;

my @b = <5 6>;

say 'ok ' ~ @b[0];
say 'ok ' ~ @b[-1];
say 'ok 7' if 2 eq @b.elems;

my @c = <ok 8>; say ~@c;

say 'ok 9' if 2 eq (1, 2).elems;
say 'ok 10' if 3 eq <a b c>.elems;
say 'ok 11' if 3 eq ['a', <2 three>].elems;

