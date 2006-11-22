#!./parrot

# check array variables

use v6;

say '1..15';

my @a = (1, 2, 3);

say 'ok ' ~ @a[0];
say 'ok ' ~ @a[1];
say 'ok ' ~ @a[-1];
say 'ok 4' if 3 eq @a.elems;

my @b = <5 6>;

say 'ok ' ~ @b[0];
say 'ok ' ~ @b[-1];
say 'ok 7' if 2 eq @b.elems;

my @c = <ok 8>; say @c;

my $d; $d = 9, <10 11>;

say 'ok ' ~ @$d[0];
say 'ok ' ~ @$d[1];
say 'ok ' ~ @$d[-1];
say 'ok 12' if 3 eq @$d.elems;

say 'ok 13' if 3 eq (1, 2).elems;
say 'ok 14' if 3 eq <a b c>.elems;
say 'ok 15' if 3 eq ['a', <2 three>].elems;
