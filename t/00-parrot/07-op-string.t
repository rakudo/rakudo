#!./parrot perl6.pbc

# check string ops

use v6;

say '1..18';
say 'ok ' ~ '1';
say 'ok' ~ ' ' ~ '2';
my $s = 'ok ';
say $s ~ '3';
$s ~= '4';
say $s;

#5
$s = 'ab';
$s eq 'ab' and say 'ok 5';
$s ne 'Ab' and say 'ok 6';
'ab' le 'ab' and say 'ok 7';
'ab' lt 'ac' and say 'ok 8';
'cd' ge 'cd' and say 'ok 9';
'cd' gt 'cc' and say 'ok 10';

'ab' x 2 eq 'abab' and say 'ok 11';
$s x= 3;
$s eq 'ababab' and say 'ok 12';

$s = 'A' ~| 3;
$s eq 's' and say 'ok 13';

$s = 'a';
$s ~&= ' ';
$s eq ' ' and say 'ok 14';
$s = 'abc';
$s ~&= '    ';
$s eq '   ' and say 'ok 15';

$s = 'ABC' ~| '   ';
$s eq 'abc' and say 'ok 16';

# check COW of Parrot strings (r27046, r27048)
my $foo = 'fred';
my $bar = 'fred';
$foo++;
$bar--;
$foo eq 'free' and say 'ok 17';
$bar eq 'frec' and say 'ok 18';
