#!./parrot

# check logical ops

use v6;

say '1..10';

1 and say 'ok 1';
0 or say 'ok 2';
1 && say 'ok 3';
0 || say 'ok 4';
(1 and 2) and say 'ok 5';
(2 && 4) and say 'ok 6';
(0 or 2) and say 'ok 7';
(2 || 0) and say 'ok 8';
(1 xor 0) and say 'ok 9';
(1 ^^ 1)  or say 'ok 10';
