#!./parrot perl6.pbc

# check logical ops

use v6;

say '1..16';

1 and say 'ok 1';
0 or say 'ok 2';
1 && say 'ok 3';
0 || say 'ok 4';
0 xor say 'ok 5';
0 ^^ say 'ok 6';

## chaining logical ops
(1 and 2) and say 'ok 7';
(2 && 4) and say 'ok 8';
(0 or 2) and say 'ok 9';
(2 || 0) and say 'ok 10';
(1 xor 0) and say 'ok 11';
(1 ^^ 1)  or say 'ok 12';

## interesting
(1 and 0 xor 1 or 0) and say 'ok 13';
(1 and 0 xor 0 || 1) and say 'ok 14';

## more interesting
(5 and 0 xor 0 || 3) eq 3 and say 'ok 15';
(0 xor 0 ^^ 42 or 1) eq 1 or say 'ok 16';
