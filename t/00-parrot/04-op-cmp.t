#!./parrot

# check compare ops

use v6;

say '1..12';

1 < 2 and say 'ok 1';
1 > 2 or say 'ok 2';
1 <= 2 and say 'ok 3';
1 >= 2 or say 'ok 4';
1 == 1 and say 'ok 5';
1 == 2 or say 'ok 6';
1 < 2 < 3 and say 'ok 7';
1 < 2 < 2 or say 'ok 8';
4 > 3 > 1 and say 'ok 9';
4 > 3 > 3 or say 'ok 10';
1 != 2 and say 'ok 11';
1 != 1 or say 'ok 12';
