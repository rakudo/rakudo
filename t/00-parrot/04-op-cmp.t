#!./parrot

# check compare ops

use v6;

say '1..24';

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

'a' lt 'b' and say 'ok 13';
'a' gt 'b' or say 'ok 14';
'a' le 'b' and say 'ok 15';
'a' ge 'b' or say 'ok 16';
'a' eq 'a' and say 'ok 17';
'a' eq 'b' or say 'ok 18';
'a' lt 'b' lt 'c' and say 'ok 19';
'a' lt 'b' lt 'b' or say 'ok 20';
4 gt 'c' gt 'a' and say 'ok 21';
4 gt 'c' gt 'c' or say 'ok 22';
'a' ne 'b' and say 'ok 23';
'a' ne 'a' or say 'ok 24';
