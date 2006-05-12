#!./parrot

# check basic regex capabilities

use v6;

say '1..27';

'abc' ~~ /abc/ and say 'ok 1';
'2' ~~ /^ \d+ $/ and say "ok ", $/;

my $rx = / <alpha> /;
'012a456' ~~ $rx and say 'ok 3';

my $l = 5;
my $r = 5;
$l   ~~ $r and say 'ok 4';
5    ~~ $r and say 'ok 5';
'5'  ~~ $r and say 'ok 6';
'25' ~~ $r and say 'ok 7';

$r = / 5 /;
$l   ~~ $r and say 'ok 8';
5    ~~ $r and say 'ok 9';
'5'  ~~ $r and say 'ok 10';
'25' ~~ $r and say 'ok 11';

$r = '5';
$l   ~~ $r and say 'ok 12';
5    ~~ $r and say 'ok 13';
'5'  ~~ $r and say 'ok 14';
'25' ~~ $r or  say 'ok 15';

$r = 5;
$l   ~~ $r and say 'ok 16';
5    ~~ $r and say 'ok 17';
'5'  ~~ $r and say 'ok 18';
'25' ~~ $r and say 'ok 19';

$r = / 5 /;
$l   ~~ $r and say 'ok 20';
5    ~~ $r and say 'ok 21';
'5'  ~~ $r and say 'ok 22';
'25' ~~ $r and say 'ok 23';

$r = '5';
$l   ~~ $r and say 'ok 24';
5    ~~ $r and say 'ok 25';
'5'  ~~ $r and say 'ok 26';
'25' ~~ $r or  say 'ok 27';


