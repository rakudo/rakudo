use v6;

# check basic regex capabilities
say '1..11';

'abc' ~~ /abc/ and say 'ok 1';
'2' ~~ /^ \d+ $/ and say "ok ", ~$/;

my $rx = / <alpha> /;
'012a456' ~~ $rx and say 'ok 3';

my $l = 5;
my $r = 5;
$l   ~~ $r and say 'ok 4';
5    ~~ $r and say 'ok 5';
'5'  ~~ $r and say 'ok 6';
'25' ~~ $r or  say 'ok 7';

$r = / 5 /;
$l   ~~ $r and say 'ok 8';
5    ~~ $r and say 'ok 9';
'5'  ~~ $r and say 'ok 10';
'25' ~~ $r and say 'ok 11';



# vim: expandtab shiftwidth=4
