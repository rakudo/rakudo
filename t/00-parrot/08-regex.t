#!./parrot

# check basic regex capabilities

use v6;

say '1..3';

'abc' ~~ /abc/ and say 'ok 1';
'2' ~~ /^ \d+ $/ and say "ok ", $/;

my $rx = / <alpha> /;
'012a456' ~~ $rx and say 'ok 3';


