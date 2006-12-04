use v6-alpha;

# Checking that testing is sane: equality and inequality


say '1..4';

my $x = '0';

($x eq $x) && say 'ok 1';
($x ne $x) && say 'not ok 1';
($x eq $x) || say 'not ok 2';
($x ne $x) || say 'ok 2';

($x == $x) && say 'ok 3';
($x != $x) || say 'ok 4';
