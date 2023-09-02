# Checking that testing is sane: equality and inequality
say '1..6';

my $x = '0';

($x eq $x) && say 'ok 1';
($x ne $x) && say 'not ok 2';
($x eq $x) || say 'not ok 3';
($x ne $x) || say 'ok 4';

($x == $x) && say 'ok 5';
($x != $x) || say 'ok 6';

# vim: expandtab shiftwidth=4
