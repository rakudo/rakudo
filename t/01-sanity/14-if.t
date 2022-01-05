use v6;

# Checking that testing is sane: if
say '1..9';

my $x = '0';

if ($x eq $x) { say     'ok 1' } else { say 'not ok 1' }
if ($x ne $x) { say 'not ok 2' } else { say     'ok 2' }

if (1) { say     'ok 3' } elsif 0 { say 'not ok 3' } else { say 'not ok 3' }
if (0) { say 'not ok 4' } elsif 1 { say     'ok 4' } else { say 'not ok 4' }
if (0) { say 'not ok 5' } elsif 0 { say 'not ok 5' } else { say     'ok 5' }

if    0 { say 'not ok 6' }
elsif 1 { say     'ok 6' }
elsif 0 { say 'not ok 6' }
else    { say 'not ok 6' }

if    0 { say 'not ok 7' }
elsif 0 { say 'not ok 7' }
elsif 1 { say     'ok 7' }
else    { say 'not ok 7' }

if    0 { say 'not ok 8' }
elsif 1 { say     'ok 8' }
elsif 1 { say 'not ok 8' }
else    { say 'not ok 8' }

unless 0 { say    'ok 9' }

# vim: expandtab shiftwidth=4
