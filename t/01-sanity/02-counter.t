#!/usr/bin/pugs

# Checking that testing is sane: counted tests

use v6;

say '1..4';

my $counter = 1;
say "ok $counter";

$counter++;
say "ok $counter";

++$counter;
say 'ok ', $counter;

++$counter;
say 'ok ' ~ $counter;
