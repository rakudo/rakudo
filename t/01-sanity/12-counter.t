use v6;

# L<S02/"Whitespace and Comments"/space before it, but may be written in any>

# Checking that testing is sane: counted tests

say '1..4';

my $counter = 1;
say "ok $counter";

$counter++;
say "ok $counter";

++$counter;
say 'ok ', $counter;

++$counter;
say 'ok ' ~ $counter;

# vim: expandtab shiftwidth=4
