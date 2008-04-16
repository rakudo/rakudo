# L<S02/"Whitespace and Comments"/space before it, but may be written in any>
use v6;

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
