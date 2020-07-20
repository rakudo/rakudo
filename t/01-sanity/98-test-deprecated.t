use v6;

use lib <lib>;
use Test;

my $approx1 = is_approx 1, 1, 'is_approx with description';
ok $approx1, 'is_approx 1,1, returns True';
my $approx2 = is_approx 1, 1;
my $approx3 = is_approx 1, 1.000001, 'is_approx with small difference';
ok $approx3, 'is_approx 1,1.000001, returns True';

# NOT_TODO
todo( 'failing is_approx 1,2;');
my $approx4 = is_approx 1, 2, 'is_approx with small difference';
nok $approx4, 'is_approx 1, 2; fails and returns False';

ok Deprecation.report ~~ /is_approx/, 'is_approx is deprecated';
done-testing;

# vim: expandtab shiftwidth=4
