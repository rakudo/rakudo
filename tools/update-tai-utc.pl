#!/usr/bin/perl
# Updates src/core/tai-utc.pm.

use warnings;
use strict;
use Time::y2038 'timegm';
use File::Slurp qw(slurp write_file);
use LWP::Simple 'get';

my $url = 'ftp://hpiers.obspm.fr/iers/bul/bulc/TimeSteps.history';

$ARGV[0] or die "Please provide a path to src/core/tai-utc.pm.\n";
my $tu_path = $ARGV[0];

my @dates = do {
    my @lines = split /\n/, get $url;
    pop @lines;
    shift @lines until $lines[0] =~ /\A 1972  Jul\.   1/;
    map {
        /(\d{4})  (Jan|Jul)/;
        $2 eq 'Jan' ? [$1 - 1, 12, 31] : [$1, 6, 30]
    } @lines
};

my $tu = slurp $tu_path;
sub replace {
    my ($find, $fmt, $f) = @_;
    $tu =~ s
        {^( *)#BEGIN $find\n.+?^ *#END $find\n}
        { sprintf "$1#BEGIN $find\n$1<\n%s\n$1>\n$1#END $find\n", join "\n",
              map { sprintf "%s$fmt", $1, $f->(@$_) } @dates }ems
      or die "Couldn't replace $find";
}
replace 'leap-second-dates', '%d-%02d-%02d', sub { @_ };
replace 'leap-second-posix', '%10d', sub {
    my ($y, $m, $d) = @_;
    1 + timegm 59, 59, 23, $d, $m - 1, $y - 1900;
};
write_file $tu_path, $tu;
print "Updated.\n";

# The IERS announces midyear leap seconds in early January and
# end-of-year leap seconds in early July. So:

my $month = (gmtime)[4];
printf "This program should next be run in %s.\n",
    1 < $month && $month < 8 ? 'August' : 'February';
