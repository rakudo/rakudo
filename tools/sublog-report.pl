#! perl
#
# This tool analyzes the "sublog" output of Rakudo to produce a
# simple call analysis.
#
# To create a sublog, simply invoke Rakudo with the "RAKUDO_SUBLOG"
# environment variable set to the name of the file in which to
# record the log output.  For example:
#      $ RAKUDO_SUBLOG=sub.log ./perl6 t/spec/S32-trig/sin.t
# This will run the desired program, recording each subroutine
# entry in the file "sub.log".
#
# Once a sublog file has been created, you can use it as input
# to this program to produce a summary of the number of calls to
# each subroutine and the top eight callers for each:
#      $ perl tools/sublog-report.pl sub.log
#

use warnings;
use strict;
use POSIX qw(ceil);


my %called;
my %calledfrom;

while (<>) {
    chomp;
    my ($called, $caller) = split("\t", $_, 2);
    $called{$called}++;
    $calledfrom{$called}{$caller}++
}


my @called_keys = sort { $called{$b} <=> $called{$a} } keys %called;

my $places = POSIX::ceil(log($called{$called_keys[0]})/log(10));
my $fmt = "%${places}d  %s\n";
my $indent = ' ' x $places;


foreach my $key (@called_keys) {
    printf $fmt, $called{$key}, $key;
    my @cfkeys = sort { $calledfrom{$key}{$b} <=> $calledfrom{$key}{$a} }
                      keys %{$calledfrom{$key}};
    my $count = 0;
    foreach my $k (@cfkeys) {
        printf "%s  $fmt", $indent, $calledfrom{$key}{$k}, $k;
        last if ++$count >= 8;
    }
    printf "%s  %s  ... and %d more\n", $indent, $indent, @cfkeys - $count if @cfkeys > $count;
}
