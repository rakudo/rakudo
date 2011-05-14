#! perl

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
