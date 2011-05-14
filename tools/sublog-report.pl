#! perl

use warnings;
use strict;


my %called;

while (<>) {
    chomp;
    my ($called, $caller) = split("\t", $_, 2);
    $called{$called}++;
}


my @called_keys = sort { $called{$b} <=> $called{$a} } keys %called;

foreach my $key (@called_keys) {
    printf "%9d  %s\n", $called{$key}, $key;
}
