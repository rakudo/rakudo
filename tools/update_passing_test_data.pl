#! perl
# Copyright (C) 2001-2008, The Perl Foundation.
# $Id$
use strict;
use warnings;

# this ugly piece of perl 5 code runs 'make spectest' and looks
# for passing test files that are not yet included in the
# 'spectest_regression' target. The output is an evil mess, but still useful.

my $test_file = $ARGV[0] || 't/spectest_regression.data';

open my $f, '<', $test_file
    or die "Can't open file '$test_file' for reading: $!";

my %known_tests;
while (<$f>){
    chomp;
    next if m/^\s*(?:#|$)/;
    $known_tests{$_}++;
}
close $f;

print "Running the test suite...";
$| = 1;

my @new;

open my $t, 'make spectest 2>&1 |'
    or die "Can't run make: $!";

while(<$t>){
    # hack until the looping parse is fixed
    unlink glob 't/spec/S29-conversions/ord_and_chr.*';
    print '.';
    if (m{^t/spec/(.*?)\.+ok$}){
        my $test = $1 . '.t';
        if (!$known_tests{$test}){
            push @new, $test;
            print "\n<< new: $test>>\n";
        }
    }
}
close $t;
if (@new){
    print "\nNewly passing tests:\n";
    print "$_\n" for @new;
}

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:

