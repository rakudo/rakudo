#!/usr/bin/perl
# Copyright (C) 2013, The Perl Foundation.

use strict;
use warnings;
use 5.008;

my ($existing) = shift @ARGV;

unless (-e $existing) {
    $existing = "$existing.bat";
}
unless (-e $existing) {
    die "Could not find " . $ARGV[0];
}

open my $fh, "<", $existing;
my $runner;
while (<$fh>) {
    $runner = $_;
}
close $fh;

$runner =~ s/nqp-runtime\.jar\;/nqp-runtime.jar;rakudo-runtime.jar;/;
$runner =~ s/nqp-runtime\.jar\:/nqp-runtime.jar:rakudo-runtime.jar:/;

my $args = join ' ', @ARGV;
$runner =~ s/"\$\@"/$args/;
$runner =~ s/\%\*/$args/;

system $runner;

# vim: expandtab sw=4
