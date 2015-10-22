#!/usr/bin/env perl6

use LWP::Simple;

my $json = LWP::Simple.get("http://www.pm.org/groups/perl_mongers.json");
my @pm-names = from-json($json).map:{ $_<name> ~~ s/ '.pm' $//; $_<name> };
my @release-names = grep { / ^ <[A..Z]> \w+ / }, qx{git tag}.lines;
.say for (@pm-names (-) @release-names).map(*.key).sort;
