#!/usr/bin/pugs

use v6;

say "1..1";

my $string = "Pugs -- Perl6 User's Golfing System";
my $substr = substr $string, 8, 5;

if $substr eq "Perl6" { say "ok 1" } else { say "not ok 1" }
