#!/usr/bin/pugs

# Checking that testing is sane: Test.pm

use v6;
use Test;

plan 1;

my $x = '0';
ok $x == $x;
