#!/usr/bin/perl
# Copyright (C) 2013, The Perl Foundation.

use strict;
use warnings;
use 5.008;

# assume $outopt is -o
my ($outopt, $out, $in) = @ARGV;

rename $in, $out;

