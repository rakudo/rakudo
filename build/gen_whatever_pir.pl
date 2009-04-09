#!/usr/bin/perl
# Copyright (C) 2008, The Perl Foundation.
# $Id$

use strict;
use warnings;

my @ops = qw(
  infix:** infix:* infix:/ infix:% infix:div infix:mod infix:+
  infix:== infix:!= infix:<  infix:>  infix:<= infix:>= infix:<=>
);

my $output = $ARGV[0] || "-";

open my $fh, "> $output" or die "Could not write $output: $!";

for (@ops) {
    my $gen = '
        .namespace []
        .sub "$_" :multi("Whatever", _)
            .param pmc x
            .param pmc y
            $P0 = get_hll_global "$_"
            .lex "$op", $P0
            .lex "$known", y
            .const "Sub" $P1 = "!whatever_helper_left"
            $P1 = clone $P1
            $P2 = interpinfo .INTERPINFO_CURRENT_SUB
            $P1."set_outer"($P2)
            capture_lex $P1
            .return ($P1)
        .end
        .sub "$_" :multi(_, "Whatever")
            .param pmc x
            .param pmc y
            $P0 = get_hll_global "$_"
            .lex "$op", $P0
            .lex "$known", x
            .const "Sub" $P1 = "!whatever_helper_right"
            $P1 = clone $P1
            $P2 = interpinfo .INTERPINFO_CURRENT_SUB
            $P1."set_outer"($P2)
            capture_lex $P1
            .return ($P1)
        .end
    ';
    $gen =~ s/\$_/$_/g;
    print $fh $gen;
}

print $fh '
.sub "!whatever_helper_left"
    .param pmc right
    $P0 = find_lex "$op"
    $P1 = find_lex "$known"
    .tailcall $P0($P1, right)
.end
.sub "!whatever_helper_right"
    .param pmc left
    $P0 = find_lex "$op"
    $P1 = find_lex "$known"
    .tailcall $P0(left, $P1)
.end
';

close $fh or die $!;
