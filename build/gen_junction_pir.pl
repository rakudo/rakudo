#!/usr/bin/perl
# Copyright (C) 2008, The Perl Foundation.
# $Id$

my @binary = qw(
  infix:**
  infix:* infix:/ infix:% infix:div infix:mod
  infix:+ infix:-
  infix:== infix:!= infix:<  infix:>  infix:<= infix:>=
  infix:eq infix:ne infix:lt infix:gt infix:le infix:ge
);

my @unary = qw(
  prefix:++ prefix:-- postfix:++ postfix:--
);

my $output = $ARGV[0] || '-';

open my $fh, "> $output" or die "Could not write $output: $!";

for (@unary) {
    print $fh qq(
        .namespace []
        .sub '$_' :multi('Junction')
            .param pmc x
            .tailcall '!DISPATCH_JUNCTION'('$_', x)
        .end
    );
}

for (@binary) {
    print $fh qq(
        .namespace []
        .sub '$_' :multi('Junction', _)
            .param pmc x
            .param pmc y
            .tailcall '!DISPATCH_JUNCTION'('$_', x, y)
        .end

        .sub '$_' :multi(_, 'Junction')
            .param pmc x
            .param pmc y
            .tailcall '!DISPATCH_JUNCTION'('$_', x, y)
        .end
    );
}

close $fh;
0;
