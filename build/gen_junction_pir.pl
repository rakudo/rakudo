#!/usr/bin/perl
# Copyright (C) 2008-2009, The Perl Foundation.
# $Id$

use strict;
use warnings;

my @binary = qw(
  infix:**
  infix:* infix:/ infix:% infix:div infix:mod
  infix:+ infix:-
  infix:~
  infix:== infix:!= infix:<  infix:>  infix:<= infix:>=
  infix:eq infix:ne infix:lt infix:gt infix:le infix:ge
  infix:<=> infix:leg infix:cmp infix:=:=
);

my @unary = qw(
  prefix:++ prefix:-- postfix:++ postfix:--
);

for (@unary) {
    print qq(
        .namespace []
        .sub '$_' :multi('Junction')
            .param pmc x
            .tailcall '!DISPATCH_JUNCTION'('$_', x)
        .end
    );
}

for (@binary) {
    print qq(
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
