#!/usr/bin/perl
# Copyright (C) 2008, The Perl Foundation.
# $Id$

use strict;
use warnings;

my @ops = qw(
  **        1
  *         1
  /         'fail'
  %         'fail'
  x         'fail'
  xx        'fail'
  +&        -1
  +<        'fail'
  +>        'fail'
  ~&        'fail'
  ~<        'fail'
  ~>        'fail'
  ?&        1
  +         0
  -         0
  ~         ''
  +|        0
  +^        0
  ~|        ''
  ~^        ''
  ?|        0
  ?^        0
);


my $output = $ARGV[0] || '-';


my $assignfmt = 
    "    optable.'newtok'('infix:%s=', 'equiv'=>'infix::=', 'lvalue'=>1)\n";
my $reducefmt =
    "    optable.'newtok'('prefix:[%s]', 'equiv'=>'infix:=')\n";

my @gtokens = ();
my @code = ();

while (@ops) {
    my $opname   = shift @ops;
    my $identity = shift @ops;

    push @gtokens, sprintf( $assignfmt, $opname );
    push @gtokens, sprintf( $reducefmt, $opname );

    push @code, qq(
        .sub 'infix:$opname='
            .param pmc a
            .param pmc b
            .tailcall '!ASSIGNMETAOP'('$opname', a, b)
        .end

        .sub 'prefix:[$opname]'
            .param pmc args    :slurpy
            .tailcall '!REDUCEMETAOP'('$opname', $identity, args)
        .end\n);
}

my $gtokens = join('', @gtokens);
  
open my $fh, "> $output" or die "Could not write $output: $!";
print $fh qq(
.namespace []
.sub '' :init :load
    .local pmc optable
    optable = get_hll_global ['Perl6';'Grammar'], '\$optable'
$gtokens
.end

);

print $fh @code;

close $fh;
0;
