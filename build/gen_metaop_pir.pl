#!/usr/bin/perl
# Copyright (C) 2008, The Perl Foundation.
# $Id$

use strict;
use warnings;

my @ops = qw(
  **        1           op
  *         1           op
  /         'fail'      op
  %         'fail'      op
  x         'fail'      op
  xx        'fail'      op
  min       'fail'      op
  max       'fail'      op
  +&        -1          op
  +<        'fail'      op
  +>        'fail'      op
  ~&        'fail'      op
  ~<        'fail'      op
  ~>        'fail'      op
  ?&        1           op
  +         0           op
  -         0           op
  ~         ''          op
  +|        0           op
  +^        0           op
  ~|        ''          op
  ~^        ''          op
  ?|        0           op
  ?^        0           op
  !==       'False'     comp
  !=        'False'     comp
  ==        'True'      comp
  <         'True'      comp
  <=        'True'      comp
  >         'True'      comp
  >=        'True'      comp
  ~~        'True'      comp
  !~~       'False'     comp
  eq        'True'      comp
  ne        'False'     comp
  lt        'True'      comp
  le        'True'      comp
  gt        'True'      comp
  ge        'True'      comp
  ===       'True'      comp
  !===      'False'     comp
  =:=       'True'      comp
  !=:=      'False'     comp
);


my $output = $ARGV[0] || '-';


my $assignfmt =
    "    optable.'newtok'('infix:%s=', 'equiv'=>'infix::=', 'lvalue'=>1)\n";
my $reducefmt =
    "    optable.'newtok'('prefix:[%s]', 'equiv'=>'infix:=')\n";
my $hyper_no_dwim_fmt =
    "    optable.'newtok'(%s, 'equiv'=>'infix:%s')\n" .
    "    optable.'newtok'('infix:%s', 'equiv'=>'infix:%s', 'subname'=>%s)\n";
my $crossfmt =
    "    optable.'newtok'('infix:X%s', 'equiv'=>'infix:X')\n";

my @gtokens = ();
my @code = ();

while (@ops) {
    my $opname   = shift @ops;
    my $identity = shift @ops;
    my $op_type  = shift @ops;

    # Only emit assignment meta-ops for standard ops.
    if ($op_type eq 'op') {
        push @gtokens, sprintf( $assignfmt, $opname );
        push @code, qq(
        .sub 'infix:$opname='
            .param pmc a
            .param pmc b
            .tailcall '!ASSIGNMETAOP'('$opname', a, b)
        .end\n);
    }

    # All ops work for reductions.
    push @gtokens, sprintf( $reducefmt, $opname );
    my $chain = $op_type eq 'comp' ? 'CHAIN' : '';
    push @code, qq(
        .sub 'prefix:[$opname]'
            .param pmc args    :slurpy
            .tailcall '!REDUCEMETAOP$chain'('$opname', $identity, args)
        .end\n);

    # Cross operators.
    push @gtokens, sprintf( $crossfmt, $opname );
    my $is_chaining = $op_type eq 'comp' ? 1 : 0;
    push @code, qq(
        .sub 'infix:X${opname}'
            .param pmc a
            .param pmc b
            .tailcall '!CROSSMETAOP'('$opname', $identity, $is_chaining, a, b)
        .end\n);

    # Non-dwimming hyper ops.
    my $hypername = qq(unicode:"infix:\\u00ab$opname\\u00bb");
    push @gtokens, sprintf($hyper_no_dwim_fmt, $hypername, $opname, ">>$opname<<", $opname, $hypername);
    push @code, qq(
        .sub $hypername
            .param pmc a
            .param pmc b
            .tailcall '!HYPEROP'('$opname', a, b, 0, 0)
        .end\n);

    # LHS-dwimming hyper ops.
    $hypername = qq(unicode:"infix:\u00bb$opname\\u00bb");
    push @gtokens, sprintf($hyper_no_dwim_fmt, $hypername, $opname, "<<$opname<<", $opname, $hypername);
    push @code, qq(
        .sub $hypername
            .param pmc a
            .param pmc b
            .tailcall '!HYPEROP'('$opname', a, b, 1, 0)
        .end\n);

    # RHS-dwimming hyper ops.
    $hypername = qq(unicode:"infix:\\u00ab$opname\\u00ab");
    push @gtokens, sprintf($hyper_no_dwim_fmt, $hypername, $opname, ">>$opname>>", $opname, $hypername);
    push @code, qq(
        .sub $hypername
            .param pmc a
            .param pmc b
            .tailcall '!HYPEROP'('$opname', a, b, 0, 1)
        .end\n);

    # Dwimming hyper ops.
    $hypername = qq(unicode:"infix:\\u00bb$opname\\u00ab");
    push @gtokens, sprintf($hyper_no_dwim_fmt, $hypername, $opname, "<<$opname>>", $opname, $hypername);
    push @code, qq(
        .sub $hypername
            .param pmc a
            .param pmc b
            .tailcall '!HYPEROP'('$opname', a, b, 1, 1)
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
