#! ../../parrot
# Copyright (C) 2009, The Perl Foundation.
# $Id$

=head1 NAME

t/pmc/p6invocation-1.t - Test the P6Invocation PMC

=head1 SYNOPSIS

    % prove t/pmc/p6invocation-1.t

=head1 DESCRIPTION

Test the P6Invocation PMC.

=cut

.loadlib 'perl6_group'

.sub 'main'
    say "1..6"
    
    $P0 = new 'ResizablePMCArray'
    $P1 = find_name 'first'
    push $P0, $P1
    $P1 = find_name 'second'
    push $P0, $P1
    $P1 = find_name 'third'
    push $P0, $P1
    $P0 = new 'P6Invocation', $P0
    say "ok 1"

    $P0()

    say "ok 6"
.end

.sub 'first'
    .lex '__CANDIATE_LIST__', $P0
    say "ok 2"

    $S0 = typeof $P0
    if $S0 == "P6Invocation" goto ok_3
    print "not "
  ok_3:
    say "ok 3"

   $P0()
.end

.sub 'second'
    .lex '__CANDIATE_LIST__', $P0
    say "ok 4"
    $P0()
.end

.sub 'third'
    .lex '__CANDIATE_LIST__', $P0
    say "ok 5"
.end


# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
