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
    say "1..9"
    
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

    say "ok 9"
.end

.sub 'first'
    .lex '__CANDIATE_LIST__', $P0
    say "ok 2"

    $S0 = typeof $P0
    if $S0 == "P6Invocation" goto ok_3
    print "not "
  ok_3:
    say "ok 3"

    $I0 = istrue $P0
    if $I0 != 0 goto ok_4
    print "not"
  ok_4:
    say "ok 4"

    $P0()
.end

.sub 'second'
    .lex '__CANDIATE_LIST__', $P0
    say "ok 5"

    $I0 = istrue $P0
    if $I0 != 0 goto ok_6
    print "not"
  ok_6:
    say "ok 6"

    $P0()
.end

.sub 'third'
    .lex '__CANDIATE_LIST__', $P0
    say "ok 7"

    $I0 = istrue $P0
    if $I0 == 0 goto ok_8
    print "not"
  ok_8:
    say "ok 8"
.end


# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
