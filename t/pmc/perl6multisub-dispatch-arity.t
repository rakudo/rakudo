#! ../../parrot
# Copyright (C) 2007-2008, The Perl Foundation.
# $Id$

=head1 NAME

t/pmc/perl6multisub-dispatch-arity.t - Arity based dispatch tests

=head1 SYNOPSIS

    % prove t/pmc/perl6multisub-dispatch-arity.t

=head1 DESCRIPTION

Tests for arity based dispatch using the Perl 6 MultiSub PMC.

=cut

.loadlib 'perl6_group'

.sub main :main
    .include 'include/test_more.pir'
    load_bytecode "perl6.pbc"

    plan(4)

    'simple'()
.end    


.sub 'simple'
    $P0 = new "Perl6MultiSub"
    $P1 = find_global 'simple_1'
    push $P0, $P1
    $P1 = find_global 'simple_2'
    push $P0, $P1
    $P1 = find_global 'simple_3'
    push $P0, $P1

    $I0 = $P0()
    is($I0, 0, 'simple call with 0 args')
    $I0 = $P0(1)
    is($I0, 1, 'simple call with 1 arg')
    $I0 = $P0(1, 2, 3)
    is($I0, 3, 'simple call with 3 args')
    $I0 = 0
    push_eh fails
    $P0(1, 2)
  fails:
    $I0 = 1
  ok:
    is($I0, 1, 'call with no arity match fails')
.end
.sub 'simple_1'
    .return (0)
.end
.sub 'simple_2'
    .param int i
    .return (1)
.end
.sub 'simple_3'
    .param int i
    .param int j
    .param int k
    .return (3)
.end


# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
