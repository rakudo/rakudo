#! ../../parrot
# Copyright (C) 2007-2008, The Perl Foundation.
# $Id$

=head1 NAME

t/pmc/perl6multisub-type.t - Type based dispatch tests

=head1 SYNOPSIS

    % prove t/pmc/perl6multisub-type.t

=head1 DESCRIPTION

Tests for type based dispatch using the Perl 6 MultiSub PMC.

=cut

.loadlib 'perl6_group'

.sub main :main
    .include 'include/test_more.pir'
    load_bytecode "perl6.pbc"

    plan(2)

    'constraint_tiebreak'()
.end


.sub 'constraint_tiebreak'
    $P0 = new "Perl6MultiSub"
    $P1 = find_global 'constraint_tiebreak_1'
    $P2 = null
    'attach_sig'($P1, $P2)
    push $P0, $P1
    $P1 = find_global 'constraint_tiebreak_2'
    $P2 = find_global 'constraint_tiebreak_2_con'
    'attach_sig'($P1, $P2)
    push $P0, $P1

    $P1 = new 'Int'
    $P1 = 42
    $I0 = $P0($P1)
    is($I0, 2, 'constraint tie-breaks')
    $P1 = 13
    $I0 = $P0($P1)
    is($I0, 1, 'constraint tie-breaks')
.end
.sub 'constraint_tiebreak_1'
    .param pmc a
    .return (1)
.end
.sub 'constraint_tiebreak_2'
    .param pmc a
    .return (2)
.end
.sub 'constraint_tiebreak_2_con'
    .param int i
    $I0 = i == 42
    .return ($I0)
.end

.sub 'attach_sig'
    .param pmc sub
    .param pmc constraints :slurpy
    
    # Make signature.
    .local pmc any, true
    any = get_hll_global 'Any'
    true = new 'Integer'
    true = 1
    $P0 = new 'Signature'
    $P1 = new 'Perl6Array'
    setattribute $P0, "@!params", $P1
    .local pmc it, con
    it = iter constraints
  param_loop:
    unless it goto param_loop_end
    con = shift it
    $P2 = new 'Perl6Hash'
    $P2["type"] = any
    $P2["constraints"] = con
    $P2["multi_invocant"] = true
    push $P1, $P2
    goto param_loop
  param_loop_end:

    setprop sub, '$!signature', $P0
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
