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

    plan(13)

    'basic_class'()
    'role'()
    'ordered_class'()
.end


.sub 'basic_class'
    $P0 = new "Perl6MultiSub"
    $P1 = get_global 'basic_class_1'
    'attach_sig'($P1, 'Int')
    push $P0, $P1
    $P1 = get_global 'basic_class_2'
    'attach_sig'($P1, 'Junction')
    push $P0, $P1

    $P1 = new 'Int'
    $P1 = 42
    $I0 = $P0($P1)
    is($I0, 1, 'dispatch on class')
    $P1 = new 'Junction'
    $I0 = $P0($P1)
    is($I0, 2, 'dispatch on class')
.end
.sub 'basic_class_1'
    .param pmc a
    .return (1)
.end
.sub 'basic_class_2'
    .param pmc a
    .return (2)
.end


.sub 'role'
    # Create a couple of roles.
    .local pmc R1, R2
    R1 = '!keyword_role'('R1')
    R2 = '!keyword_role'('R2')

    # Set up multis.
    $P0 = new "Perl6MultiSub"
    $P1 = get_global 'role_1'
    'attach_sig'($P1, 'R1')
    push $P0, $P1
    $P1 = get_global 'role_2'
    'attach_sig'($P1, 'R2')
    push $P0, $P1

    # Couple of classes that do the roles.
    .local pmc C1, C2, p6meta
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    C1 = p6meta.'new_class'('C1', 'parent'=>'Any')
    p6meta.'add_role'(R1, 'to'=>C1)
    C2 = p6meta.'new_class'('C2', 'parent'=>'Any')
    p6meta.'add_role'(R2, 'to'=>C2)

    # Tests
    $P1 = C1.'new'()
    $I0 = $P0($P1)
    is($I0, 1, 'dispatch on a role')
    $P1 = C2.'new'()
    $I0 = $P0($P1)
    is($I0, 2, 'dispatch on a role')
.end
.sub 'role_1'
    .param pmc a
    .return (1)
.end
.sub 'role_2'
    .param pmc a
    .return (2)
.end


.sub 'ordered_class'
    # Create 3 classes.
    .local pmc p6meta
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    p6meta.'new_class'('Paper', 'parent'=>'Any')
    p6meta.'new_class'('Scissors', 'parent'=>'Any')
    p6meta.'new_class'('Stone', 'parent'=>'Any')

    $P0 = new "Perl6MultiSub"
    $P1 = get_global 'ordered_class_1'
    'attach_sig'($P1, 'Any', 'Any')
    push $P0, $P1
    $P1 = get_global 'ordered_class_2'
    'attach_sig'($P1, 'Paper', 'Stone')
    push $P0, $P1
    $P1 = get_global 'ordered_class_3'
    'attach_sig'($P1, 'Stone', 'Scissors')
    push $P0, $P1
    $P1 = get_global 'ordered_class_4'
    'attach_sig'($P1, 'Scissors', 'Paper')
    push $P0, $P1

    .local pmc paper, scissors, stone
    paper = get_hll_global 'Paper'
    paper = paper.'new'()
    scissors = get_hll_global 'Scissors'
    scissors = scissors.'new'()
    stone = get_hll_global 'Stone'
    stone = stone.'new'()

    $I0 = $P0(paper, paper)
    is($I0, 0, 'topological sorting')
    $I0 = $P0(paper, scissors)
    is($I0, 0, 'topological sorting')
    $I0 = $P0(paper, stone)
    is($I0, 1, 'topological sorting')
    $I0 = $P0(scissors, paper)
    is($I0, 1, 'topological sorting')
    $I0 = $P0(scissors, scissors)
    is($I0, 0, 'topological sorting')
    $I0 = $P0(scissors, stone)
    is($I0, 0, 'topological sorting')
    $I0 = $P0(stone, paper)
    is($I0, 0, 'topological sorting')
    $I0 = $P0(stone, scissors)
    is($I0, 1, 'topological sorting')
    $I0 = $P0(stone, stone)
    is($I0, 0, 'topological sorting')
.end
.sub 'ordered_class_1'
    .param pmc a
    .param pmc b
    .return (0)
.end
.sub 'ordered_class_2'
    .param pmc a
    .param pmc b
    .return (1)
.end
.sub 'ordered_class_3'
    .param pmc a
    .param pmc b
    .return (1)
.end
.sub 'ordered_class_4'
    .param pmc a
    .param pmc b
    .return (1)
.end

.sub 'attach_sig'
    .param pmc sub
    .param pmc types :slurpy
    
    # Make signature.
    .local pmc true
    true = new 'Integer'
    true = 1
    $P0 = new 'Signature'
    $P1 = new 'Perl6Array'
    setattribute $P0, "@!params", $P1
    .local pmc it, type
    it = iter types
  param_loop:
    unless it goto param_loop_end
    $P3 = shift it
    $S0 = $P3
    type = get_hll_global $S0
    $P2 = new 'Perl6Hash'
    $P2["nom_type"] = type
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
