#! ../../parrot
# Copyright (C) 2007-2008, The Perl Foundation.
# $Id$

=head1 NAME

t/pmc/perl6multisub-arity.t - Arity based dispatch tests

=head1 SYNOPSIS

    % prove t/pmc/perl6multisub-arity.t

=head1 DESCRIPTION

Tests for arity based dispatch using the Perl 6 MultiSub PMC.

=cut

.loadlib 'perl6_group'

.sub main :main
    .include 'include/test_more.pir'
    load_bytecode "perl6.pbc"

    plan(13)

    'simple'()
    'with_optional'()
    'with_slurpy'()
    'another_with_slurpy'()
.end    


.sub 'simple'
    $P0 = new "Perl6MultiSub"
    $P1 = find_global 'simple_1'
    'attach_any_sig'($P1, 0)
    push $P0, $P1
    $P1 = find_global 'simple_2'
    'attach_any_sig'($P1, 1)
    push $P0, $P1
    $P1 = find_global 'simple_3'
    'attach_any_sig'($P1, 3)
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


.sub 'with_optional'
    $P0 = new "Perl6MultiSub"
    $P1 = find_global 'with_optional_1'
    'attach_any_sig'($P1, 0)
    push $P0, $P1
    $P1 = find_global 'with_optional_2'
    'attach_any_sig'($P1, 2)
    push $P0, $P1

    $I0 = $P0()
    is($I0, 0, 'call with 0 args')
    $I0 = $P0(1)
    is($I0, 1, 'with 1 arg - optional not supplied')
    $I0 = $P0(1, 2)
    is($I0, 1, 'with 2 args - optional supplied')
.end
.sub 'with_optional_1'
    .return (0)
.end
.sub 'with_optional_2'
    .param int i
    .param int j :optional
    .return (1)
.end


.sub 'with_slurpy'
    $P0 = new "Perl6MultiSub"
    $P1 = find_global 'with_slurpy_1'
    'attach_any_sig'($P1, 0)
    push $P0, $P1
    
    $I0 = $P0()
    is($I0, 42, 'call with 0 args to slurpy')
    $I0 = $P0(1)
    is($I0, 42, 'with 1 arg to slurpy')
    $I0 = $P0(1, 2)
    is($I0, 42, 'with 2 args to slurpy')
.end
.sub 'with_slurpy_1'
    .param pmc params :slurpy
    .return (42)
.end


.sub 'another_with_slurpy'
    $P0 = new "Perl6MultiSub"
    $P1 = find_global 'another_with_slurpy_1'
    'attach_any_sig'($P1, 0)
    push $P0, $P1
    $P1 = find_global 'another_with_slurpy_2'
    'attach_any_sig'($P1, 1)
    push $P0, $P1
    
    $I0 = $P0()
    is($I0, 0, 'call with 0 args - not to slurpy')
    $I0 = $P0(1)
    is($I0, 1, 'with 1 arg, giving empty slurpy')
    $I0 = $P0(1, 2, 3)
    is($I0, 1, 'with 3 args, giving slurpy values')
.end
.sub 'another_with_slurpy_1'
    .return (0)
.end
.sub 'another_with_slurpy_2'
    .param int x
    .param pmc xs :slurpy
    .return (1)
.end


.sub 'attach_any_sig'
    .param pmc sub
    .param int num_params

    # Get Any type.
    .local pmc any
    any = get_hll_global "Any"
    
    # Make signature.
    $P0 = new 'Signature'
    $P1 = new 'Perl6Array'
    setattribute $P0, "@!params", $P1
    $I0 = 0
  param_loop:
    if $I0 == num_params goto param_loop_end
    $P2 = new 'Perl6Hash'
    $P2["type"] = any
    push $P1, $P2
    inc $I0
    goto param_loop
  param_loop_end:

    setprop sub, '$!signature', $P0
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
