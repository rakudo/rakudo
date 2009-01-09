#! ../../parrot
# Copyright (C) 2007-2008, The Perl Foundation.
# $Id$

=head1 NAME

t/pmc/objectref.t - Test the ObjectRef PMC

=head1 SYNOPSIS

    % prove t/pmc/objectref.t

=head1 DESCRIPTION

Tests the ObjectRef PMC.

=cut

.loadlib 'perl6_group'

.sub main :main
    .include 'include/test_more.pir'
    load_bytecode "perl6.pbc"

    plan(4)

    init()
    init_pmc()
    meth_call()
    multi_call()
.end    


.sub init
    # ObjectRef is initialized to contain an undef.
    $P1 = new "ObjectRef"
    $S1 = typeof $P1
    is($S1, 'Undef', 'typeof newclass retval')
.end


.sub init_pmc
    # Assigning a value.
    $P2 = get_hll_global 'Int'
    $P2 = $P2.'new'()
    $P2 = 42
    $P1 = new 'ObjectRef', $P2

    # Get integer value; see what we have stored.
    $I0 = $P1
    is($I0, 42, 'stored value')
.end


.sub meth_call
    # Check we can call methods.
    $P2 = 'list'(1,2,3)
    $P1 = new 'ObjectRef', $P2
    $I0 = $P1.'elems'()
    is($I0, 3, 'method calls on value work')
.end


.sub multi_call
    # Try and do a multi-dispatch call with two items.
    .local pmc x, y
    $P2 = get_hll_global 'Int'
    $P3 = $P2.'new'()
    $P3 = 35
    x = new 'ObjectRef', $P3
    $P4 = $P2.'new'()
    $P4 = 7
    y = new 'ObjectRef', $P4
    $P5 = 'infix:+'(x, y)
    $I0 = $P5
    is($I0, 42, 'multi call worked')
.end


# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
