#! ../../parrot
# Copyright (C) 2007-2008, The Perl Foundation.
# $Id$

=head1 NAME

t/pmc/mutablevar.t - Test the MutableVAR PMC

=head1 SYNOPSIS

    % prove t/pmc/mutablevar.t

=head1 DESCRIPTION

Tests the MutableVAR PMC.

=cut

.loadlib 'perl6_group'

.sub main :main
    .include 'include/test_more.pir'
    load_bytecode "perl6.pbc"

    plan(3)

    needs_init_pmc()
    readonly_true()
    readonly_false()
.end    


.sub needs_init_pmc
    $I0 = 1
    push_eh ok
    $P1 = new "MutableVAR"
    $I0 = 0
  ok:
    is($I0, 1, 'need to init with a scalar')
.end


.sub readonly_true
    $P0 = new 'Boolean'
    $P0 = 1
    $P1 = new 'ObjectRef'
    setprop $P1, "readonly", $P0
    $P2 = new 'MutableVAR', $P1
    $I0 = $P2.'readonly'()
    is($I0, 1, 'readonly true')
.end


.sub readonly_false
    $P0 = new 'Boolean'
    $P0 = 0
    $P1 = new 'ObjectRef'
    setprop $P1, "readonly", $P0
    $P2 = new 'MutableVAR', $P1
    $I0 = $P2.'readonly'()
    is($I0, 0, 'readonly false')
.end


# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
