#! ../../parrot
# Copyright (C) 2007-2008, The Perl Foundation.
# $Id$

=head1 NAME

t/pmc/perl6multisub-basic.t - Basics tests for the perl6multisub PMC

=head1 SYNOPSIS

    % prove t/pmc/perl6multisub-basic.t

=head1 DESCRIPTION

A few basic sanity tests for the Perl 6 MultiSub PMC.

=cut

.loadlib 'perl6_group'

.sub main :main
    .include 'include/test_more.pir'
    load_bytecode "perl6.pbc"

    plan(1)

    'instantiate'()
.end    


.sub 'instantiate'
    # Check we can instantiate a multi.
    $P0 = new "Perl6MultiSub"
    $I0 = defined $P0
    ok($I0, "instantiated Perl6MultiSub")
.end


# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
