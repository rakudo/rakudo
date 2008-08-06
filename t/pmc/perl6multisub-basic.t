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

    plan(4)

    'instantiate'()
    'push_and_elements'()
.end    


.sub 'instantiate'
    # Check we can instantiate a multi.
    $P0 = new "Perl6MultiSub"
    $I0 = defined $P0
    ok($I0, "instantiated Perl6MultiSub")
.end


.sub 'push_and_elements'
    # Make sure we can push subs onto the multi-sub.
    $P0 = new "Perl6MultiSub"
    $P1 = find_name 'push_test1'
    push $P0, $P1
    $I0 = elements $P0
    is($I0, 1, "added one sub")
    $P1 = find_name 'push_test2'
    push $P0, $P1
    $I0 = elements $P0
    is($I0, 2, "added two subs")

    # Make sure pushing a non-invokable dies.
    $P1 = new 'Integer'
    $I0 = 0
    push_eh fails_ok
    push $P0, $P1
    goto done
  fails_ok:
    $I0 = 1
  done:
    is($I0, 1, "cannot push a non-invokable")
.end
.sub push_test1
    .return (1)
.end
.sub push_test2
    .param pmc x
    .return (2)
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
