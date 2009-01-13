## $Id$

=head1 TITLE

Match - Perl 6 match objects

=head1 Description

=cut

.sub '' :anon :load :init
    .local pmc p6meta, matchproto
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    matchproto = p6meta.'new_class'('Match', 'parent'=>'PGE::Match Any')
    $P0 = get_hll_global 'Positional'
    $P0 = $P0.'!select'()
    p6meta.'add_role'($P0, 'to'=>matchproto)
    $P0 = get_hll_global 'Associative'
    $P0 = $P0.'!select'()
    p6meta.'add_role'($P0, 'to'=>matchproto)
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
