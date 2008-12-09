## $Id$

=head1 TITLE

Match - Perl 6 match objects

=head1 Description

At the moment file is a dummy file, it does nothing more than
cause PGE::Match objects to act as mutables via the Scalar method.
Eventually we'll derive a proper Match subclass here that can
do it the same way as other Rakudo classes, but this is a
good workaround for now.

(We have to handle mutable-ness specially here, because PGE::Match
is derived from Parrot's Hash class, and Rakudo's Mapping class
causes Parrot's Hash to act like an immutable.  HLL mapping would
help here, too.)

=over 4

=item onload

=cut

.namespace ['PGE';'Match']

.sub '' :anon :load :init
    $P0 = get_hll_global ['PGE'], 'Match'
    $P0.'!MUTABLE'()

    # Also install Match proto in our HLL namespace.
    set_hll_global 'Match', $P0

    .local pmc p6meta
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    $P1 = get_hll_global 'Positional'
    p6meta.'add_role'($P1, 'to'=>$P0)
.end

#
#.sub 'Scalar' :method
#    $I0 = isa self, 'ObjectRef'
#    unless $I0 goto not_ref
#    .return (self)
#  not_ref:
#    $P0 = new 'ObjectRef', self
#    .return ($P0)
#.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
