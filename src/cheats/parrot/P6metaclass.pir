## $Id$

=head1 TITLE

P6metaclass - methods on P6metaclass

=head1 DESCRIPTION

=head2 Methods on P6metaclass

=over

=item compose()

We add this compose method so that we can augment in the
setting things that were initially defined as having
P6metaclass as their metaclass rather than ClassHOW.

=cut

.namespace ['P6metaclass']
.sub 'compose' :method
    .param pmc ignored
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
