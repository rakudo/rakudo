## $Id$

=head1 TITLE

P6metaclass - methods on P6metaclass

=head1 DESCRIPTION

=head2 Methods on P6metaclass

=over

=item add_composable

Composes a role into the class. Just maps Perl 6 method name to the
P6metaclass one.

=cut

.namespace ['P6metaclass']
.sub 'add_composable' :method
    .param pmc obj
    .param pmc composee
    .tailcall self.'compose_role'(obj, composee)
.end


=item compose()

We add this compose method so that we can augment in the
setting things that were initially defined as having
P6metaclass as their metaclass rather than ClassHOW.

=cut

.namespace ['P6metaclass']
.sub 'compose' :method
    .param pmc obj
    .return (obj)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
