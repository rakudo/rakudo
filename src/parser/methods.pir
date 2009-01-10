## $Id$

=head1 TITLE

methods.pir - Methods on Perl6::Grammar.

=head2 DESCRIPTION

STD.pm contains various methods. This file implements some of them.

=over 4

=item C<add_type(type_name)>

Registers a type in the namespace.

=cut

.namespace [ "Perl6";"Grammar" ]
.sub "add_type" :method
    .param string name
    # XXX TODO
.end


=item C<is_type(name)>

Checks if the name we have been passed represents a type.

=cut

.sub 'is_type' :method
    .param string name
    # XXX TODO
    .return (1)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
