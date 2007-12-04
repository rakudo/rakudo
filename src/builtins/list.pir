## $Id$

=head1 NAME

src/builtins/list.pir - Perl 6 list operators

=head1 Functions

=over 4

=item C<list(...)>

Build a List from its arguments.

=cut

.namespace

.sub 'list'
    .param pmc args            :slurpy
    .local pmc list
    .return (args)
.end


=item C<infix:,(...)>

Operator form for building a list from its arguments.

=cut

.sub 'infix:,'
    .param pmc args            :slurpy
    .return 'list'(args :flat)
.end


## TODO: grep join map reduce reverse sort zip


=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
