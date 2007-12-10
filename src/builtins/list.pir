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
    .local pmc list, item
    list = new 'List'
  args_loop:
    unless args goto args_end
    item = shift args
    # $I0 = isa item, 'Array'
    # if $I0 goto add_item
    $I0 = does item, 'array'
    unless $I0 goto add_item
    splice args, item, 0, 0
    goto args_loop
  add_item:
    push list, item
    goto args_loop
  args_end:
    .return (list)
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
