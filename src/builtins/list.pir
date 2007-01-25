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
    list = new 'List'
  args_loop:
    unless args goto end
    $P0 = shift args
    push list, $P0
    goto args_loop
  end:
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

## vim: expandtab sw=4
