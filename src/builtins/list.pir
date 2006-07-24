## $Id$

=head1 NAME

src/builtins/list.pir - Perl 6 List class

=head1 Functions

=over 4

=cut

.namespace

=item C<list(...)>

Build a Perl6List from its arguments.

=cut

.sub 'list'
    .param pmc args            :slurpy
    .local pmc list
    list = new 'Perl6List'
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


.namespace [ 'Perl6List' ]

.sub '__get_string' :method
    $S0 = join ' ', self
    .return ($S0)
.end


=back

=cut

## vim: expandtab sw=4
