## $Id$

=head1 NAME

src/builtins/junction.pir - Perl 6 junction operators

=head1 Functions

=over 4

=cut

.namespace

=item C<all(...)>

Builds an 'all' junction from its arguments.

=cut

.sub 'all'
    .param pmc args            :slurpy
    .local pmc junc
    junc = new 'Junction'

    junc."!values"(args)

    $P0 = new 'Integer'
    $P0 = JUNCTION_TYPE_ALL
    junc."!type"($P0)
    
    .return (junc)
.end


=item C<infix:&(...)>

Operator form for building an 'all' junction.

=cut

.sub 'infix:&'
    .param pmc args            :slurpy
    .return 'all'(args :flat)
.end


=item C<any(...)>

Builds an 'any' junction from its arguments.

=cut

.sub 'any'
    .param pmc args            :slurpy
    .local pmc junc
    junc = new 'Junction'

    junc."!values"(args)

    $P0 = new 'Integer'
    $P0 = JUNCTION_TYPE_ANY
    junc."!type"($P0)
    
    .return (junc)
.end


=item C<infix:|(...)>

Operator form for building an 'any' junction.

=cut

.sub 'infix:|'
    .param pmc args            :slurpy
    .return 'any'(args :flat)
.end


=item C<one(...)>

Builds a 'one' junction from its arguments.

=cut

.sub 'one'
    .param pmc args            :slurpy
    .local pmc junc
    junc = new 'Junction'

    junc."!values"(args)

    $P0 = new 'Integer'
    $P0 = JUNCTION_TYPE_ONE
    junc."!type"($P0)
    
    .return (junc)
.end


=item C<infix:^(...)>

Operator form for building a 'one' junction.

=cut

.sub 'infix:^'
    .param pmc args            :slurpy
    .return 'one'(args :flat)
.end


=item C<none(...)>

Builds a 'none' junction from its arguments.

=cut

.sub 'none'
    .param pmc args            :slurpy
    .local pmc junc
    junc = new 'Junction'

    junc."!values"(args)

    $P0 = new 'Integer'
    $P0 = JUNCTION_TYPE_NONE
    junc."!type"($P0)
    
    .return (junc)
.end


=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
