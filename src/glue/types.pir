## $Id$

=head1 NAME

src/glue/types.pir - internals bits to do with types

=head1 SUBS

=over 4

.namespace []

=item !have_exact_same_type

Takes two types and returns true if they match exactly (not accounting for any
subtyping relations, etc).

=cut

.sub '!have_exact_same_type'
    .param pmc t1
    .param pmc t2

    # If they have equal address, obviously the same.
    .local pmc t1meta, t2meta
    t1meta = t1.'HOW'()
    t2meta = t2.'HOW'()
    eq_addr t1meta, t2meta, same

    # If they are junctions, compare inside them recursively.
    $I0 = isa t1, 'Junction'
    unless $I0 goto not_junc
    $I1 = isa t2, 'Junction'
    unless $I0 == $I1 goto not_junc
    .local pmc j1, j2
    .local int max, i
    j1 = t1.'eigenstates'()
    j2 = t1.'eigenstates'()
    max = elements j1
    i = 0
  junc_loop:
    if i >= max goto junc_loop_end
    $P0 = j1[i]
    $P1 = j2[i]
    $I0 = '!have_exact_same_type'($P0, $P1)
    unless $I0 goto not_same
    inc i
    goto junc_loop
  junc_loop_end:
  not_junc:

  not_same:
    .return(0)
  same:
    .return (1)
.end


.sub '&CREATE_HASH_LOW_LEVEL'
    .param pmc storage :optional
    unless null storage goto have_storage
    storage = root_new ['parrot';'Hash']
  have_storage:
    $P0 = get_hll_global 'Hash'
    $P1 = $P0.'CREATE'()
    $P0 = $P0.'bless'($P1, 'storage'=>storage)
    .return ($P0)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
