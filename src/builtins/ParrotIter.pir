=head1 TITLE

ParrotIter - Maps Perl 6 Iterator to Parrot v-table

=head1 DESCRIPTION

There are some semantic mis-matches between Perl 6's iterator
model and Parrot's. This exposes the Parrot v-table interface
in terms of a Perl 6 iterator.

=head2 Methods

=over 4

=cut

.namespace ['ParrotIter']
.sub 'onload' :anon :init :load
    .local pmc p6meta, proto
    p6meta = get_hll_global ['Mu'], '$!P6META'
    proto = p6meta.'new_class'('ParrotIter', 'parent'=>'Iterator', 'attr'=>'$!iterator $!lookahead')
.end


=item new

Creates a Parrot Iterator from an existing iterator and does the
initial lookahead.

=cut

.sub 'new' :method
    .param pmc iterator
    $P0 = new ['ParrotIter']
    setattribute $P0, '$!iterator', iterator
    $P1 = iterator.'get'()
    setattribute $P0, '$!lookahead', $P1
    .return ($P0)
.end


=item shift_pmc (vtable)

Returns the current item in the iteration, and updates the lookahead.

=cut

.sub '' :vtable('shift_pmc')
    $P0 = getattribute self, '$!lookahead'
    $I0 = isa $P0, ['EMPTY']
    if $I0 goto done
    $P1 = getattribute self, '$!iterator'
    $P1 = $P1.'get'()
    setattribute self, '$!lookahead', $P1
  done:
    .return ($P0)
.end


=item get_boolean (vtable)

Uses the lookahead to check if we've reached the end.

=cut

.sub '' :vtable('get_bool')
    $P0 = getattribute self, '$!lookahead'
    $I0 = isa $P0, ['EMPTY']
    $I0 = not $I0
    .return ($I0)
.end


=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
