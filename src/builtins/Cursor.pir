## $Id$

=head1 TITLE

Cursor - Perl 6 cursor objects

=head2 Methods

=over 4

=cut

.namespace [ 'Cursor' ]

.sub 'onload' :anon :init :load
    .local pmc p6meta, cursorproto
    p6meta = get_hll_global ['Mu'], '$!P6META'
    cursorproto = p6meta.'new_class'('Cursor', 'parent'=>'parrot;Regex::Cursor Any')
.end


.sub 'new_match' :method
    $P0 = new ['Match']
    .return ($P0)
.end

.sub 'new_array' :method
    $P0 = new ['Array']
    .return ($P0)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
