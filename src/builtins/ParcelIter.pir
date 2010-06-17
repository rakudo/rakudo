=head1 TITLE

ParcelIter - Perl 6 Parcel iterator

=head1 DESCRIPTION

This file implements ParcelIter, which is used for iterating
Parcels.  It's almost a trivial class, since the result of
.reify on a ParcelIter is simply to return the original Parcel
contents.

=head2 Methods

=over 4

=cut

.namespace ['ParcelIter']
.sub 'onload' :anon :init :load
    .local pmc p6meta, piproto
    p6meta = get_hll_global ['Mu'], '$!P6META'
    piproto = p6meta.'new_class'('ParcelIter', 'parent'=>'Iterator', 'attr'=>'$!parcel')
.end

.namespace ['ParcelIter']
.sub 'reify' :method
    $P0 = getattribute self, '$!parcel'
    .return ($P0)
.end


