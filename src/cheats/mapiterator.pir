=head1 TITLE

MapIterator - Perl 6 map iterator

=head1 DESCRIPTION

MapIterator is used to apply a block to a list one element
at a time.  We do it in PIR for speed, because this is also
the basis of "for" loops, and it's easier to handle next/last/redo
exceptions here.  Perl 6 source would be something like:

    class MapIterator is (does?) Iterator {
        has $!iter;
        has &!block;

        method get() {
            my $parcel = $!iter.batch(&!block.count);
            $parcel ~~ EMPTY ?? EMPTY !! &!block(|$parcel);
        }
    }

=head2 Methods

=over 4

=cut

.namespace ['MapIterator']
.sub 'onload' :anon :init :load
    .local pmc p6meta, proto
    p6meta = get_hll_global ['Mu'], '$!P6META'
    proto = p6meta.'new_class'('MapIterator', 'parent'=>'Iterator', 'attr'=>'$!iter &!block')
.end

=item get()

Returns the next element of the list.

=cut

.sub 'get' :method
    .local pmc iter, block, parcel
    iter  = getattribute self, '$!iter'
    block = getattribute self, '&!block'

    $I0 = block.'count'()
    parcel = iter.'batch'($I0)
    $I0 = isa parcel, ['EMPTY']
    if $I0 goto done
    parcel = block(parcel :flat)
  done:
    .return (parcel)
.end
    
=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
