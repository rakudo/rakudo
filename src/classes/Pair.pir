## $Id$

=head1 NAME

src/classes/Pair.pir - methods for the Pair class

=head1 Methods

=over 4

=cut

.namespace ['Perl6Pair']

.sub 'onload' :anon :load :init
    .local pmc p6meta
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    p6meta.'new_class'('Perl6Pair', 'parent'=>'Any', 'attr'=>'$!key $!value', 'name'=>'Pair')
.end

=item key

Gets the key of the pair.

=cut

.sub 'key' :method
    $P0 = getattribute self, '$!key'
    .return ($P0)
.end


=item value

Gets the value of the pair.

=cut

.sub 'value' :method
    $P0 = getattribute self, '$!value'
    .return ($P0)
.end


=item get_string()  (vtable method)

Stringify the Pair.

=cut

.sub 'get_string' :method :vtable
    $S0 = self.'key'()
    concat $S0, "\t"
    $S1 = self.'value'()
    concat $S0, $S1
    .return ($S0)
.end


=item perl

Returns a Perl code representation of the pair.

=cut

.sub perl :method
    # Get key and value.
    $P0 = self.'key'()
    $P1 = self.'value'()

    # Get perl representation
    $S0 = $P0.'perl'()
    $S1 = $P1.'perl'()

    # build result
    .local string result
    result = concat '(', $S0
    result .= ' => '
    result .= $S1
    result .= ')'
    .return (result)
.end


.namespace []

.sub 'infix:=>'
    .param pmc key
    .param pmc value
    key = key.'item'()
    value = value.'item'()
    $P0 = get_hll_global 'Pair'
    .return $P0.'new'('key'=>key, 'value'=>value)
.end


=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
