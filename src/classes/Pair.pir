## $Id$

=head1 NAME

src/classes/Pair.pir - methods for the Pair class

=head1 Methods

=over 4

=cut

.namespace ['Pair']

.sub 'onload' :anon :load :init
    $P0 = subclass 'Pair', 'Perl6Pair'
    $P1 = get_hll_global 'Any'
    $P1 = $P1.HOW()
    addparent $P0, $P1
    $P1 = get_hll_global ['Perl6Object'], 'make_proto'
    $P1($P0, 'Pair')
.end


=item clone (vtable method)

Pairs are immutable, so just return ourself.

=cut

.sub 'clone' :method :vtable
    .return (self)
.end


=item perl

Returns a Perl code representation of the pair.

=cut

.sub perl :method
    # Get key and value.
    $P0 = self.'key'()
    $P1 = self.'value'()

    # Get types.
    $S0 = $P0.'WHAT'()
    $S1 = $P1.'WHAT'()

    # If the key is not a string, can't use colonpair form.
    if $S0 != 'Str' goto fatarrow

    # If key is not same when escaped, can't use colonpair form either.
    $S2 = $P0
    $S3 = escape $S2
    if $S3 != $S2 goto fatarrow

    # If it's string, same when escaped and has Int value, can use simple form.
    if $S1 != 'Int' goto valnoint
    if $P1 == 0 goto falseval
    if $P1 == 1 goto trueval
    goto valnoint
falseval:
    $S5 = ":!"
    concat $S5, $S2
    .return ($S5)
trueval:
    $S5 = ":"
    concat $S5, $S2
    .return ($S5)
valnoint:
    if $S1 == 'Str' goto valliteral
    if $S1 == 'Int' goto valnum
    if $S1 == 'Num' goto valnum
    goto fatarrow

valliteral:
    $S1 = $P1
    $I0 = index $S1, '>'
    if $I0  != -1 goto esc_val_litteral
    $I0 = index $S1, '<'
    if $I0 != -1 goto esc_val_litteral
    $S6 = ":"
    $S7 = $P0
    concat $S6, $S7
    concat $S6, '<'
    $S7 = $P1
    concat $S6, $S7
    concat $S6, '>'
    .return ( $S6 )

esc_val_litteral:
    $S1 = escape $S1
    $S1 = concat '"', $S1
    $S1 = concat $S1, '"'
    $P1 = $S1   # fall-thru
valnum:
    $S6 = ":"
    $S7 = $P0
    concat $S6, $S7
    concat $S6, '('
    $S7 = $P1
    concat $S6, $S7
    concat $S6, ')'
    .return ( $S6 )

fatarrow:
    $S3 = $P0.perl()
    concat $S2, $S3
    concat $S2, " => "
    $S3 = $P1.perl()
    concat $S2, $S3
    .return($S2)
.end


=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
