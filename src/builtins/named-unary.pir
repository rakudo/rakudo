## $Id$

=head1 NAME

src/builtins/named-unary.pir - Perl6 named unary builtins

=head1 Functions

=over 4

=cut

.namespace

=item defined($x)

Returns a true value if $x is defined, and a false value otherwise.

=cut

.sub 'defined'
    .param pmc x
    $I0 = defined x
    .return ($I0)
.end


=item rand($x)

Returns a random floating point number greater than or equal to zero and less
than $x.

=cut

.sub 'prefix:rand'
    .param num limit
    
    $P0 = new 'Random'
    $N0 = $P0
    $N0 *= limit
    
    .return ($N0)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
