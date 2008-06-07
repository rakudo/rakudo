## $Id$

=head1 TITLE

Complex - Perl 6 complex numbers

=head1 SUBROUTINES

=over 4

=item infix:+

=cut

.namespace []
.sub 'infix:+' :multi('Complex', _)
    .param pmc a
    .param pmc b
    $P0 = new 'Complex'
    add $P0, a, b
    .return ($P0)
.end

.sub 'infix:+' :multi(_, 'Complex')
    .param pmc a
    .param pmc b
    $P0 = new 'Complex'
    add $P0, a, b
    .return ($P0)
.end

.sub 'infix:-' :multi('Complex', _)
    .param pmc a
    .param pmc b
    $P0 = new 'Complex'
    sub $P0, a, b
    .return ($P0)
.end

.sub 'infix:-' :multi(_, 'Complex')
    .param pmc a
    .param pmc b
    $P0 = new 'Complex'
    sub $P0, a, b
    .return ($P0)
.end

.sub 'infix:*' :multi('Complex', _)
    .param pmc a
    .param pmc b
    $P0 = new 'Complex'
    mul $P0, a, b
    .return ($P0)
.end

.sub 'infix:*' :multi(_, 'Complex')
    .param pmc a
    .param pmc b
    $P0 = new 'Complex'
    mul $P0, a, b
    .return ($P0)
.end

.sub 'infix:/' :multi('Complex', _)
    .param pmc a
    .param pmc b
    $P0 = new 'Complex'
    div $P0, a, b
    .return ($P0)
.end

.sub 'infix:/' :multi(_, 'Complex')
    .param pmc a
    .param pmc b
    $P0 = new 'Complex'
    div $P0, a, b
    .return ($P0)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
