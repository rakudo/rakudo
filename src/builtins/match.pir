## $Id$

=head1 NAME

src/builtins/match.pir - Perl6 builtins for smart matching

=head1 Functions

=over 4

=cut

.namespace []

.sub 'infix:~~' :multi()
    .param pmc topic
    .param pmc x
    .tailcall x.'ACCEPTS'(topic)
.end


.sub 'infix:!~~'
    .param pmc topic
    .param pmc x
    .tailcall x.'REJECTS'(topic)
.end


.sub 'make'
    .param pmc value
    $P0 = getinterp
    $P1 = $P0['lexpad';1]
    $P2 = $P1['$/']
    $P2.'result_object'(value)
.end


=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
