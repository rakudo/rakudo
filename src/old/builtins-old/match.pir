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

=item make($item)

Equivalent to C<$/."!make"($item)>.  This sets the ast value of 
the current match object.

=cut

.sub 'make'
    .param pmc value
    $P0 = getinterp
    $P1 = $P0['lexpad';1]
    $P2 = $P1['$/']
    $I0 = can $P2, '!make'
    unless $I0 goto err_make
    $P2.'!make'(value)
    .return ()
  err_make:
    'die'("make() cannot set result of non-Match object in $/")
    .return ()
.end


=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
