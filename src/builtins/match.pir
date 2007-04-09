## $Id$

=head1 NAME

src/builtins/match.pir - Perl6 builtins for smart matching

=head1 Functions

=over 4

=cut

.namespace

.sub 'infix:~~' :multi(_, Sub)
    .param pmc topic
    .param pmc regex
    .local pmc match
    match = regex(topic, 'grammar'=>'Match')
    $P0 = getinterp
    $P1 = $P0['lexpad';1]
    $P1['$/'] = match
    .return (match)
.end

.sub 'infix:~~' :multi(_, Integer)
    .param pmc topic
    .param pmc x
    .return 'infix:=='(topic, x)
.end


.sub 'infix:~~' :multi(_, Float)
    .param pmc topic
    .param pmc x
    .return 'infix:=='(topic, x)
.end


.sub 'infix:~~' :multi(_, String)
    .param pmc topic
    .param pmc x
    .return 'infix:eq'(topic, x)
.end


.sub 'infix:~~' :multi(ResizablePMCArray, _)
    .param pmc array
    .param pmc x
    .local pmc iter
    iter = new .Iterator, array
    $P1 = new .Integer
  iter_loop:
    unless iter goto iter_end
    $P0 = shift iter
    $P1 = 'infix:~~'($P0, x)
    unless $P1 goto iter_loop
  iter_end:
    .return ($P1)
.end


.sub 'infix:!~'
    .param pmc topic
    .param pmc x
    $P0 = 'infix:~~'(topic, x)
    $I0 = isfalse $P0
    .return ($I0)
.end
    

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
