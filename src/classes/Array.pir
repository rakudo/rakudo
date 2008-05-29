## $Id$

=head1 NAME

src/classes/Array.pir - Perl 6 Array class and related functions

=head2 Methods

=over 4

=cut

.sub 'onload' :anon :load :init
    .local pmc p6meta, arrayproto
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    arrayproto = p6meta.'new_class'('Perl6Array', 'parent'=>'List', 'name'=>'Array')
    p6meta.'new_class'('Arrayref', 'parent'=>arrayproto, 'protoobject'=>arrayproto)

.end

.namespace

.sub 'infix:=' :multi(Perl6Array, _)
    .param pmc target
    .param pmc source
    $P0 = source.'list'()
    $P0 = clone $P0
    $I0 = elements target
    splice target, $P0, 0, $I0
    .return (target)
.end


.sub '!Arrayref'
    .param pmc args            :slurpy
    .local pmc result, iter
    args = 'list'(args)
    result = new 'Arrayref'
    iter = args.'iterator'()
  iter_loop:
    unless iter goto iter_end
    $P0 = shift iter
    $P0 = clone $P0
    push result, $P0
    goto iter_loop
  iter_end:
    .return (result)
.end


.namespace ['Arrayref']

.sub 'item' :method
    .return (self)
.end


.sub 'list' :method
    $P0 = new 'List'
    push $P0, self
    .return ($P0)
.end

=back

=cut


# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
