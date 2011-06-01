=head1 TITLE

MapIter - Perl 6 map iterator

=head1 DESCRIPTION

MapIter is used to repeatedly apply a block to a List.
We do it in PIR for speed, because this is also the basis of 
"for" loops, and it's easier to handle next/last/redo
exceptions here.  Perl 6 source would be something like:

=head2 Methods

=over 4

=cut

.namespace ['MapIter']
.sub 'onload' :anon :init :load
    .local pmc p6meta, proto
    p6meta = get_hll_global ['Mu'], '$!P6META'
    proto = p6meta.'new_class'('MapIter', 'parent'=>'Iterator', 'attr'=>'&!block @!list @!reify')
.end

.namespace ['MapIter']
.sub 'reify' :method
    .local pmc reify
    reify = getattribute self, '@!reify'
    unless null reify goto iter_reified
    reify = new ['Parcel']
    setattribute self, '@!reify', reify
    .local pmc block, list, args
    .local int count
    block = getattribute self, '&!block'
    list = getattribute self, '@!list'
    count = block.'count'()
    args = list.'munch'(count)
    unless args goto iter_reified
    .local pmc handler, value
    handler = root_new ['parrot';'ExceptionHandler']
    set_addr handler, catch 
    handler.'handle_types'(.CONTROL_LOOP_LAST, .CONTROL_LOOP_NEXT, .CONTROL_LOOP_REDO)
    push_eh handler
    value = block(args :flat)
    pop_eh
    push reify, value
    goto iter_next
  catch:
    .local pmc exception, type
    .get_results (exception)
    pop_eh
    value = getattribute exception, 'payload'
    push reify, value
    type = getattribute exception, 'type'
    if type == .CONTROL_LOOP_LAST goto iter_reified
    if type != .CONTROL_LOOP_REDO goto iter_next
    list.'unshift'(args)
  iter_next:
    .local pmc nextiter
    nextiter = new ['MapIter']
    setattribute nextiter, '&!block', block
    setattribute nextiter, '@!list', list
    push reify, nextiter
  iter_reified:
    .return (reify)
.end
    
=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
