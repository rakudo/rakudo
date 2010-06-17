=head1 TITLE

GatherIter - Perl 6 gather iterator

=head1 DESCRIPTION

GatherIter is used to handle gather/take.

=head2 Methods

=over 4

=cut

.namespace ['GatherIter']
.sub 'onload' :anon :init :load
    .local pmc p6meta, proto
    p6meta = get_hll_global ['Mu'], '$!P6META'
    proto = p6meta.'new_class'('GatherIter', 'parent'=>'Iterator', 'attr'=>'@!reify &!coro')
.end

=item reify()

Returns the next component of the iteration.

=cut


.namespace ['GatherIter']
.sub 'reify' :method
    .local pmc reify
    reify = getattribute self, '@!reify'
    unless null reify goto iter_reified
    .local pmc coro
    coro = getattribute self, '&!coro'
    reify = coro()
    setattribute self, '@!reify', reify
  iter_reified:
    .return (reify)
.end


.namespace []
.sub '!GATHER'
    .param pmc block
    .local pmc gatheriter, coro, list
    gatheriter = new ['GatherIter']
    .const 'Sub' coro_sub = '!gather_coroutine'
    .local pmc coro
    coro = clone coro_sub
    # Initial coroutine call to set the Coroutine's block and coro values
    coro(block, coro)
    setattribute gatheriter, '&!coro', coro
    list = gatheriter.'list'()
    .return (list)
.end


.include 'except_types.pasm'
.sub '' :subid('!gather_coroutine')
    .param pmc block
    .param pmc coro
    # Initial yield back to !GATHER
    .yield ()
    .local pmc handler
    handler = root_new ['parrot';'ExceptionHandler']
    handler.'handle_types'(.CONTROL_TAKE)
    set_addr handler, take
    push_eh handler
    block()
    pop_eh
  gather_done:
    .local pmc reify
    reify = root_new ['parrot';'ResizablePMCArray']
    .yield (reify)
    goto gather_done

  take:
    .local pmc exception, value, resume, nextiter
    .get_results (exception)
    value = exception['payload']
    resume = exception['resume']
    reify = root_new ['parrot';'ResizablePMCArray']
    push reify, value
    nextiter = new ['GatherIter']
    setattribute nextiter, '&!coro', coro
    push reify, nextiter
    .yield (reify)
    resume()
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
