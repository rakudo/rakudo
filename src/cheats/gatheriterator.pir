=head1 TITLE

GatherIterator - Perl 6 gather iterator

=head1 DESCRIPTION

GatherIterator is used to handle gather/take.

=head2 Methods

=over 4

=cut

.namespace ['GatherIterator']
.sub 'onload' :anon :init :load
    .local pmc p6meta, proto
    p6meta = get_hll_global ['Mu'], '$!P6META'
    proto = p6meta.'new_class'('GatherIterator', 'parent'=>'Iterator', 'attr'=>'$!coro $!block')
.end

=item get()

Returns the next element of the list.

=cut

.include 'except_types.pasm'
.sub 'get' :method
    .local pmc coro, i
    coro = getattribute self, '$!coro'
    i = self.coro()
    .return (i)
.end

.sub '' :method :subid('!gather_coroutine')
    .local pmc block, eh, ex, i, res
    block = getattribute self, '$!block'
    eh = root_new ['parrot';'ExceptionHandler']
    eh.'handle_types'(.CONTROL_TAKE)
    set_addr eh, got
    push_eh eh
    block()
    pop_eh
  gather_done:
    $P0 = get_hll_global 'IterDone'
    .yield ($P0)
    goto gather_done

  got:
    .get_results (ex)
    i = ex['payload']
    res = ex['resume']
    .yield (i)
    res()
.end

.namespace []
.sub '!GATHER'
    .param pmc block
    .local pmc gi
    gi = new ['GatherIterator']
    setattribute gi, '$!block', block
    .const 'Sub' coro = '!gather_coroutine'
    $P0 = clone coro
    setattribute gi, '$!coro', $P0
    .return (gi)
.end


=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
