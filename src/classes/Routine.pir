## $Id$

=head1 TITLE

Code - Perl 6 Routine class

=head1 DESCRIPTION

This file sets up the Perl 6 C<Routine> class, the base class for all
wrappable executable objects.

=cut

.include 'interpinfo.pasm'

.namespace ['Routine']

.sub 'onload' :anon :load :init
    .local pmc p6meta
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    p6meta.'new_class'('Routine', 'parent'=>'Block')
.end


=head1 METHODS

=over 4

=item wrap

=cut

.sub 'wrap' :method
    .param pmc wrapper

    # Did we already wrap?
    .local pmc cand_list, cur_sub
    cur_sub = getattribute self, ['Sub'], 'proxy'
    cand_list = getprop '@!candidates', cur_sub
    unless null cand_list goto have_cand_list

    # If not, need to create a new candidate list with the current sub,
    # and install the wrap helper that will start dispatching at the
    # start of the candidate list.
    .local pmc p6i
    cand_list = root_new ['parrot';'ResizablePMCArray']
    unshift cand_list, cur_sub
    p6i = root_new ['parrot';'P6Invocation'], cand_list
    .lex '__CANDIDATE_LIST__', p6i
    .const 'Sub' $P0 = '!wrap_start_helper'
    $P0 = newclosure $P0
    setattribute self, ['Sub'], 'proxy', $P0
    setprop $P0, '@!candidates', cand_list

    # We need to clone the wrapper, then tweak it to have an outer of
    # !wrap_clholder_helper, which we use to hold the candidate list,
    # and set the helper's outer to the block's original outer to maintain
    # the static chain. This is so we have a lexical slot for the
    # candidate list to go in; beats giving every single block one.
  have_cand_list:
    .local pmc orig_wrapper, tmp, tmp2
    orig_wrapper = wrapper
    wrapper = clone orig_wrapper
    .fixup_cloned_sub(orig_wrapper, wrapper)
    .const 'Sub' $P1 = '!wrap_clholder_helper'
    $P1 = clone $P1
    setprop $P1, '$!wrapper_block', wrapper
    $P2 = wrapper.'get_outer'()
    $P1.'set_outer'($P2)
    wrapper.'set_outer'($P1)

    # Unshift this candidate onto the list; generate a wrap handle also, stick
    # it on the candidate and return it.
    .local pmc handle
    $I0 = 1
    $P2 = cand_list[0]
    $P2 = getprop '$!handle', $P2
    if null $P2 goto no_handle
    $I0 = $P2
  no_handle:
    inc $I0
    handle = box $I0
    setprop $P1, '$!handle', handle
    unshift cand_list, $P1
    .return (handle)
.end
.sub '!wrap_start_helper' :anon :outer('wrap')
    .param pmc pos_args   :slurpy
    .param pmc named_args :slurpy :named
    $P0 = find_lex '__CANDIDATE_LIST__'
    $P1 = clone $P0
    .tailcall $P1(pos_args :flat, named_args :flat :named)
.end
.sub '!wrap_clholder_helper' :anon
    .param pmc pos_args   :slurpy
    .param pmc named_args :slurpy :named
say "!wrap_clholder_helper"
    # Slot for candidate list.
    .lex '__CANDIDATE_LIST__', $P0

    # Set up return handler, so next[with|same] work.
    $P2 = root_new ['parrot';'ExceptionHandler']
    set_addr $P2, ret_handler
    $P2."handle_types"(58)
    push_eh $P2

    # Get the inner block and call it.
    $P1 = interpinfo .INTERPINFO_CURRENT_SUB
    $P1 = getprop '$!wrapper_block', $P1
    capture_lex $P1
    ($P3) = $P1(pos_args :flat, named_args :flat :named)
    .return ($P3)

  ret_handler:
    .local pmc exception, result
    .get_results (exception)
    result = getattribute exception, "payload"
    .return (result)
.end


=item unwrap

=cut

.sub 'unwrap' :method
    .param pmc handle

    # Check it's wrapped.
    .local pmc cand_list, cur_sub
    cur_sub = getattribute self, ['Sub'], 'proxy'
    cand_list = getprop '@!candidates', cur_sub
    if null cand_list goto error

    # Look by handle for what to remove and remove it.
    $I0 = elements cand_list
    $I1 = 0
  find_loop:
    if $I1 >= $I0 goto error
    $P0 = cand_list[$I1]
    $P0 = getprop '$!handle', $P0
    if null $P0 goto error
    if handle == $P0 goto remove
    inc $I1
    goto find_loop
  remove:
    delete cand_list[$I1]

    # If it's not the last wrapper we're done, otherwise  we'll remove the
    # wrapper completely and restore the sub.
    $I0 = elements cand_list
    if $I0 == 1 goto final
    .return (handle)

  final:
    $P0 = shift cand_list
    setattribute self, ['Sub'], 'proxy', $P0
    .return (handle)

  error:
    'die'('Could not unwrap; unrecognized wrap handle')
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
