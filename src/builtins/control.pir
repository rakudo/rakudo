=head1 NAME

src/builtins/control.pir - control flow related functions

=head1 Functions

=over 4

=cut

.include 'except_types.pasm'
.include 'except_severity.pasm'

=item die

=cut

.sub '&die'
    .param pmc list :slurpy
    .local string message
    .local pmc p6ex
    .local pmc ex

    message = join '', list
    if message > '' goto have_message
    message = "Died\n"
  have_message:
    p6ex = new ['Perl6Exception']
    ex = root_new ['parrot';'Exception']
    ex = message
    ex['severity'] = .EXCEPT_FATAL
    ex['type'] = .CONTROL_ERROR
    setattribute p6ex, '$!exception', ex
    set_global '$!', p6ex
    throw ex
    .return ()
.end

=item exit

=cut

.sub '&exit'
    .param int status     :optional
    .param int has_status :opt_flag

    if has_status goto x
    status = 0
  x:
    exit status
.end

.sub '&warn'
    .param pmc list :slurpy
    .local string message
    .local pmc p6ex
    .local pmc ex

    message = join '', list
    if message > '' goto have_message
    message = "Warning\n"
  have_message:
    p6ex = new ['Perl6Exception']
    ex = root_new ['parrot';'Exception']
    ex = message
    ex['severity'] = .EXCEPT_WARNING
    ex['type'] = .CONTROL_ERROR
    setattribute p6ex, '$!exception', ex
    set_global '$!', p6ex
    throw ex
    .return ()
.end

=item fail

=cut

# XXX Need to throw a Failure, not a Mu
# XXX Need to check for 'use fatal'
.sub '&fail'
    .param pmc value :optional
    .param int has_value :opt_flag
    .local pmc ex, p6ex, failure, fatal

    ex = root_new ['parrot';'Exception']
    unless has_value goto no_value
    ex['payload'] = value
    $S0 = value
    ex['message'] = value
  no_value:
    fatal = "!find_contextual"("$*FATAL")
    unless fatal goto no_fatal
    throw ex
  no_fatal:
    $P0 = get_hll_global 'Exception'
    p6ex = $P0.'new'(ex)
    $P0 = get_hll_global 'Failure'
    failure = $P0.'new'(p6ex)

    ex = root_new ['parrot';'Exception']
    ex['payload'] = failure
    ex['severity'] = .EXCEPT_ERROR
    ex['type'] = .CONTROL_RETURN
    throw ex
.end

=item proceed

=cut

.sub '&proceed'
    .local pmc ex, p6ex
    ex = root_new ['parrot';'Exception']
    ex['severity'] = .EXCEPT_NORMAL
    ex['type'] = .CONTROL_CONTINUE
    p6ex = new ['Perl6Exception']
    setattribute p6ex, '$!exception', ex
    set_global '$!', p6ex
    throw ex
.end

=item succeed

=cut

.sub '&succeed'
    .param pmc arg :optional
    .param int has_arg :opt_flag
    .local pmc e, p6ex
    e = root_new ['parrot';'Exception']
    e['severity'] = .EXCEPT_NORMAL
    e['type'] = .CONTROL_BREAK
    unless has_arg, no_arg
    e['payload'] = arg
  no_arg:
    p6ex = new ['Perl6Exception']
    setattribute p6ex, '$!exception', e
    set_global '$!', p6ex
    throw e
.end

=item next

=cut

.sub '&next'
    .local pmc e, p6ex
    e = root_new ['parrot';'Exception']
    e['severity'] = .EXCEPT_NORMAL
    e['type'] = .CONTROL_LOOP_NEXT
    p6ex = new ['Perl6Exception']
    setattribute p6ex, '$!exception', e
    set_global '$!', p6ex
    throw e
.end

=item redo

=cut

.sub '&redo'
    .local pmc e, p6ex
    e = root_new ['parrot';'Exception']
    e['severity'] = .EXCEPT_NORMAL
    e['type'] = .CONTROL_LOOP_REDO
    p6ex = new ['Perl6Exception']
    setattribute p6ex, '$!exception', e
    set_global '$!', p6ex
    throw e
.end

=item last

=cut

.sub '&last'
    .local pmc e, p6ex
    e = root_new ['parrot';'Exception']
    e['severity'] = .EXCEPT_NORMAL
    e['type'] = .CONTROL_LOOP_LAST
    p6ex = new ['Perl6Exception']
    setattribute p6ex, '$!exception', e
    set_global '$!', p6ex
    throw e
.end

=item take

=cut

.sub '&take'
    .param pmc value
    .local pmc ex, p6ex

    ex         = root_new ['parrot';'Exception']
    ex['type'] = .CONTROL_TAKE
    ex['severity'] = .EXCEPT_NORMAL
    ex['message'] = 'take without gather'
    setattribute ex, 'payload', value
    p6ex = new ['Perl6Exception']
    setattribute p6ex, '$!exception', ex
    set_global '$!', p6ex
    throw ex
    .return (value)
.end

.sub '&time'
    $N0 = time
    .return ($N0)
.end


=item callwith

=cut

.sub '&callwith'
    .param pmc pos_args    :slurpy
    .param pmc named_args  :slurpy :named

    # For callwith, it's easy - just want to get the next candidate, call
    # it and hand back it's return values. A tailcall does fine.
    .local pmc clist, lexpad, self, next
    get_next_candidate_info clist, $P0, lexpad
    next = clone clist
    next.'set_failure_mode'()
    $P0 = deref next
    $I0 = isa $P0, 'Method'
    unless $I0 goto not_method
    self = lexpad['self']
    .tailcall next(self, pos_args :flat, named_args :flat :named)
  not_method:
    .tailcall next(pos_args :flat, named_args :flat :named)
.end


=item nextwith

=cut

.sub '&nextwith'
    .param pmc pos_args    :slurpy
    .param pmc named_args  :slurpy :named

    # Find next candiate, invoke it and get its return value, then use
    # return to return it as if it was from our original call.
    .local pmc clist, lexpad, self, next, result
    get_next_candidate_info clist, $P0, lexpad
    next = clone clist
    next.'set_failure_mode'()
    $P0 = deref next
    $I0 = isa $P0, 'Method'
    unless $I0 goto not_method
    self = lexpad['self']
    (result) = next(self, pos_args :flat, named_args :flat :named)
    goto process_result
  not_method:
    (result) = next(pos_args :flat, named_args :flat :named)

  process_result:
    $I0 = isa result, ['Failure']
    unless $I0 goto did_defer
    $P0 = result.'exception'()
    if null $P0 goto did_defer
    $S0 = $P0.'Str'()
    if $S0 != 'No method to defer to' goto did_defer
    .return (result)

  did_defer:
    $P0 = root_new ['parrot';'Exception']
    $P0['type'] = .CONTROL_RETURN
    setattribute $P0, 'payload', result
    throw $P0
.end


=item callsame

=cut

.sub '&callsame'
    # Find next candidate as well as caller and lexpad.
    .local pmc clist, routine, lexpad, next
    get_next_candidate_info clist, routine, lexpad
    next = clone clist

    # Build arguments based upon what the caller was originall invoked with,
    # and tailcall the next candidate.
    .local pmc pos_args, named_args
    $P1 = lexpad['call_sig']
    (pos_args, named_args) = '!deconstruct_call_sig'($P1)
    next.'set_failure_mode'()
    .tailcall next(pos_args :flat, named_args :flat :named)
.end


=item nextsame

=cut

.sub '&nextsame'
    # Find next candidate as well as caller and lexpad.
    .local pmc clist, routine, lexpad, next
    get_next_candidate_info clist, routine, lexpad
    next = clone clist

    # Build arguments based upon what the caller was originall invoked with,
    # get the result of the next candidate and use return to retrun from
    # the caller, provided the defer did not fail.
    .local pmc pos_args, named_args, result
    $P1 = lexpad['call_sig']
    (pos_args, named_args) = '!deconstruct_call_sig'($P1)
    next.'set_failure_mode'()
    (result) = next(pos_args :flat, named_args :flat :named)

    $I0 = isa result, ['Failure']
    unless $I0 goto did_defer
    $P0 = result.'exception'()
    if null $P0 goto did_defer
    $S0 = $P0.'Str'()
    if $S0 != 'No method to defer to' goto did_defer
    .return (result)

  did_defer:
    $P0 = root_new ['parrot';'Exception']
    $P0['type'] = .CONTROL_RETURN
    setattribute $P0, 'payload', result
    throw $P0
.end


=item lastcall

Trims the candidate list so that nextsame/nextwith/callsame/callwith will
find nothing more to call.

=cut

.sub '&lastcall'
    # Find candidate list and trim it.
    .local pmc clist
    get_next_candidate_info clist, $P0, $P1
    clist.'trim_candidate_list'()
.end

=back

=cut
