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


=item !GATHER

=cut

.sub '!GATHER'
    .param pmc block
    .local pmc true, array, eh
    true = get_hll_global 'True'
    array = new ['Array']
    setprop array, "rw", true
    setprop array, "flatten", true
    eh = root_new ['parrot';'ExceptionHandler']
    eh.'handle_types'(.CONTROL_TAKE)
    set_addr eh, handler
    push_eh eh
    block()
    pop_eh
    .return (array)
  handler:
    .local pmc exception, continuation
    .local string message
    .get_results(exception)
    continuation = exception['resume']
    $P0 = exception['payload']
    array.'push'($P0)
    continuation()
.end


=back

=cut
