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

=item continue

=cut

.sub 'continue'
    .local pmc e
    e = root_new ['parrot';'Exception']
    e['severity'] = .EXCEPT_NORMAL
    e['type'] = .CONTROL_CONTINUE
    throw e
.end

=item break

=cut

.sub 'break'
    .param pmc arg :optional
    .param int has_arg :opt_flag
    .local pmc e
    e = root_new ['parrot';'Exception']
    e['severity'] = .EXCEPT_NORMAL
    e['type'] = .CONTROL_BREAK
    unless has_arg, no_arg
    e['payload'] = arg
  no_arg:
    throw e
.end

=back

=cut
