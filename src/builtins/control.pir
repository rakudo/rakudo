## $Id$

=head1 NAME

src/builtins/control.pir - Perl 6 Control functions

=head1 Functions

=over 4

=cut


.namespace []
## TODO: get the next line to work
## .namespace [ 'Control::Basic' ]


=item return

Create a return exception.  (Only handles 1 return value for
the moment -- we'll do more complex handling a bit later.)

=cut

.include 'except_types.pasm'
.include 'except_severity.pasm'

.sub 'return'
    .param pmc value           :optional
    .param int has_value       :opt_flag

    if has_value goto have_value
    value = new 'Nil'
  have_value:
    $P0         = new 'Exception'
    $P0['type'] = .CONTROL_RETURN
    setattribute $P0, 'payload', value
    throw $P0
    .return (value)
.end


=item fail

=cut

.sub '!FAIL'
    .param string value        :optional
    .param int has_value       :opt_flag
    if has_value goto have_value
    value = 'Use of uninitialized value'
  have_value:
    $P0 = new 'Exception'
    $P0['message'] = value
    $P1 = new 'Failure'
    setattribute $P1, '$!exception', $P0
    .return ($P1)
.end

.sub 'fail'
    .param pmc value           :optional
    .param int has_value       :opt_flag
    .local pmc result
    if has_value goto have_value
    result = '!FAIL'()
    goto done
  have_value:
    result = '!FAIL'(value)
  done:
    'return'(result)
    .return(result)
.end

=item take

=cut

.sub 'take'
    .param pmc value

    $P0         = new 'Exception'
    $P0['type'] = .CONTROL_TAKE
    $P0['severity'] = .EXCEPT_NORMAL
    setattribute $P0, 'payload', value
    throw $P0
    .return (value)
.end

=item gather

=cut

.sub 'gather'
    .param pmc block
    .local pmc list
    .local pmc eh
    list = 'list'()
    eh = new 'ExceptionHandler'
    eh.'handle_types'(.CONTROL_TAKE)
    set_addr eh, handler
    push_eh eh
    block()
    pop_eh
    .return (list)
  handler:
    .local pmc exception, continuation
    .local string message
    .get_results(exception)
    message = exception['message']
    continuation = exception['resume']
    $P0 = exception['payload']
    list.'push'($P0)
    continuation()
.end

=item next

=cut

.sub 'last'
    .local pmc e
    e = new 'Exception'
    e['severity'] = .EXCEPT_NORMAL
    e['type'] = .CONTROL_LOOP_LAST
    throw e
.end

.sub 'next'
    .local pmc e
    e = new 'Exception'
    e['severity'] = .EXCEPT_NORMAL
    e['type'] = .CONTROL_LOOP_NEXT
    throw e
.end

.sub 'redo'
    .local pmc e
    e = new 'Exception'
    e['severity'] = .EXCEPT_NORMAL
    e['type'] = .CONTROL_LOOP_REDO
    throw e
.end

.sub 'continue'
    .local pmc e
    e = new 'Exception'
    e['severity'] = .EXCEPT_NORMAL
    e['type'] = .CONTROL_CONTINUE
    throw e
.end

.sub 'break'
    .param pmc arg :optional
    .param int has_arg :opt_flag
    .local pmc e
    e = new 'Exception'
    e['severity'] = .EXCEPT_NORMAL
    e['type'] = .CONTROL_BREAK
    unless has_arg, no_arg
    e['payload'] = arg
  no_arg:
    throw e
.end

=item term:...

=cut

.sub '...'
    .param pmc message        :optional
    .param int have_message   :opt_flag
    if have_message goto message_done
    message = new 'Str'
    message = "Attempt to execute stub code (...)"
  message_done:
    'fail'(message)
.end


=item die

=cut

.sub 'die' :multi('Exception')
    .param pmc ex
    set_global '$!', ex
    throw ex
    .return ()
.end

.sub 'die' :multi(_)
    .param pmc list            :slurpy
    .local string message
    .local pmc ex

    message = list.'join'('')
    if message > '' goto have_message
    message = "Died\n"
  have_message:
    ex = new 'Exception'
    ex = message
    set_global '$!', ex
    throw ex
    .return ()
.end


=item exit

 multi Control::Basic::exit ( Int $status = 0)

Stops all program execution, and returns C<$status> to the calling environment.

=cut

.sub 'exit'
    .param int status     :optional
    .param int has_status :opt_flag

    if has_status goto x
    status = 0
  x:
    exit status
.end


=item nothing

 multi Control::Basic::nothing ()

No operation. Literally does nothing.

=cut

.sub 'nothing'
.end


=item sleep

 our Num multi Control::Basic::sleep ( Num $for = Inf )

Attempt to sleep for up to C<$for> seconds. Implementations are obligated
to support subsecond resolutions if that is at all possible.

[Q: what about multithreading?  do we just sleep this thread?  need
to coordinate with entire async model.  -law]

=cut

.sub 'sleep'
    .param num a               :optional
    .param int has_a           :opt_flag
    if has_a goto have_a
    a = 2147483647                               # FIXME: RT #57294
  have_a:
    $N0 = time
    sleep a
    $N1 = time
    $N2 = $N1 - $N0
    .return ($N2)
.end


=item time

 our Time sub Control::Basic::time()

XXX Should be returning a (currently unspec'd, it seems) Time object that
numifies to a floating point value giving the number of seconds and
fractional seconds since 2000. At the moment, just handing back what the
Parrot time opcode does, since that doesn't give something with a consistent
epoch. Mails sent about both issues, will fix when answers come back.

=cut

.sub 'time'
    $N0 = time
    .return ($N0)
.end


=item eval

 multi Control::Basic::eval ( Str $code, Grammar :$lang = CALLER::<$?PARSER>)

Execute C<$code> as if it were code written in C<$lang>.  The default
is the language in effect at the exact location of the eval call.

Returns whatever C<$code> returns, or undef on error. Sets caller's C<$!>
on error.

=cut

.sub 'eval'
    .param pmc code
    .param pmc lang            :named('lang') :optional
    .param int have_lang       :opt_flag

    $P0 = get_hll_global 'Str'
    $I0 = $P0.'ACCEPTS'(code)
    if $I0 goto type_ok
    'die'("Parameter type check failed on call to 'eval'.")
  type_ok:

    # We want to make the lexicals known to the Perl 6 compiler. (One day
    # PCT maybe will provide a way to tell any language about these.)
    .local pmc blocks, block_info, interp, sub
    interp = new 'ParrotInterpreter'
    $P0 = get_hll_global ['PAST'], 'Block'
    block_info = $P0.'new'()
    sub = interp["sub"; 1]
  lex_loop:
    if null sub goto lex_loop_end
    $P0 = sub.'get_lexinfo'()
    if null $P0 goto symbols_loop_end
    $P0 = inspect $P0, 'symbols'
    $P0 = iter $P0
  symbols_loop:
    unless $P0 goto symbols_loop_end
    $S0 = shift $P0
    block_info.'symbol'($S0, 'scope'=>'lexical')
    goto symbols_loop
  symbols_loop_end:
    sub = sub.'get_outer'()
    goto lex_loop
  lex_loop_end:
    blocks = get_hll_global ['Perl6';'Grammar';'Actions'], '@?BLOCK'
    block_info['eval'] = 1
    blocks.'unshift'(block_info)

    .local pmc compiler, invokable
    .local pmc res, exception
    unless have_lang goto no_lang
    push_eh catch
    $S0 = lang
    $S0 = concat 'languages/', $S0
    $S0 = concat $S0, '/'
    $S1 = lang
    $S0 = concat $S0, $S1
    $S0 = concat $S0, '.pbc'
    load_bytecode $S0
    $S0 = lang
    compiler = compreg $S0
    goto got_lang
  no_lang:
    push_eh catch
    compiler = compreg 'Perl6'
  got_lang:
    invokable = compiler.'compile'(code)

    # Clear lexical info we added.
    blocks.'shift'()

    # Set lexical scope.
    $P0 = interp["sub"; 1]
    $P1 = invokable[0]
    $P1.'set_outer'($P0)

    # Invoke.
    res = invokable()
    exception = new 'Failure'
    goto done

  catch:
    .get_results (exception)

  done:
    pop_eh
    
    # Propagate exception to caller
    $P0 = getinterp
    $P0 = $P0['lexpad';1]
    $P0['$!'] = exception
    unless null res goto with_res
    res = new ['Nil']
  with_res:
    .return (res)
.end

=item warn

=cut

.sub 'warn'
    .param pmc list            :slurpy
    .local pmc ex
    .local string message

    message = list.'join'('')
    if message > '' goto have_message
    message = "Warning!  Something's wrong\n"
  have_message:
    ## count_eh is broken
    # $I0 = count_eh
    # eq $I0, 0, no_eh
    ex = new 'Exception'
    ex['severity'] = .EXCEPT_WARNING
    ex['message'] = message
    throw ex
    .return ()
  no_eh:
    .local pmc err
    err = get_hll_global "$ERR"
    err.'print'(message)
    .return ()
.end


=back

=head1 TODO: Functions

=over 4

=item evalfile

 multi Control::Basic::evalfile (Str $filename : Grammar :$lang = Perl6)

Behaves like, and replaces Perl 5 C<do EXPR>, with optional C<$lang>
support.


=item fail

B<TODO>: Research the exception handling system.

=item warn

B<TODO>: Throw a resumable exception when Rakudo supports top-level exception
handlers.  Note that the default exception handler should print the message of
this exception to standard error.


=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
