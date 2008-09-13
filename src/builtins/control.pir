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
    value = 'list'()
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
    set_addr eh, handler
    push_eh eh
    block()
    pop_eh
    .return (list)
  handler:
    .local pmc exception
    .local string message
    .local pmc continuation
    .get_results(exception,message)
    continuation = exception['resume']
    $P0 = exception['payload']
    list.push($P0)
    eh = 0 # work around the workaround
    continuation()
.end


=item term:...

=cut

.sub '...'
    .param pmc message        :optional
    .param int have_message   :opt_flag
    if have_message goto message_done
    message = new 'Perl6Str'
    message = "Attempt to execute stub code (...)"
  message_done:
    'fail'(message)
.end


=item die

=cut

.sub 'die'
    .param pmc list            :slurpy
    .local pmc iter
    .local string message

    message = ''
    iter = new 'Iterator', list
  iter_loop:
    unless iter goto iter_end
    $P0 = shift iter
    $S0 = $P0
    message .= $S0
    goto iter_loop
  iter_end:
    if message > '' goto have_message
    message = "Died\n"
  have_message:
    $P0 = new 'Exception'
    $P0 = message
    set_global '$!', $P0
    throw $P0
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

    unless have_lang goto no_lang
    'die'('Lanuage parameter to eval unimplemented.')
  no_lang:

    .local pmc compiler, invokable
    .local pmc res, exception
    push_eh catch
    compiler = compreg 'Perl6'
    invokable = compiler.'compile'(code)

    res = invokable()
    pop_eh
    exception = new 'Failure'
    goto done

  catch:
    .get_results (exception, $S0)
    goto done

  done:
    # Propagate exception to caller
    $P0 = getinterp
    $P0 = $P0['lexpad';1]
    $P0['$!'] = exception
    .return (res)
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


=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
