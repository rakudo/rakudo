## $Id$

=head1 NAME

src/builtins/control.pir - Perl 6 Control functions

=head1 Functions

=over 4

=cut


.namespace
## TODO: get the next line to work
## .namespace [ 'Control::Basic' ]


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
    $P0['_message'] = message
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
    .param num a
    sleep a
.end

=item eval

 multi Control::Basic::eval ( Str $code, Grammar :$lang = CALLER::<$?PARSER>)

Execute C<$code> as if it were code written in C<$lang>.  The default
is the language in effect at the exact location of the eval call.

Returns whatever C<$code> returns, or undef on error.

=cut

.sub 'eval'
    .param pmc code
    .param pmc lang            :named('lang') :optional
    'die'("eval unimplemented")
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


=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
