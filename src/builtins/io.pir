## $Id$

=head1 NAME

src/builtins/io.pir - Perl6 builtins for I/O

=head1 Functions

=over 4

=cut

.namespace []

.sub 'print'
    .param pmc args            :slurpy
    .local pmc it
    args.'!flatten'()
    it = iter args
  iter_loop:
    unless it goto iter_end
    $P0 = shift it
    unless null $P0 goto iter_nonull
    $P0 = new 'Failure'
  iter_nonull:
    print $P0
    goto iter_loop
  iter_end:
    .return (1)
.end


.sub 'say'
    .param pmc list            :slurpy
    'print'(list :flat)
    print "\n"
    .return (1)
.end


=item printf

Parses a format string and prints formatted output according to it.

=cut

.sub 'printf'
    .param pmc args            :slurpy
    $S0 = 'sprintf'(args :flat)
    print $S0
    .return (1)
.end


.sub 'open'
    .param string filename
    .param int r :named('r') :optional
    .param int w :named('w') :optional
    .param int a :named('a') :optional

    # Work out a mode string. XXX Default to r?
    .local string mode
    if r goto is_read
    if w goto is_write
    if a goto is_append
is_read:
    mode = "r"
    goto done_mode
is_write:
    mode = "w"
    goto done_mode
is_append:
    mode = "wa"
    goto done_mode
done_mode:

    # Open file to get PIO file handle.
    $P0 = open filename, mode
    if $P0 goto opened_ok
    'die'("Unable to open file") # XXX better message
opened_ok:

    # Create IO object and set handle.
    .local pmc obj
    obj = get_hll_global 'IO'
    obj = obj.'new'()
    setattribute obj, "$!PIO", $P0
    .return(obj)
.end

.sub 'close'
    .param pmc obj
    obj.'close'()
.end

.sub 'slurp'
    .param string filename
    .local string contents

    $P0 = 'open'(filename, 'r')
    contents = $P0.'slurp'()
    'close'($P0)
    .return(contents)
.end


=item unlink LIST

Deletes a list of files.  Returns the number of files successfully
deleted.

    $cnt = unlink 'a', 'b', 'c';

Be warned that unlinking a directory can inflict damage on your filesystem.
Finally, using C<unlink> on directories is not supported on many operating
systems.  Use C<rmdir> instead.

It is an error to use bare C<unlink> without arguments.

=cut

.sub 'unlink'
    .param pmc to_delete :slurpy
    .local pmc it, os
    .local int success_count

    # Error with no arguments.
    $I0 = elements to_delete
    if $I0 goto ok
    'die'("Cannot call unlink without any arguments")
  ok:

    os = new 'OS'
    success_count = 0
    it = iter to_delete
  it_loop:
    unless it goto it_loop_end
    $S0 = shift it
    push_eh unlink_skip
    os.'rm'($S0)
    inc success_count
  unlink_skip:
    pop_eh
    goto it_loop
  it_loop_end:

  .return (success_count)
.end


=item prompt

Shows the supplied message and then waits for input from $*IN.

=cut

.sub 'prompt'
    .param string prompt
    'print'(prompt)
    $P0 = get_hll_global "$IN"
    .tailcall $P0.'readline'()
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
