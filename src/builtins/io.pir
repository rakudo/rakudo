## $Id$

=head1 NAME

src/builtins/io.pir - Perl6 builtins for I/O

=head1 Functions

=over 4

=cut

.namespace []

.sub 'print'
    .param pmc args            :slurpy
    .local pmc iter
    args.'!flatten'()
    iter = new 'Iterator', args
  iter_loop:
    unless iter goto iter_end
    $S0 = shift iter
    print $S0
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


.sub 'use'
    .param pmc module
    .param pmc args :slurpy

    .local string module_string
    module_string = module

    .local pmc path
    path     = split '::', module_string

    .local string file_string
    file_string = join '/', path

    .local pmc filename
    $P0 = get_hll_global 'Str'
    filename  = $P0.'new'()
    filename  = file_string
    filename .= '.pm'

    require(filename)

    .local pmc import
    import = find_global module_string, 'import'

    .local int have_import
    have_import = defined import
    unless have_import goto import_finished
    import(args :flat)

  import_finished:

.end

.sub 'require'
    .param pmc filename

    .local pmc p6compiler
    p6compiler = compreg 'Perl6'
    p6compiler.'evalfiles'(filename)
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
    mode = "<"
    goto done_mode
is_write:
    mode = ">"
    goto done_mode
is_append:
    mode = ">>"
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

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
