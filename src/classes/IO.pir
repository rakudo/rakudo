## $Id$

=head1 TITLE

IO - Perl 6 IO class

=head1 DESCRIPTION

This file implements the IO file handle class.

=head1 Methods

=over 4

=cut

.namespace ['IO']

.sub 'onload' :anon :init :load
    .local pmc p6meta
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    p6meta.'new_class'('IO', 'parent'=>'Any', 'attr'=>'$!PIO')
    p6meta.'new_class'('IOIterator', 'parent'=>'Perl6Object', 'attr'=>'$!IO')

    $P0 = get_hll_namespace ['IO']
    '!EXPORT'('lines', 'from'=>$P0)
.end


=item lines

our List multi method lines (IO $handle:) is export;

Returns all the lines of a file as a (lazy) List regardless of context. See also slurp.

=cut

.sub 'lines' :method :multi('IO')
    .local pmc PIO, res, chomper
    PIO = getattribute self, "$!PIO"
    res = new 'List'
    chomper = get_hll_global 'chomp'

  loop:
    $S0 = PIO.'readline'()
    unless $S0 goto done
    $S0 = chomper($S0)
    res.'push'($S0)
    goto loop

  done:
    .return (res)
.end


=item print

Writes the given list of items to the file.

=cut

.sub 'print' :method
    .param pmc args            :slurpy
    .local pmc it
    .local pmc PIO
    PIO = getattribute self, "$!PIO"
    args = 'list'(args)
    it = iter args
  iter_loop:
    unless it goto iter_end
    $S0 = shift it
    print PIO, $S0
    goto iter_loop
  iter_end:
    .return (1)
.end


=item say

Writes the given list of items to the file, then a newline character.

=cut

.sub 'say' :method
    .param pmc list            :slurpy
    .local pmc PIO
    PIO = getattribute self, "$!PIO"
    self.'print'(list)
    print PIO, "\n"
    .return (1)
.end


=item printf

Parses a format string and prints formatted output according to it.

=cut

.sub 'printf' :method
    .param pmc args            :slurpy
    .local pmc PIO
    PIO = getattribute self, "$!PIO"
    $S0 = 'sprintf'(args :flat)
    print PIO, $S0
    .return (1)
.end


=item readline

Reads a line from the file handle.

=cut

.sub 'readline' :method
    .local pmc PIO, chomper
    PIO = getattribute self, "$!PIO"
    $P0 = PIO.'readline'()
    chomper = get_hll_global 'chomp'
    .tailcall chomper($P0)
.end


=item slurp

Slurp a file into a string.

=cut

.sub 'slurp' :method
    .local pmc PIO
    PIO = getattribute self, "$!PIO"
    $S0 = PIO.'readall'()
    .return($S0)
.end


=item eof

Tests if we have reached the end of the file.

=cut

.sub 'eof' :method
    .local pmc PIO
    PIO = getattribute self, "$!PIO"
    if PIO goto not_eof
    $P0 = get_hll_global [ 'Bool' ], 'True'
    .return ($P0)
  not_eof:
    $P0 = get_hll_global [ 'Bool' ], 'False'
    .return ($P0)
.end


=item close

Closes the file.

=cut

.sub 'close' :method
    .local pmc PIO
    PIO = getattribute self, "$!PIO"
    close PIO
    .return(1)
.end


.namespace []

=back

=head1 EXPORTED MULTI SUBS

=over 4

=item C<prefix:=(IO $io)>

Gets the iterator for the IO object.

=cut

.sub 'prefix:=' :multi('IO')
    .param pmc io
    $P0 = get_hll_global 'IOIterator'
    $P0 = $P0.'new'('IO' => io)
    .return($P0)
.end


.namespace [ 'IOIterator' ]

=back

=head1 IOIterator

The IOIterator class implements the I/O iterator.

=over 4

=cut

.sub get_bool :method :vtable
    .local pmc PIO
    $P0 = getattribute self, "$!IO"
    PIO = getattribute $P0, "$!PIO"
    if PIO goto more
    .return(0)
more:
    .return(1)
.end


=item Scalar

Return the value inside this container in item context.

=cut

.sub 'Scalar' :method
    .tailcall self.'item'()
.end

.sub 'item' :method :vtable('shift_pmc')
    .local pmc pio, chomper
    $P0 = getattribute self, "$!IO"
    pio = getattribute $P0, "$!PIO"
    $P0 = pio.'readline'()
    chomper = get_hll_global 'chomp'
    .tailcall chomper($P0)
.end

.sub 'list' :method
    .local pmc pio, res, chomper
    $P0 = getattribute self, "$!IO"
    pio = getattribute $P0, "$!PIO"
    res = new 'List'
    chomper = get_hll_global 'chomp'

  loop:
    $S0 = pio.'readline'()
    if $S0 == '' goto done
    $S0 = chomper($S0)
    res.'push'($S0)
    goto loop

  done:
    .return (res)
.end

.sub 'get_string' :vtable
    .tailcall self.'item'()
.end

.sub 'get_iter' :method :vtable
    .return(self)
.end


=back

=cut


# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
