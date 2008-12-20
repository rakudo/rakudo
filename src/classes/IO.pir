## $Id$

=head1 TITLE

IO - Perl 6 IO class

=head1 DESCRIPTION

This file implements the IO file handle class.

=cut

.namespace ['IO']
.sub '' :anon :init :load
    .local pmc p6meta
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    p6meta.'new_class'('IO', 'parent'=>'Any', 'attr'=>'$!PIO')
    p6meta.'new_class'('IOIterator', 'parent'=>'Perl6Object', 'attr'=>'$!IO')

    $P0 = get_hll_namespace ['IO']
    '!EXPORT'('lines,readline', 'from'=>$P0)
.end

=head2 Methods

=over 4

=item close

Closes the file.

=cut

.namespace ['IO']
.sub 'close' :method
    .local pmc PIO
    PIO = getattribute self, "$!PIO"
    close PIO
    .return(1)
.end


=item eof

Tests if we have reached the end of the file.

=cut

.namespace ['IO']
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


=item lines

our List multi method lines (IO $handle:) is export;

Returns all the lines of a file as a (lazy) List regardless of context.
See also slurp.

=cut

.namespace ['IO']
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

.namespace ['IO']
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
    $P0 = get_hll_global 'IOIterator'
    $P0 = $P0.'new'('IO' => self)
    .return ($P0)
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


=item slurp

Slurp a file into a string.

=cut

.sub 'slurp' :method
    .local pmc PIO
    PIO = getattribute self, "$!PIO"
    $S0 = PIO.'readall'()
    .return($S0)
.end


=back

=head2 Functions

=over 4

=item C<prefix:=(IO $io)>

Gets the iterator for the IO object.

=cut

.namespace []
.sub 'prefix:=' :multi('IO')
    .param pmc io
    .tailcall io.'readline'()
.end

=back

=head1 IOIterator

The IOIterator class implements the I/O iterator.

=head2 Methods

=over 4

=item item()  (Vtable shift_pmc)

Read a single line and return it.

=cut

.namespace ['IOIterator']
.sub 'item' :method :vtable('shift_pmc')
    .local pmc pio, chomper
    $P0 = getattribute self, "$!IO"
    pio = getattribute $P0, "$!PIO"
    $P0 = pio.'readline'()
    chomper = get_hll_global 'chomp'
    .tailcall chomper($P0)
.end

=item list()

Read all of the lines and return them as a List.

=cut

.namespace ['IOIterator']
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


=back

=head2 Coercion methods

=item Scalar

Return the value inside this container in item context.

=cut

.namespace ['IOIterator']
.sub 'Scalar' :method
    .tailcall self.'item'()
.end


=back

=head2 Private methods

=over

=item !flatten

Return the remainder of the input in flattening context.

=cut

.namespace ['IOIterator']
.sub '!flatten' :method
    .tailcall self.'list'()
.end


=back

=head2 Vtable functions

=cut

.namespace ['IOIterator']
.sub '' :vtable('get_bool') :method
    .local pmc PIO
    $P0 = getattribute self, "$!IO"
    PIO = getattribute $P0, "$!PIO"
    if PIO goto more
    .return (0)
  more:
    .return (1)
.end

.sub '' :vtable('get_iter') :method
    .return (self)
.end

.sub '' :vtable('get_string') :method
    $S0 = self.'item'()
    .return ($S0)
.end


# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
