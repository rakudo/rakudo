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
    $P0 = p6meta.'new_class'('IO', 'parent'=>'Any', 'attr'=>'$!PIO $!ins')
    $P0.'!MUTABLE'()
    p6meta.'new_class'('IOIterator', 'parent'=>'Perl6Object', 'attr'=>'$!IO')

    $P0 = get_hll_namespace ['IO']
    '!EXPORT'('get', 'from'=>$P0)
.end

=head2 Methods

=over 4

.namespace ['IO']

=item get

Reads a line from the file handle.

=cut

.sub 'get' :method
    $P0 = get_hll_global 'IOIterator'
    $P0 = $P0.'new'('IO' => self)
    .return ($P0)
.end


=back

=head2 Functions

=over 4

=item C<prefix:=(IO $io)>

Gets the iterator for the IO object.

=cut

.namespace []
.sub 'prefix:=' :multi('IO')
    'die'("prefix:<=> has been superseeded by $handle.lines and $handle.get")
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
    .local pmc pio, ins, chomper
    $P0 = getattribute self, "$!IO"
    pio = getattribute $P0, "$!PIO"
    ins = getattribute $P0, "$!ins"
    'prefix:++'(ins)
    setattribute $P0, "$!ins", ins
    pio = '!DEREF'(pio)
    $P0 = pio.'readline'()
    chomper = get_hll_global 'chomp'
    .tailcall chomper($P0)
.end

=item list()

Read all of the lines and return them as a List.

=cut

.namespace ['IOIterator']
.sub 'list' :method
    .local pmc pio, ins, res, chomper
    $P0 = getattribute self, "$!IO"
    pio = getattribute $P0, "$!PIO"
    pio = '!DEREF'(pio)
    res = new 'List'
    chomper = get_hll_global 'chomp'

  loop:
    ins = getattribute $P0, "$!ins"
    'prefix:++'(ins)
    setattribute $P0, "$!ins", ins
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

=over 4

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
    .local pmc pio
    $P0 = getattribute self, "$!IO"
    pio = getattribute $P0, "$!PIO"
    if pio goto more
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
