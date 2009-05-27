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
    .local pmc pio, ins
    pio = getattribute self, "$!PIO"
    ins = getattribute self, "$!ins"
    'prefix:++'(ins)
    setattribute self, "$!ins", ins
    pio = '!DEREF'(pio)
    $P0 = pio.'readline'()
    $P1 = get_hll_global 'chomp'
    .tailcall $P1($P0)
.end


=back

=head2 Functions

=over 4

=item C<prefix:=(IO $io)>

Gets the iterator for the IO object.

=cut

.namespace []
.sub 'prefix:=' :multi('IO')
    'die'("prefix:<=> has been superseded by $handle.lines and $handle.get")
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
