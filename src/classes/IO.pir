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
.end

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
