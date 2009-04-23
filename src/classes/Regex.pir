## $Id$

=head1 TITLE

Regex - Perl 6 Regex class

=head1 DESCRIPTION

This file sets up the Perl 6 C<Regex> class, the class for regexes.

=cut

.namespace ['Regex']

.sub 'onload' :anon :load :init
    .local pmc p6meta
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    p6meta.'new_class'('Regex', 'parent'=>'Routine')
.end

=over 4

=item true()

Evaluate a Regex in boolean context -- i.e., perform a match
against $_.

=cut

.sub '' :method('true')
    $P0 = find_caller_lex '$_'
    $P3 = self($P0)
    .tailcall 'prefix:?'($P3)
.end

.sub '' :vtable('get_bool') :method
    $I0 = self.'true'()
    .return ($I0)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
