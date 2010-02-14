=head1 TITLE

Submethod - Perl 6 Submethod class

=head1 DESCRIPTION

This file sets up the Perl 6 C<Submethod> class, the class for submethods.

=cut

.namespace ['Submethod']

.sub 'onload' :anon :load :init
    .local pmc p6meta
    p6meta = get_hll_global ['Mu'], '$!P6META'
    p6meta.'new_class'('Submethod', 'parent'=>'Routine')
.end

=over 4

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
