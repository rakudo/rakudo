## $Id$

=head1 TITLE

Code - Perl 6 Method class

=head1 DESCRIPTION

This file sets up the Perl 6 C<Method> class, the class for methods.

=cut

.namespace ['Method']

.sub 'onload' :anon :load :init
    .local pmc p6meta
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    p6meta.'new_class'('Method', 'parent'=>'Routine')
.end

=over 4

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
