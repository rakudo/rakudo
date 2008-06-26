## $Id:$

=head1 TITLE

Code - Perl 6 Sub class

=head1 DESCRIPTION

This file sets up the Perl 6 C<Sub> class, the class for subroutines.

=cut

.namespace ['Sub']

.sub 'onload' :anon :load :init
    .local pmc p6meta, subproto
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    subproto = p6meta.'new_class'('P6Sub', 'parent'=>'Routine', 'name'=>'Sub')
    p6meta.'register'('Perl6Sub', 'parent'=>subproto, 'protoobject'=>subproto)
.end

=over 4

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
