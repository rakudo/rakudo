## $Id$

=head1 TITLE

RoleHOW - default metaclass for Perl 6 roles

=head1 DESCRIPTION

This class subclasses ClassHOW to provide role-specific behaviors.

=cut

.namespace ['RoleHOW']

.sub 'onload' :anon :init :load
    .local pmc p6meta
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    p6meta.'new_class'('RoleHOW', 'parent'=>'ClassHOW')
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
