## $Id$

=head1 TITLE

Code - Perl 6 Code class

=head1 DESCRIPTION

This file sets up the Perl 6 C<Code> class, the base class
for executable objects.

=cut


.sub 'onload' :anon :init :load
    $P0 = get_hll_global ['Perl6Object'], 'make_proto'
    $P0('Sub', 'Code')
    $P0('Closure', 'Code')
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
