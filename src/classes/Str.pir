## $Id$

=head1 TITLE

Str - Perl 6 strings

=head1 DESCRIPTION

This file sets up the C<Perl6Str> PMC type (from F<src/pmc/perl6str.pmc>)
as the Perl 6 C<Str> class.

=cut

.namespace ['Perl6Str']

.sub 'onload' :anon :init :load
    $P0 = get_hll_global ['Perl6Object'], 'make_proto'
    $P0('Perl6Str', 'Str')
.end


.sub 'ACCEPTS' :method
    .param string topic
    .return 'infix:eq'(topic, self)
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
