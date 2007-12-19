## $Id$

=head1 TITLE

Int - Perl 6 integers

=cut

.namespace [ 'Integer' ]

.sub 'onload' :anon :init :load
    $P0 = get_hll_global ['Perl6Object'], 'make_proto'
    $P0('Integer', 'Int')
.end


.sub 'ACCEPTS' :method
    .param int topic
    $I0 = self
    $I0 = iseq $I0, topic
    .return ($I0)
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
