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
    .param num topic
    .return 'infix:=='(topic, self)
.end


# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
