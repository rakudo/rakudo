## $Id$

=head1 TITLE

Int - Perl 6 integers

=cut

.namespace [ 'Int' ]

.sub 'onload' :anon :init :load
    $P0 = subclass 'Integer', 'Int'
    $P1 = get_class 'Any'
    addparent $P0, $P1
    addattribute $P0, "vartype" # XXX should get Object's one
    $P1 = get_hll_global ['Perl6Object'], 'make_proto'
    $P1($P0, 'Int')
    $P1('Integer', 'Int')
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
