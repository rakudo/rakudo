## $Id$

=head1 TITLE

Int - Perl 6 integers

=cut

.namespace [ 'Int' ]

.sub 'onload' :anon :init :load
    $P0 = subclass 'Integer', 'Int'
    $P1 = get_hll_global 'Any'
    $P1 = $P1.HOW()
    addparent $P0, $P1
    $P1 = get_hll_global ['Perl6Object'], 'make_proto'
    $P1($P0, 'Int')
    $P1('Integer', 'Int')
.end


.sub 'ACCEPTS' :method
    .param num topic
    .return 'infix:=='(topic, self)
.end


.sub 'clone' :method :vtable
    .local pmc clone_type
    clone_type = self.HOW()
    $P0 = clone_type.'new'()
    $P0 = self
    .return($P0)
.end


# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
