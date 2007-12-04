=head1 TITLE

Object - Perl 6 Object class

=cut

.sub '__onload' :init :load
    $P0 = newclass 'Perl6Object'
    $P1 = new $P0
    set_hll_global 'Object', $P1
.end


.namespace ['Perl6Object']

.sub 'new' :method
    $P0 = typeof self
    $P1 = new $P0
    .return ($P1)
.end


.sub 'WHAT' :method
    $S0 = typeof self
    .return ($S0)
.end


# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:

