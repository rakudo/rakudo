.namespace ['Bool']

.sub 'onload' :anon :init :load
    .local pmc protoobject
    $P0 = get_hll_global ['Perl6Object'], 'make_proto'
    protoobject = $P0('Boolean', 'Bool')

    $P0 = protoobject.'new'()
    $P0 = 0
    set_global 'False', $P0

    $P0 = protoobject.'new'()
    $P0 = 1
    set_global 'True', $P0
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
