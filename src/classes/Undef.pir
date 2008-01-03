.namespace

.sub 'undef'
    $P0 = new 'Undef'
    .return ($P0)
.end

.namespace [ 'Undef' ]

.sub 'onload' :anon :init :load
    $P0 = get_hll_global ['Perl6Object'], 'make_proto'
    $P0('Undef', 'Undef')
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
