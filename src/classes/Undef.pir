.namespace

.namespace [ 'Undef' ]

.sub 'onload' :anon :init :load
    .local pmc p6meta
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    p6meta.'register'('Undef', 'parent'=>'Any', 'name'=>'Failure')
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
