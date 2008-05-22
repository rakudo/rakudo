.namespace

.namespace [ 'Failure' ]

.sub 'onload' :anon :init :load
    .local pmc p6meta, failureproto
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    failureproto = p6meta.'new_class'('Failure', 'parent'=>'Undef Any')
    p6meta.'register'('Undef', 'parent'=>'Any', 'protoobject'=>failureproto)
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
