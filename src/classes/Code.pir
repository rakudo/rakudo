
.namespace [ 'Sub' ]

.sub 'isa' :method
    .param string x
    $I0 = iseq x, 'Code'
    .return ($I0)
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
