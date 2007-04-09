
.namespace [ 'Float' ]

.sub 'isa' :method
    .param string x
    $I0 = iseq x, 'Num'
    .return ($I0)
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
