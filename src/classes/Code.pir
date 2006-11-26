
.namespace [ 'Sub' ]

.sub 'isa' :method
    .param string x
    $I0 = iseq x, 'Code'
    .return ($I0)
.end
