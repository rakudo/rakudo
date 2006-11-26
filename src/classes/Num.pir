
.namespace [ 'Float' ]

.sub 'isa' :method
    .param string x
    $I0 = iseq x, 'Num'
    .return ($I0)
.end
