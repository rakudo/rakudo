
.namespace [ 'Perl6Str' ]

.sub 'isa' :method
    .param string x
    $I0 = iseq x, 'Str'
    .return ($I0)
.end
