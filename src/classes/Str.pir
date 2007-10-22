.namespace

.sub __onload :init :load
    $P0 = get_class 'Perl6Str'
    set_global 'Str', $P0
.end

.namespace [ 'Perl6Str' ]

.sub 'isa' :method
    .param string x
    $I0 = iseq x, 'Str'
    .return ($I0)
.end

.sub 'WHAT' :method
    $P0 = get_class 'Perl6Str'
    .return ($P0)
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
