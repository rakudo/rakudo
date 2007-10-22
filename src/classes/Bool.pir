.namespace

.sub __onload :init :load
    $P0 = get_class 'Perl6Bool'
    set_global 'Bool', $P0
    $P0 = new 'Perl6Bool'
    $P0 = 0
    set_global 'Bool::False', $P0
    $P0 = new 'Perl6Bool'
    $P0 = 1
    set_global 'Bool::True', $P0
.end

.namespace [ 'Perl6Bool' ]

.sub 'isa' :method
    .param string x
    $I0 = iseq x, 'Bool'
    .return ($I0)
.end

.sub 'WHAT' :method
    $P0 = get_class 'Perl6Bool'
    .return ($P0)
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
