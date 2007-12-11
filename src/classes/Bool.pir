.namespace ['Bool']

.sub 'onload' :anon :init :load
    $P0 = get_hll_global ['Perl6Object'], 'make_class'
    $P0('Bool', 'super'=>'Boolean')

    $P0 = new 'Bool'
    $P0 = 0
    set_global 'False', $P0

    $P0 = new 'Bool'
    $P0 = 1
    set_global 'True', $P0
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
