.namespace

.sub __onload :init :load
    $P0 = get_hll_global ['Perl6Object'], 'make_class'
    $P0('Str', 'super'=>'Perl6Str')
.end


# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
