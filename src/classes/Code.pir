.sub 'onload' :anon :init :load
    $P0 = get_hll_global ['Perl6Object'], 'make_proto'
    $P0('Sub', 'Code')
    $P0('Closure', 'Code')
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
