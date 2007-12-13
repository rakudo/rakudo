.namespace

.sub __onload :init :load
    $P0 = get_hll_global ['Perl6Object'], 'make_proto'
    $P0('Perl6Str', 'Str')
.end


# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
