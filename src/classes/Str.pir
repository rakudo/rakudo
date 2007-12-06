.namespace

.sub __onload :init :load
    $P0 = subclass 'Perl6Str', 'Str'
    $P1 = get_class ['Perl6Object']
    $P0.'add_parent'($P1)

    $P1 = new $P0
    set_hll_global 'Str', $P1
.end


# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
