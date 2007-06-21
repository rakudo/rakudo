.namespace

.sub 'undef'
    $P0 = new 'Perl6Undef'
    .return ($P0)
.end

.namespace [ 'Perl6Undef' ]

.sub 'WHAT' :method
    $P0 = getclass 'Perl6Undef'
    .return ($P0)
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
