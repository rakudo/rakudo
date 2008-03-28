## $Id$

=head1 NAME

src/builtins/misc.pir - miscellaneous builtins that need reclassification

=cut

.sub 'prefix:='
    .param pmc what
    $P0 = iter what
    .return($P0)
.end

.sub 'WHAT'
    .param pmc x
    .return x.'WHAT'()
.end


# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
