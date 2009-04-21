## $Id$

=head1 NAME

src/builtins/misc.pir - miscellaneous builtins that need reclassification

=head1 BUILTINS

=over

=item prefix:=

=cut

.sub 'prefix:=' :multi(_)
    .param pmc what
    $P0 = iter what
    .return($P0)
.end


=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
