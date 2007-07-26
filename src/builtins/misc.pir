## $Id$

=head1 NAME

src/builtins/misc.pir - miscellaneous builtins that need reclassification

=cut

.sub 'WHAT'
    .param pmc x
    .return x.'WHAT'()
.end

