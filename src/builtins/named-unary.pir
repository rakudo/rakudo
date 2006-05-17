## $Id: builtins.pir 12709 2006-05-17 01:42:08Z pmichaud $

=head1 NAME

src/builtins/named-unary.pir - Perl6 named unary builtins

=head1 Functions

=over 4

=cut

.namespace [ "" ]


.sub 'defined'
    .param pmc x
    $I0 = defined x
    .return ($I0)
.end


.sub 'prefix:abs'
    .param pmc a
    $P0 = abs a
    .return ($P0)
.end


=back

=cut


## vim: expandtab sw=4
