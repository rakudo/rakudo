## $Id$

=head1 NAME

src/builtins/named-unary.pir - Perl6 named unary builtins

=head1 Functions

=over 4

=cut

.namespace

.sub 'defined'
    .param pmc x
    $I0 = defined x
    .return ($I0)
.end


=back

=cut


## vim: expandtab sw=4
