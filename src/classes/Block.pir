## $Id:$

=head1 TITLE

Code - Perl 6 Block class

=head1 DESCRIPTION

This file sets up the Perl 6 C<Block> class, the class for executable objects
that have lexical scopes

=cut

.namespace ['Block']

.sub 'onload' :anon :load :init
    .local pmc p6meta, blockproto
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    blockproto = p6meta.'new_class'('Block', 'parent'=>'Code')
    p6meta.'register'('Perl6Block', 'parent'=>blockproto, 'protoobject'=>blockproto)
.end

=over 4

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
