## $Id$

=head1 NAME

src/classes/Hash.pir - Perl 6 Hash class and related functions

=head2 Object Methods

=over 4

=cut

.sub 'onload' :anon :load :init
    .local pmc p6meta, hashproto
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    hashproto = p6meta.'new_class'('Perl6Hash', 'parent'=>'Mapping', 'name'=>'Hash')
.end


.namespace

.sub 'infix:=' :multi(Perl6Hash, _)
    .param pmc target
    .param pmc source
    $P0 = source.'hash'()
    copy target, $P0
    .return (target)
.end


.sub 'hash'
    .param pmc args            :slurpy
    .param pmc hash            :slurpy :named
    args.'!flatten'()
    push args, hash
    .return args.'hash'()
.end


.namespace ['Perl6Hash']

.sub 'hash' :method
    .return (self)
.end

=back

=cut


# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
