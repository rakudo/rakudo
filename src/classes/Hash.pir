## $Id$

=head1 NAME

src/classes/Hash.pir - Perl 6 Hash class and related functions

=head2 Object Methods

=over 4

=cut

.namespace []

.sub 'onload' :anon :load :init
    .local pmc p6meta, hashproto
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    hashproto = p6meta.'new_class'('Perl6Hash', 'parent'=>'Mapping', 'name'=>'Hash')
.end


.sub 'hash'
    .param pmc args            :slurpy
    .param pmc hash            :slurpy :named
    args.'!flatten'()
    unless hash goto hash_done
    unshift args, hash
  hash_done:
    .return args.'hash'()
.end


.namespace ['Perl6Hash']

.sub 'infix:=' :method
    .param pmc source
    $P0 = source.'hash'()
    copy self, $P0
    .return (self)
.end

.sub 'delete' :method
    .param pmc keys :slurpy
    .local pmc result
    .local string key
    .local pmc tmp
    result = new 'List'
    keys.'!flatten'()

  keys_loop: 
    unless keys goto done
    key = shift keys
    tmp = self[key]
    push result, tmp
    delete self[key]
    goto keys_loop

  done:
    .return (result)
.end

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
