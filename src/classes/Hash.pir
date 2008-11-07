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

=item ACCEPTS()

=cut

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

=item Scalar()

Returns an ObjectRef referencing itself, unless it already is one in which
case just returns as is.

=cut

.sub 'Scalar' :method
    $I0 = isa self, 'ObjectRef'
    unless $I0 goto not_ref
    .return (self)
  not_ref:
    $P0 = new 'ObjectRef', self
    .return ($P0)
.end


.sub 'ACCEPTS' :method
    .param pmc topic
    .return self.'contains'(topic)
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

.sub 'exists' :method
    .param pmc key

    $I0 = exists self[key]
    .return( $I0 )
.end

.sub 'contains' :method
    .param pmc key

    $I0 = exists self[key]
    .return( $I0 )
.end

=back

=cut


# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
