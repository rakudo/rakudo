## $Id$

=head1 NAME

src/classes/Hash.pir - Perl 6 Hash class and related functions

=cut

.namespace []
.sub 'onload' :anon :load :init
    .local pmc p6meta, hashproto
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    hashproto = p6meta.'new_class'('Perl6Hash', 'parent'=>'Mapping', 'name'=>'Hash')
    hashproto.'!MUTABLE'()
.end

=head2 Methods

=over 4

=cut

=item ACCEPTS()

=cut

.namespace ['Perl6Hash']
.sub 'ACCEPTS' :method :subid('hash_ACCEPTS')
    .param pmc topic
    .tailcall self.'contains'(topic)
.end
.sub '' :init :load
    .local pmc block, signature
    .const 'Sub' $P0 = "hash_ACCEPTS"
    block = $P0
    signature = new ["Signature"]
    setprop block, "$!signature", signature
    signature."!add_param"("$topic")
    $P0 = get_hll_global 'Hash'
    signature."!add_implicit_self"($P0)
.end


.namespace ['Perl6Hash']
.sub 'contains' :method :subid('hash_contains')
    .param pmc key
    $I0 = exists self[key]
    .return( $I0 )
.end
.sub '' :init :load
    .local pmc block, signature
    .const 'Sub' $P0 = "hash_contains"
    block = $P0
    signature = new ["Signature"]
    setprop block, "$!signature", signature
    signature."!add_param"("$key")
    $P0 = get_hll_global 'Hash'
    signature."!add_implicit_self"($P0)
.end

.namespace ['Perl6Hash']
.sub 'delete' :method :subid('hash_delete')
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
.sub '' :init :load
    .local pmc block, signature
    .const 'Sub' $P0 = "hash_delete"
    block = $P0
    signature = new ["Signature"]
    setprop block, "$!signature", signature
    signature."!add_param"("@keys", 1 :named('slurpy'))
    $P0 = get_hll_global 'Hash'
    signature."!add_implicit_self"($P0)
.end

.namespace ['Perl6Hash']
.sub 'exists' :method :subid('hash_exists')
    .param pmc key
    $I0 = exists self[key]
    .return( $I0 )
.end
.sub '' :init :load
    .local pmc block, signature
    .const 'Sub' $P0 = "hash_exists"
    block = $P0
    signature = new ["Signature"]
    setprop block, "$!signature", signature
    signature."!add_param"("$key")
    $P0 = get_hll_global 'Hash'
    signature."!add_implicit_self"($P0)
.end

.namespace ['Perl6Hash']
.sub 'hash' :method :subid('hash_hash')
    .return (self)
.end
.sub '' :init :load
    .local pmc block, signature
    .const 'Sub' $P0 = "hash_hash"
    block = $P0
    signature = new ["Signature"]
    setprop block, "$!signature", signature
    $P0 = get_hll_global 'Hash'
    signature."!add_implicit_self"($P0)
.end

.namespace ['Perl6Hash']
.sub 'Hash' :method :subid('hash_Hash')
    .return (self)
.end
.sub '' :init :load
    .local pmc block, signature
    .const 'Sub' $P0 = "hash_Hash"
    block = $P0
    signature = new ["Signature"]
    setprop block, "$!signature", signature
    $P0 = get_hll_global 'Hash'
    signature."!add_implicit_self"($P0)
.end

=back

=head2 Operators

=over

=item circumfix:<{ }>

Create a Hash (hashref).

=cut

.namespace []
.sub 'circumfix:{ }'
    .param pmc values :slurpy
    $P0 = values.'Hash'()
    $P0 = new 'Perl6Scalar', $P0
    .return ($P0)
.end

=back

=head2 Private methods

=over

=item !STORE

Store a value into a hash.

=cut

.namespace ['Perl6Hash']
.sub '!STORE' :method
    .param pmc source
    ## we create a new hash here instead of emptying self in case
    ## the source argument contains self or elements of self.
    .local pmc hash, it, type
    hash = new 'Perl6Hash'

    ## Need to preserve typing.
    type = self.'of'()
    if type == "Object" goto untyped
    $P0 = get_hll_global 'Associative'
    $P0 = $P0.'!select'(type)
    'infix:does'(hash, $P0)
  untyped:

    source = 'list'(source)
    it = iter source
  iter_loop:
    unless it goto iter_done
    .local pmc elem, key, value
    elem = shift it
    $I0 = does elem, 'hash'
    if $I0 goto iter_hash
    $I0 = isa elem, 'Perl6Pair'
    if $I0 goto iter_pair
    unless it goto err_odd_list
    key = elem
    value = shift it
    goto iter_kv
  iter_pair:
    key = elem.'key'()
    value = elem.'value'()
  iter_kv:
    $I0 = type.'ACCEPTS'(value)
    unless $I0 goto type_error
    value = '!CALLMETHOD'('Scalar', value)
    hash[key] = value
    goto iter_loop
  iter_hash:
    .local pmc hashiter
    hashiter = iter elem
  hashiter_loop:
    unless hashiter goto hashiter_done
    $S0 = shift hashiter
    value = elem[$S0]
    $I0 = type.'ACCEPTS'(value)
    unless $I0 goto type_error
    value = '!CALLMETHOD'('Scalar', value)
    value = clone value
    hash[$S0] = value
    goto hashiter_loop
  hashiter_done:
    goto iter_loop
  iter_done:
    copy self, hash

    # Since copy calls clone which is deep and loses properties, need to now
    # re-apply type.
    it = iter self
  prop_set_loop:
    unless it goto prop_set_loop_end
    $S0 = shift it
    value = self[$S0]
    setprop value, 'type', type
    goto prop_set_loop
  prop_set_loop_end:
    .return (self)

  err_odd_list:
    die "Odd number of elements found where hash expected"
  type_error:
    'die'("Type mismatch in assignment to Hash.")
.end


=back

=cut


# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
