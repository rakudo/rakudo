## $Id$

=head1 NAME

src/classes/Mapping.pir - Perl 6 hash class and related functions

=head1 Methods

=over 4

=cut

.namespace ['Mapping']

.sub 'onload' :anon :load :init
    .local pmc p6meta, mappingproto
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    mappingproto = p6meta.'new_class'('Mapping', 'parent'=>'Hash Any')
    p6meta.'register'('Hash', 'parent'=>mappingproto, 'protoobject'=>mappingproto)
    $P0 = get_hll_namespace ['Mapping']
    '!EXPORT'('keys kv values reverse', $P0)
.end


.sub 'get_string' :method :vtable
    $S0 = ''
    .local pmc iter
    iter = new 'Iterator', self
  loop:
    unless iter goto end
    $S1 = shift iter
    $S2 = iter[$S1]
    $S0 = concat $S0, $S1
    concat $S0, "\t"
    concat $S0, $S2
    concat $S0, "\n"
    goto loop
  end:
    .return ($S0)
.end


=item hash()

Return invocant as a Hash

=cut

.sub 'hash' :method
    .local pmc result, iter
    result = new 'Perl6Hash'
    iter = new 'Iterator', self
  iter_loop:
    unless iter goto iter_end
    $S0 = shift iter
    $P0 = self[$S0]
    result[$S0] = $P0
    goto iter_loop
  iter_end:
    .return (result)
.end


=item perl()

Return perl representation of the invocant.

=cut

.sub 'perl' :method
    .local string result
    .local pmc keys
    result = '{'
    keys = self.'keys'()
    unless keys goto iter_end
  iter_loop:
    .local pmc key, value
    key = shift keys
    value = self[key]
    $S0 = key.'perl'()
    result .= $S0
    result .= ' => '
    $S0 = value.'perl'()
    result .= $S0
    unless keys goto iter_end
    result .= ', '
    goto iter_loop
  iter_end:
    result .= '}'
    .return (result)
.end


=item kv (method)

Returns elements of hash as array of C<Pair(key, value)>

=cut

.sub 'kv' :method :multi('Hash')
    .local pmc iter
    .local pmc rv
    iter = new 'Iterator', self
    rv   = new 'List'
  loop:
    unless iter goto end
    $S1 = shift iter
    push rv, $S1
    $P1 = iter[$S1]
    push rv, $P1
    goto loop
  end:
    .return (rv)
.end

=item pairs (method)

Returns elements of hash as array of C<Pairs>

=cut

.sub 'pairs' :method :multi('Hash')
    .local pmc iter
    .local pmc rv
    iter = new 'Iterator', self
    rv   = 'list'()
    $P3 = get_hll_global 'Perl6Pair'
  loop:
    unless iter goto end
    $P1 = shift iter
    $P2 = iter[$P1]
    $P4 = $P3.'new'('key' => $P1, 'value' => $P2)
    push rv, $P4
    goto loop
  end:
    .return (rv)
.end


.sub 'keys' :method :multi('Hash')
    .local pmc iter
    .local pmc rv
    iter = new 'Iterator', self
    rv   = new 'List'
  loop:
    unless iter goto end
    $S1 = shift iter
    push rv, $S1
    goto loop
  end:
    .return (rv)
.end

=item reverse

=cut

.sub 'reverse' :method :multi('Hash')
    .local pmc result, iter
    result = new 'Perl6Hash'
    iter = new 'Iterator', self
  iter_loop:
    unless iter goto iter_end
    $S0 = shift iter
    $S1 = self[$S0]
    result[$S1] = $S0
    goto iter_loop
  iter_end:
    .return (result)
.end


.sub 'values' :method :multi('Hash')
    .local pmc iter
    .local pmc rv
    iter = new 'Iterator', self
    rv   = new 'List'
  loop:
    unless iter goto end
    $S1 = shift iter
    $P1 = iter[$S1]
    push rv, $P1
    goto loop
  end:
    .return (rv)
.end


=back

=head1 Functions

=over 4

=back

=head1 TODO: Functions

=over 4

=cut

.namespace []

=item delete

 our List  multi method Hash::delete ( *@keys )
 our Scalar multi method Hash::delete ( $key ) is default

Deletes the elements specified by C<$key> or C<$keys> from the invocant.
returns the value(s) that were associated to those keys.

=item exists

 our Bool multi method Hash::exists ( $key )

True if invocant has an element whose key matches C<$key>, false
otherwise.

=cut


=item values

 multi Int|List Hash::keys ( %hash : MatchTest *@keytests )
 multi Int|List Hash::kv ( %hash : MatchTest *@keytests )
 multi Int|(List of Pair) Hash::pairs  (%hash : MatchTest *@keytests )
 multi Int|List Hash::values ( %hash : MatchTest *@keytests )

Iterates the elements of C<%hash> in no apparent order, but the order
will be the same between successive calls to these functions, as long as
C<%hash> doesn't change.

If C<@keytests> are provided, only elements whose keys evaluate
C<$key ~~ any(@keytests)> as true are iterated.

What is returned at each element of the iteration varies with function.
C<keys> only returns the key; C<values> the value; C<kv> returns both as
a 2 element list in (key, value) order, C<pairs> a C<Pair(key, value)>.

Note that C<kv %hash> returns the same as C<zip(keys %hash; values %hash)>

In Scalar context, they all return the count of elements that would have
been iterated.

The lvalue form of C<keys> is not longer supported. Use the C<.buckets>
property instead.

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
