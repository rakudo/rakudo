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
    '!EXPORT'('keys,kv,values,reverse', $P0)
.end


=item Scalar

When we're going to be stored as an item, become a Hash and then return
ourself in a ObjectRef.

=cut

.sub 'Scalar' :method
    # Create a hash with our values.
    .local pmc hash, it
    hash = get_hll_global "Hash"
    hash = hash.'new'()
    it = iter self
  it_loop:
    unless it goto it_loop_end
    $P0 = shift it
    $P1 = self[$P0]
    hash[$P0] = $P1
    goto it_loop
  it_loop_end:

    # Wrap it up in an object ref and return it.
    .local pmc ref
    ref = new 'ObjectRef', hash
    .return (ref)
.end


.sub 'get_string' :method :vtable
    $S0 = ''
    .local pmc it
    it = iter self
  loop:
    unless it goto end
    $S1 = shift it
    $S2 = it[$S1]
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
    .local pmc result, it
    result = new 'Perl6Hash'
    it = iter self
  iter_loop:
    unless it goto iter_end
    $S0 = shift it
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
    .local pmc it
    .local pmc rv
    it = iter self
    rv   = new 'List'
  loop:
    unless it goto end
    $S1 = shift it
    push rv, $S1
    $P1 = it[$S1]
    push rv, $P1
    goto loop
  end:
    .return (rv)
.end

=item pairs (method)

Returns elements of hash as array of C<Pairs>

=cut

.sub 'pairs' :method :multi('Hash')
    .local pmc it
    .local pmc rv
    it = iter self
    rv   = 'list'()
    $P3 = get_hll_global 'Perl6Pair'
  loop:
    unless it goto end
    $P1 = shift it
    $P2 = it[$P1]
    $P4 = $P3.'new'('key' => $P1, 'value' => $P2)
    push rv, $P4
    goto loop
  end:
    .return (rv)
.end


=item fmt

 our Str multi Mapping::fmt ( Str $format, $separator = "\n" )

Returns the invocant mapping formatted by an implicit call to C<.fmt> on
every pair, joined by newlines or an explicitly given separator.

=cut

.sub 'fmt' :method :multi('Hash')
    .param pmc format
    .param string sep          :optional
    .param int has_sep         :opt_flag

    .local pmc pairs
    .local pmc res
    .local pmc iter
    .local pmc retv
    .local pmc elem
    .local pmc key
    .local pmc value
    .local pmc elemres

    if has_sep goto have_sep
    sep = "\n"
  have_sep:
    pairs = self.'pairs'()
    res = new 'List'
    iter = pairs.'iterator'()
  elem_loop:
    unless iter goto done

  invoke:
    elem = shift iter
    elemres = elem.'fmt'(format)
    push res, elemres
    goto elem_loop

  done:
    retv = 'join'(sep, res)
    .return(retv)
.end


.sub 'keys' :method :multi('Hash')
    .local pmc it
    .local pmc rv
    it = iter self
    rv   = new 'List'
  loop:
    unless it goto end
    $S1 = shift it
    push rv, $S1
    goto loop
  end:
    .return (rv)
.end

=item reverse

=cut

.sub 'reverse' :method :multi('Hash')
    .local pmc result, it
    result = new 'Perl6Hash'
    it = iter self
  iter_loop:
    unless it goto iter_end
    $S0 = shift it
    $S1 = self[$S0]
    result[$S1] = $S0
    goto iter_loop
  iter_end:
    .return (result)
.end


.sub 'values' :method :multi('Hash')
    .local pmc it
    .local pmc rv
    it = iter self
    rv   = new 'List'
  loop:
    unless it goto end
    $S1 = shift it
    $P1 = it[$S1]
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
