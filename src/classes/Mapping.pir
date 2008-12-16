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

When we're going to be stored as an item, become a Hash and
return an ObjectRef to it.

=cut

.namespace ['Mapping']
.sub 'Scalar' :method
    $P0 = self.'Hash'()
    $P0 = new 'ObjectRef', $P0
    .return ($P0)
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


=item list()

Return invocant as a List of Pairs.

=cut

.sub 'list' :method
    .tailcall self.'pairs'()
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
    $P0 = get_hll_global 'list'
    rv  = $P0()
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


.sub '!flatten' :method
    .tailcall self.'pairs'()
.end


=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
