## $Id$

=head1 NAME

src/classes/Mapping.pir - Perl 6 hash class and related functions

=head1 Methods

=cut

.namespace ['Mapping']

.sub 'onload' :anon :load :init
    .local pmc p6meta, mappingproto
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    mappingproto = p6meta.'new_class'('Mapping', 'parent'=>'Hash Any')
    $P0 = get_hll_global 'Associative'
    p6meta.'add_role'($P0, 'to'=>mappingproto)
    p6meta.'register'('Hash', 'parent'=>mappingproto, 'protoobject'=>mappingproto)
    $P0 = get_hll_namespace ['Mapping']
    '!EXPORT'('keys,kv,values', $P0)
.end

=head2 Methods

=over

=item fmt

 our Str multi Mapping::fmt ( Str $format, $separator = "\n" )

Returns the invocant mapping formatted by an implicit call to C<.fmt> on
every pair, joined by newlines or an explicitly given separator.

=cut

.sub 'fmt' :method :multi('Hash')
    .param pmc format
    .param string sep  :optional
    .param int has_sep :opt_flag

    .local pmc it
    .local pmc rv

    if has_sep goto have_sep
    sep = "\n"

  have_sep:
    it = self.'iterator'()
    rv = new 'List'

  loop:
    .local pmc pairfmt
    .local pmc pair

    unless it goto end

    pair = shift it
    pairfmt = pair.'fmt'(format)

    push rv, pairfmt
    goto loop

  end:
    rv = 'join'(sep, rv)
    .return(rv)
.end


=item iterator()

=cut

.sub 'iterator' :method :multi('Hash')
    .local pmc it
    .local pmc rv

    it = iter self
    rv = new 'List'

  loop:
    .local string key
    .local pmc pair
    .local pmc val

    unless it goto end
      key = shift it
      val = it[key]

      pair = 'infix:=>'(key, val)
      push rv, pair
    goto loop

  end:
    .return (rv)
.end


=item keys()

Returns keys of hash as a List

=cut

.sub 'keys' :method :multi('Hash')
    .local pmc it
    .local pmc rv

    it = self.'iterator'()
    rv = new 'List'
  loop:
    .local string key
    .local pmc pair

    unless it goto end
    pair = shift it
    key = pair.'key'()

    push rv, key
    goto loop

  end:
    .return (rv)
.end


=item kv (method)

Returns elements of hash as array of C<Pair(key, value)>

=cut

.sub 'kv' :method :multi('Hash')
    .local pmc it
    .local pmc rv

    it = self.'iterator'()
    rv = new 'List'

  loop:
    .local string key
    .local pmc pair
    .local pmc val

    unless it goto end
    pair = shift it
    key = pair.'key'()
    val = pair.'value'()

    push rv, key
    push rv, val
    goto loop

  end:
    .return (rv)
.end


=item list()

Return invocant as a List of Pairs.

=cut

.sub 'list' :method
    .tailcall self.'iterator'()
.end


=item pairs (method)

Returns elements of hash as array of C<Pairs>

=cut

.sub 'pairs' :method :multi('Hash')
    .tailcall self.'iterator'()
.end


=item perl()

Return perl representation of the invocant.

=cut

.sub 'perl' :method
    .local string rv
    .local pmc it

    rv = '{'
    it = self.'iterator'()
    unless it goto done
  loop:
    $P1 = shift it
    $S1 = $P1.'perl'()
    rv .= $S1
    unless it goto done
    rv .= ', '
    goto loop
  done:
    rv .= '}'
    .return (rv)
.end


=item reverse

=cut

.namespace ['Mapping']
.sub 'reverse' :method
    .local pmc it
    .local pmc rv

    rv = new 'Perl6Hash'
    it = self.'iterator'()

  loop:
    .local string key
    .local pmc pair
    .local pmc val

    unless it goto end
    pair = shift it
    key = pair.'key'()
    val = pair.'value'()

    rv[val] = key
    goto loop

  end:
    .return (rv)
.end


=item values()

Returns values of hash as a List

=cut

.sub 'values' :method :multi('Hash')
    .local pmc it
    .local pmc rv

    it = self.'iterator'()
    rv = new 'List'

  loop:
    .local pmc pair
    .local pmc val

    unless it goto end
    pair = shift it
    val = pair.'value'()

    push rv, val
    goto loop

  end:
    .return (rv)
.end

=back


=head2 Coercion methods

=over

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

=item Str

Stringification of a Mapping

=cut

## FIXME: :vtable('get_string') is wrong here
.namespace ['Mapping']
.sub 'Str' :vtable('get_string') :method
    .local string rv
    .local pmc it

    it = self.'iterator'()
    rv = ''
  loop:
    .local string str

    unless it goto end
    str = shift it
    rv .= str
    rv .= "\n"
    goto loop

  end:
    .return (rv)
.end


=head2 Private methods

=over 4

=item !flatten()

Flatten the invocant, as in list context.

=cut

.sub '!flatten' :method
    .tailcall self.'iterator'()
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
