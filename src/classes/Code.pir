## $Id$

=head1 TITLE

Code - Perl 6 Code class

=head1 DESCRIPTION

This file sets up the Perl 6 C<Code> class, the base class
for executable objects.

=cut

.namespace ['Code']

.sub 'onload' :anon :load :init
    .local pmc p6meta, codeproto
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    codeproto = p6meta.'new_class'('Code', 'parent'=>'Sub Any')
    $P0 = get_hll_global 'Callable'
    $P0 = $P0.'!select'()
    p6meta.'add_role'($P0, 'to'=>codeproto)
    codeproto.'!IMMUTABLE'()
    p6meta.'register'('Sub', 'parent'=>codeproto, 'protoobject'=>codeproto)
.end


=over 4

=item ACCEPTS(topic)

=cut

.sub 'ACCEPTS' :method
    .param pmc topic
    .local pmc match

    # If topic is an Array or Hash, need special treatment.
    $I0 = isa topic, 'Perl6Array'
    if $I0 goto is_array
    $I0 = isa topic, 'Perl6Hash'
    if $I0 goto is_hash
    goto is_match

    # Hash - just get keys and fall through to array case.
  is_hash:
    topic = topic.'keys'()

    # Array - try matching against each entry. In future, can probably
    # let junction dispatcher handle this for us.
  is_array:
    .local pmc it
    it = iter topic
  it_loop:
    unless it goto it_loop_end
    $P0 = shift it
    match = self.'!invoke'($P0)
    if match goto store_match
    goto it_loop
  it_loop_end:
    match = new 'Undef' # Otherwise we'd get a Null PMC Exception later
    goto store_match

    # Otherwise, just match on the topic.
  is_match:
    match = self.'!invoke'(topic)

  store_match:
    # Store match object in $/.
    push_eh not_regex
    $P0 = getinterp
    $P1 = $P0['lexpad';1]
    $P1['$/'] = match
  not_regex:
    .return (match)
.end

=item REJECTS(topic)

=cut

.sub 'REJECTS' :method
    .param pmc topic
    .local pmc match
    .local pmc pgesave
    match = self.'!invoke'(topic)
    $P0 = getinterp
    $P1 = $P0['lexpad';1]
    $P1['$/'] = match
    .tailcall 'prefix:!'(match)
.end

=item perl()

Return a response to .perl.

=cut

.sub 'perl' :method
    .return ('{ ... }')
.end

=item signature()

Gets the signature for the block, or returns Failure if it lacks one.

=cut

.sub 'signature' :method
    $P0 = getprop '$!signature', self
    if null $P0 goto no_sig
    .return ($P0)
  no_sig:
    $P0 = get_hll_global 'Failure'
    .return ($P0)
.end

=item assumming()

Returns a curried version of self.

=cut

.sub 'assuming' :method :subid('assuming')
    .param pmc args :slurpy
    .param pmc named_args :slurpy :named
    .local pmc curried
    .lex '@args', args
    .lex '%args', named_args
    .lex '$obj', self
    .const 'Sub' curried = 'assuming_helper'
    capture_lex curried
    .return (curried)
.end

.sub 'assuming_helper' :outer('assuming')
    .param pmc args :slurpy
    .param pmc named_args :slurpy :named
    .local pmc obj, assumed_args, assumed_named_args, result
    find_lex obj, '$obj'
    find_lex assumed_args, '@args'
    find_lex assumed_named_args, '%args'
    result = obj(assumed_args :flat, args :flat, assumed_named_args :flat :named, named_args :flat :named)
    .return (result)
.end

=item !invoke

Currently we don't have an easy way to distinguish Regex objects
from other types of Code objects, and so we have to resort to some
out-of-band mucking with PGE to get it to build Match objects.
That's the purpose of this method -- to set and restore the
type of match object that PGE regexes will create, without interfering
with the behavior of "normal" subs.

=cut

.sub '!invoke' :method
    .param pmc topic
    .local pmc pgesave, result
    pgesave = get_hll_global ['PGE'], '$!MATCH'
    $P0 = get_hll_global 'Match'
    set_hll_global ['PGE'], '$!MATCH', $P0
    result = self(topic)
    set_hll_global ['PGE'], '$!MATCH', pgesave
    .return (result)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
