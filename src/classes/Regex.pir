## $Id$

=head1 TITLE

Regex - Perl 6 Regex class

=head1 DESCRIPTION

This file sets up the Perl 6 C<Regex> class, the class for regexes.

=cut

.namespace ['Regex']

.sub 'onload' :anon :load :init
    .local pmc p6meta
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    p6meta.'new_class'('Regex', 'parent'=>'Routine')
.end

=over 4

=item ACCEPTS

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
    match = '!FAIL'('no matches')
    goto store_match

    # Otherwise, just match on the topic.
  is_match:
    match = self.'!invoke'(topic)

  store_match:
    # Store match object in $/.
    push_eh not_regex
    $P0 = getinterp
    $P1 = $P0['lexpad';1]
    $P2 = root_new ['parrot';'Perl6Scalar'], match
    $P1['$/'] = $P2
  not_regex:
    .return (match)
.end


=item true()

Evaluate a Regex in boolean context -- i.e., perform a match
against $_.

=cut

.sub '' :method('true')
    $P0 = find_caller_lex '$_'
    $P3 = self($P0)
    .tailcall 'prefix:?'($P3)
.end

.sub '' :vtable('get_bool') :method
    $I0 = self.'true'()
    .return ($I0)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
