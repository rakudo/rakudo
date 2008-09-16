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
    codeproto = p6meta.'new_class'('Code', 'parent'=>'Any')
    p6meta.'register'('Sub', 'parent'=>codeproto, 'protoobject'=>codeproto)
    p6meta.'register'('Closure', 'parent'=>codeproto, 'protoobject'=>codeproto)
.end

=over 4

=item ACCEPTS(topic)

=cut

.sub 'ACCEPTS' :method
    .param pmc topic
    .local pmc match
    match = self(topic)
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
    match = topic.'match'(self)
    $P0 = getinterp
    $P1 = $P0['lexpad';1]
    $P1['$/'] = match
    .return 'prefix:!'(match)
.end

=item perl()

Return a response to .perl.

=cut

.sub 'perl' :method
    .return ('{ ... }')
.end

=item WHAT()

Gets the proto-object for this value.

=cut

.sub 'WHAT' :method
    $P0 = getprop '$!proto', self
    if null $P0 goto block
    .return ($P0)
  block:
    $P0 = get_hll_global 'Block'
    .return ($P0)
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

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
