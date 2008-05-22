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
    $P0 = getinterp
    $P1 = $P0['lexpad';1]
    $P1['$/'] = match
    .return (match)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
