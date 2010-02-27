## $Id$

=head1 TITLE

Grammar - Perl 6 Grammar class

=head1 DESCRIPTION

This file implements the Grammar class.

=cut

.sub '' :anon :init :load
    .local pmc p6meta
    p6meta = get_hll_global ['Mu'], '$!P6META'
    p6meta.'new_class'('Grammar', 'parent'=>'Any')

    # XXX pmichaud++ needs to fix this bunch. kplzthnxbai jnthn :-)
    #p6meta.'new_class'('Grammar', 'parent'=>'Match')

    #$P0 = get_root_namespace ['parrot';'PGE';'Grammar']
    #$P0 = get_class $P0
    #.const 'Sub' $P1 = 'Grammar.parse'
    #$P0.'add_method'('parse', $P1)
.end

=head2 Methods

=over

=item parse(string)

Parse a string according to the TOP rule in the grammar.

=cut

=item parse(topic)

Invokes the TOP rule in the grammar on the given topic.

=cut

.namespace ['Grammar']
.sub 'parse' :method :subid('Grammar.parse')
    .param pmc topic
    .param pmc options         :slurpy :named
    .local pmc TOP

    # If there's a TOP rule, invoke it.
    push_eh no_TOP
    TOP = find_method self, "TOP"
    pop_eh
    .local pmc match, p6meta
    p6meta = get_hll_global ['Mu'], '$!P6META'
    $P0 = p6meta.'get_parrotclass'(self)
    $P0 = inspect $P0, 'namespace'
    $P0 = $P0.'get_name'()
    $S0 = shift $P0
    $S0 = join '::', $P0
    match = TOP(topic, options :named :flat, 'grammar' => $S0)
    $P0 = getinterp
    $P1 = $P0['lexpad';1]
    $P1['$/'] = match
    .return(match)

  no_TOP:
    pop_eh
    'die'("The grammar has no TOP rule to invoke.")
.end


=item parsefile(filename)

Reads in the file in filename and then invokes the TOP rule in the
grammar on it.

=cut

.sub 'parsefile' :method
    .param string filename
    .param pmc options         :slurpy :named
    $S0 = 'slurp'(filename)
    .tailcall self.'parse'($S0, options :named :flat)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
