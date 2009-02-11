## $Id$

=head1 TITLE

Grammar - Perl 6 Grammar class

=head1 DESCRIPTION

This file implements the Grammar class.

=cut

.sub '' :anon :init :load
    load_bytecode "PGE.pbc"
    .local pmc p6meta
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    p6meta.'new_class'('Grammar', 'parent'=>'PGE::Grammar')
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
.sub 'parse' :method
    .param pmc topic
    .local pmc TOP

    # If there's a TOP rule, invoke it.
    push_eh no_TOP
    TOP = find_method self, "TOP"
    pop_eh
    .local pmc match
    $S0 = self.'WHAT'()
    match = TOP(topic, 'grammar' => $S0)
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
    $S0 = 'slurp'(filename)
    .tailcall self.'parse'($S0)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
