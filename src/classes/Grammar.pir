## $Id$

=head1 TITLE

Grammar - Perl 6 Grammar class

=head1 DESCRIPTION

This file implements the Grammar class.

=cut

.namespace [ 'Grammar' ]

.sub 'onload' :anon :init :load
    $P0 = subclass 'Any', 'Grammar'
    load_bytecode "PGE.pbc"
    $P1 = get_class [ 'PGE::Grammar' ]
    addparent $P0, $P1
    $P1 = get_hll_global ['Perl6Object'], 'make_proto'
    $P1($P0, 'Grammar')
.end


=item ACCEPTS(topic)

Invokes the TOP rule in the grammar on the given topic.

=cut

.sub 'ACCEPTS' :method
    .param pmc topic
    .local pmc TOP

    # If there's a TOP rule, invoke it.
    push_eh no_TOP
    TOP = find_method self, "TOP"
    pop_eh
    .local pmc match
    match = TOP(topic)
    $P0 = getinterp
    $P1 = $P0['lexpad';2]
    $P1['$/'] = match
    .return(match)

  no_TOP:
    'die'("The grammar has no TOP rule to invoke.")
.end


# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
