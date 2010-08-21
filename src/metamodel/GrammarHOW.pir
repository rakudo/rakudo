## $Id$

=head1 TITLE

GrammarHOW - metaclass for grammars.

=head1 DESCRIPTION

This just subclasses the final compose method to make sure we inherit from
Grammar by default.

=cut

.namespace ['GrammarHOW']

.sub 'onload' :anon :init :load
    .local pmc p6meta
    p6meta = get_hll_global ['Mu'], '$!P6META'
    p6meta.'new_class'('GrammarHOW', 'parent'=>'ClassHOW')
.end


=head2 Methods on GrammarHOW

=over

=item compose(meta)

If none of the parents is a Grammar, add Grammar as a parent and then
delegates to ClassHOW.

=cut

.sub 'compose' :method
    .param pmc obj
    .local pmc parrotclass, grammarclass
    grammarclass = get_hll_global 'Grammar'
    parrotclass = getattribute self, 'parrotclass'
    $P0 = inspect parrotclass, 'parents'
    unless $P0 goto add_grammar_as_parent
    $P1 = iter $P0
loop_on_parents:
    unless $P1 goto add_grammar_as_parent
    $P2 = shift $P1
    $I0 = $P2.'isa'(grammarclass)
    if $I0 goto loop_on_parents_done
    goto loop_on_parents
  add_grammar_as_parent:  
    self.'add_parent'(obj, grammarclass)
  loop_on_parents_done:
    $P0 = get_hll_global 'ClassHOW'
    $P0 = find_method $P0, 'compose'
    .tailcall $P0(self, obj)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
