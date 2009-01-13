## $Id$

=head1 NAME

src/classes/Role.pir - methods for the Role class

=head1 Description

This class represents a role in Perl 6. It is not substitutable for a Parrot
role, nor does it subclass it. Instead, it provides a way to get at a Parrot
level role through a multiple dispatch (or perhaps from a cache). You can see
it as a kind of "role factory", which manufactures roles of a particular
short name for a particular set of parameters.

=head1 Methods

=over 4

=cut

.namespace ['Perl6Role']

.sub 'onload' :anon :init :load
    .local pmc p6meta, roleproto
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    roleproto = p6meta.'new_class'('Perl6Role', 'parent'=>'Any', 'name'=>'Role', 'attr'=>'$!selector @!created')
.end


=item !add_variant

Adds a parameterized variant of the role.

=cut

.sub '!add_variant' :method
    .param pmc variant
    .local pmc selector
    selector = getattribute self, '$!selector'
    unless null selector goto have_selector
    selector = new 'Perl6MultiSub'
    setattribute self, '$!selector', selector
  have_selector:
    push selector, variant
.end


=item !select

Selects a variant of the role to do based upon the supplied parameters.

=cut

.sub '!select' :method
    .param pmc pos_args  :slurpy
    .param pmc name_args :slurpy :named
    
    # XXX We need to look through the parameters we have and keep track
    # of variants we did already initialize/parameterize with.
    .local pmc selector, result, created_list
    selector = getattribute self, '$!selector'
    result = selector(pos_args :flat, name_args :flat :named)
    created_list = getattribute self, '@!created'
    unless null created_list goto got_created_list
    created_list = new 'ResizablePMCArray'
    setattribute self, '@!created', created_list
  got_created_list:
    push created_list, result
    .return (result)
.end


=item ACCEPTS(topic)

Checks if the given topic does the role.

=cut

.sub 'ACCEPTS' :method
    .param pmc topic

    # Since we aren't re-blessing code objects yet, need to get and test their
    # proto-object instead.
    $I0 = topic.'isa'('Code')
    unless $I0 goto no_proto
    topic = topic.'WHAT'()
  no_proto:

    # Now go over the roles we've created and see if one of them is done.
    .local pmc created, it
    created = getattribute self, '@!created'
    if null created goto it_loop_end
    it = iter created
    $I0 = 0
  it_loop:
    unless it goto it_loop_end
    $P0 = shift it
    $I0 = does topic, $P0
    if $I0 == 0 goto it_loop
  it_loop_end:

    $P0 = 'prefix:?'($I0)
    .return ($P0)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
