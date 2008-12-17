## $Id$

=head1 NAME

src/classes/Role.pir - methods for the Role class

=head1 Methods

=over 4

=cut

.namespace ['Role']

.sub 'onload' :anon :init :load
    .local pmc p6meta, roleproto
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    roleproto = p6meta.'new_class'('Perl6Role', 'parent'=>'Role Any', 'name'=>'Role')
    p6meta.'register'('Role', 'parent'=>roleproto, 'protoobject'=>roleproto)
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

    $I0 = does topic, self
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
