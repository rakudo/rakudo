## $Id$

=head1 TITLE

P6metaclass - methods on P6metaclass

=head1 DESCRIPTION

=head2 Methods on P6metaclass

=over

=item add_composable

Composes a role into the class. Just maps Perl 6 method name to the
P6metaclass one.

=cut

.namespace ['P6metaclass']
.sub 'add_composable' :method
    .param pmc obj
    .param pmc composee
    .tailcall self.'compose_role'(obj, composee)
.end


=item compose()

We add this compose method so that we can augment in the
setting things that were initially defined as having
P6metaclass as their metaclass rather than ClassHOW.

=cut

.namespace ['P6metaclass']
.sub 'compose' :method
    .param pmc obj
    .return (obj)
.end


=item rebless

Used to rebless the current class into some subclass of itself.

=cut

.sub 'rebless' :method
    .param pmc target
    .param pmc new_class

    # Get and rebless into the underlying Parrot class.
    .local pmc new_how
    new_how = new_class.'HOW'()
    $P0 = getattribute new_how, 'parrotclass'
    rebless_subclass target, $P0

    # Also need to do initialization of the containers for any attributes.
    .local pmc example, attrs, it, cur_attr, tmp
    example = new_class.'CREATE'()
    attrs = getattribute new_how, '$!attributes'
    it = iter attrs
  it_loop:
    unless it goto it_loop_end
    cur_attr = shift it
    $S0 = cur_attr.'name'()
    tmp = getattribute example, $S0
    setattribute target, $S0, tmp
    goto it_loop
  it_loop_end:

    .return (target)
.end


=item CREATE()

We have this here since P6metaclass doesn't (yet) inherit from Mu of
pretend to at least.

=cut

.namespace ['P6metaclass']
.sub 'CREATE' :method
    .param pmc repr :optional
    $P0 = typeof self
    $P0 = new $P0
    .return ($P0)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
