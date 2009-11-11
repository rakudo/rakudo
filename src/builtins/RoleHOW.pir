## $Id$

=head1 TITLE

RoleHOW - default metaclass for Perl 6 roles

=head1 DESCRIPTION

This class subclasses ClassHOW to provide role-specific behaviors.

We use a P6role as our backing store. However, we keep a list of parents
separately - we simply pass these on to the class as an "implementation
detail". We also don't want Parrot's immediate-composition semantics, so
we also have an attribute collecting roles to flatten and compose later
on.

=cut

.namespace ['RoleHOW']

.sub 'onload' :anon :init :load
    .local pmc p6meta
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    p6meta.'new_class'('RoleHOW', 'parent'=>'ClassHOW', 'attr'=>'$!parents $!roles')
.end


=item new()

Creates a new instance of the meta-class.

=cut

.sub 'new' :method
    .param pmc name :optional
    .local pmc how, p6role, nsarray, ns
    if null name goto anon_role

    # Named role - associate with Parrot namespace.
    $P0 = get_hll_global [ 'Perl6';'Grammar' ], 'parse_name'
    nsarray = $P0(name)
    ns = get_hll_namespace nsarray
    p6role = new ['P6role'], ns
    goto have_p6role

    # Anonymous class - just create a new Parrot class and we're done.
  anon_role:
    p6role = new ['P6role']

    # Stash in metaclass instance, init a couple of other fields,
    # and hand it back.
  have_p6role:
    how = new ['RoleHOW']
    setattribute how, 'parrotclass', p6role
    $P0 = new ['ResizablePMCArray']
    setattribute how, '$!parents', $P0
    $P0 = new ['ResizablePMCArray']
    setattribute how, '$!roles', $P0
    .return (how)
.end


=item add_parent

Stores the parent; we'll add it to a class at compose time.

=cut

.sub 'add_parent' :method
    .param pmc meta
    .param pmc parent
    $P0 = getattribute meta, '$!parents'
    push $P0, parent
.end


=item compose(meta)

Completes the creation of the metaclass and return the P6role.

=cut

.sub 'compose' :method
    .param pmc meta
    .local pmc p6role

    # Associate the metaclass with the p6role.
    p6role = getattribute meta, 'parrotclass'
    setprop p6role, 'how', meta
    .return (p6role)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
