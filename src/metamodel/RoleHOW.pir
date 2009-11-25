## $Id$

=head1 TITLE

RoleHOW - default metaclass for Perl 6 roles

=head1 DESCRIPTION

This is the metaclass for roles.

We use a P6role as our backing store. However, we keep a list of parents
separately - we simply pass these on to the class as an "implementation
detail". We also don't want Parrot's immediate-composition semantics, so
we also have an attribute collecting roles to flatten and compose later
on.

=cut

.namespace ['RoleHOW']

.sub 'onload' :anon :init :load
    .local pmc p6meta, rolehowproto
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    rolehowproto = p6meta.'new_class'('RoleHOW', 'parent'=>'Object', 'attr'=>'parrotclass shortname longname protoobject $!parents $!composees $!requirements $!collisions')

    # Also want to get various methods from the ParrotBacked role, since we're
    # backed by a Parrot Class PMC and using it to store most things.
    .local pmc parrotbacked
    parrotbacked = get_class ['Perl6';'Metamodel';'ParrotBackend']
    p6meta.'compose_role'(rolehowproto, parrotbacked)
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
    setattribute how, '$!composees', $P0
    $P0 = new ['ResizablePMCArray']
    setattribute how, '$!requirements', $P0
    $P0 = new ['ResizablePMCArray']
    setattribute how, '$!collisions', $P0
    setprop p6role, 'metaclass', how
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


=item add_requirement

Adds the name of a required method to the requirements list for the role.

=cut

.sub 'add_requirement' :method
    .param pmc meta
    .param pmc requirement
    $P0 = getattribute meta, '$!requirements'
    push $P0, requirement
.end


=item add_collision

Adds the name of a colliding method that needs the class or a role to resolve
it to the collisions list for the role.

=cut

.sub 'add_collision' :method
    .param pmc meta
    .param pmc collision
    $P0 = getattribute meta, '$!collisions'
    push $P0, collision
.end


=item add_composable

Stores something that we will compose (e.g. a role) at class composition time.

=cut

.sub 'add_composable' :method
    .param pmc meta
    .param pmc composee
    $P0 = getattribute meta, '$!composees'
    push $P0, composee
.end


=item methods

Gets the list of methods that this role does.

=cut

.sub 'methods' :method
    .param pmc meta
    .local pmc result, it, p6role
    result = root_new ['parrot';'ResizablePMCArray']
    p6role = getattribute meta, 'parrotclass'
    $P0 = inspect p6role, 'methods'
    it = iter $P0
  it_loop:
    unless it goto it_loop_end
    $S0 = shift it
    $P1 = $P0[$S0]
    push result, $P1
    goto it_loop
  it_loop_end:
    .return (result)
.end


=item parents

Gets the parents list for this role (e.g. the parents we are passing along for
later being added to the class).

=cut

.sub 'parents' :method
    .param pmc meta
    $P0 = getattribute meta, '$!parents'
    .return ($P0)
.end


=item requirements

Accessor for list of method names a role requires.

=cut

.sub 'requirements' :method
    .param pmc meta
    $P0 = getattribute meta, '$!requirements'
    .return ($P0)
.end


=item collisions

Accessor for list of method names in conflict; the class must resolve them.

=cut

.sub 'collisions' :method
    .param pmc meta
    $P0 = getattribute meta, '$!collisions'
    .return ($P0)
.end


=item applier_for

For now, we can't use a class as a composable thing. In the future we can
instead extract a role from the class (or rather, hand back a composer that
knows how to do that).

=cut

.sub 'applier_for' :method
    .param pmc meta
    .param pmc for
    
    $I0 = isa for, 'ClassHOW'
    if $I0 goto class_applier
    $I0 = isa for, 'RoleHOW'
    if $I0 goto role_applier
    if $I0 goto instance_applier

  class_applier:
    $P0 = get_hll_global ['Perl6';'Metamodel'], 'RoleToClassApplier'
    .return ($P0)

  role_applier:
    $P0 = get_hll_global ['Perl6';'Metamodel'], 'RoleToRoleApplier'
    .return ($P0)

  instance_applier:
    die 'Applying a role to an instance is not yet supported.'
.end


=item compose(meta)

Completes the creation of the metaclass and return the P6role.

=cut

.sub 'compose' :method
    .param pmc meta

    # See if we have anything to compose. Also, make sure our composees
    # all want the same composer.
    .local pmc composees, chosen_applier, composee_it
    composees = getattribute meta, '$!composees'
    $I0 = elements composees
    if $I0 == 0 goto composition_done
    if $I0 == 1 goto one_composee
    composee_it = iter composees
  composee_it_loop:
    unless composee_it goto apply_composees
    $P0 = shift composee_it
    if null chosen_applier goto first_composee
    $P1 = $P0.'HOW'()
    $P1 = $P1.'applier_for'($P0, meta)
    $P2 = chosen_applier.'WHAT'()
    $P3 = $P1.'WHAT'()
    $I0 = '&infix:<===>'($P2, $P3)
    if $I0 goto composee_it_loop
    die 'Can not compose multiple composees that want different appliers'
  first_composee:
    $P1 = $P0.'HOW'()
    chosen_applier = $P1.'applier_for'($P0, meta)
    goto composee_it_loop
  one_composee:
    $P0 = composees[0]
    $P1 = $P0.'HOW'()
    chosen_applier = $P1.'applier_for'($P0, meta)
  apply_composees:
    chosen_applier.'apply'(meta, composees)
  composition_done:

    # Associate the metaclass with the p6role.
    .local pmc p6role
    p6role = getattribute meta, 'parrotclass'
    setprop p6role, 'how', meta
    setattribute meta, 'protoobject', p6role
    .return (p6role)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
