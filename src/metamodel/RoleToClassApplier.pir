## $Id$

=head1 TITLE

Perl6::Metamodel::RoleToClassApplier

=head1 DESCRIPTION

Applies a role to a class.

=cut

.namespace ['Perl6';'Metamodel';'RoleToClassApplier']

.sub 'onload' :anon :init :load
    .local pmc p6meta
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    p6meta.'new_class'('Perl6::Metamodel::RoleToClassApplier', 'parent'=>'Object')
.end


=item apply(target, composees)

Creates a new instance of the meta-class.

=cut

.sub 'apply' :method
    .param pmc target
    .param pmc composees

    # If we have more than one composee, need to resolve them down to a single role.
    # We just create a temporary role and punt the hard work off to the role to role
    # composer.
    .local pmc to_compose, rolehow, to_compose_meta, it
    rolehow = get_hll_global 'RoleHOW'
    $I0 = elements composees
    if $I0 == 1 goto one_composee
    to_compose_meta = rolehow.'new'()
    it = iter composees
  composee_it_loop:
    unless it goto composee_it_loop_end
    $P0 = shift it
    rolehow.'add_composable'(to_compose_meta, $P0)
    goto composee_it_loop
  composee_it_loop_end:
    to_compose = rolehow.'compose'(to_compose_meta)
    goto got_composee
  one_composee:
    to_compose = composees[0]
    to_compose_meta = to_compose.'HOW'()
  got_composee:

    # Check that all collisions are resolved.
    .local pmc collisions, requirements
    collisions = rolehow.'collisions'(to_compose_meta)
    it = iter collisions
  collision_loop:
    unless it goto collision_loop_end
    $S0 = shift it
    $I0 = target.'can'(target, $S0)
    if $I0 goto collision_loop
    '&die'("Method '", $S0, "' collides and a resolution must be provided by the class")
  collision_loop_end:
    requirements = rolehow.'requirements'(to_compose_meta)
    it = iter requirements
  requirement_loop:
    unless it goto requirement_loop_end
    $S0 = shift it
    $I0 = target.'can'(target, $S0)
    if $I0 goto requirement_loop
    '&die'("Method '", $S0, "' is required by a role and must be provided by the class, a parent class or by composing another role that implements it")
  requirement_loop_end:

    # Use the Parrot addrole to compose the methods.
    $P0 = getattribute target, 'parrotclass'
    addrole $P0, to_compose
.end
