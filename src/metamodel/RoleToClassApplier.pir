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
    $I0 = elements composees
    if $I0 == 1 goto one_composee
    rolehow = get_hll_global 'RoleHOW'
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
  got_composee:

    # Use the Parrot addrole to compose the methods.
    $P0 = getattribute target, 'parrotclass'
    addrole $P0, to_compose
.end
