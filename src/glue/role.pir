## $Id$

=head1 NAME

src/glue/role.pir - internals bits to do with roles

=head1 SUBS

=over 4

.namespace []


=item !create_parametric_role

Helper method for creating parametric roles.

=cut

.sub '!create_parametric_role'
    .param string name

    # Parse the name.
    .local pmc nsarray
    $P0 = get_hll_global [ 'Perl6';'Grammar' ], 'parse_name'
    nsarray = $P0(name)

    # This is a little fun. We only want to create the Parrot role and suck
    # in the methods once per role definition. We do this and it is attached to
    # the namespace. Then we attach this "master role" to a new one we create
    # per invocation, so the methods can be newclosure'd and added into it in
    # the body.
    .local pmc ns, info, parrotrole
    ns = get_hll_namespace nsarray
    parrotrole = get_class ns
    unless null parrotrole goto have_role

    info = root_new ['parrot';'Hash']
    $P0 = nsarray[-1]
    info['name'] = $P0
    info['namespace'] = nsarray
    parrotrole = root_new ['parrot';'P6role'], info
  have_role:

    # Copy list of roles done by the original role into this specific one.
    .local pmc specific_role, tmp, it
    specific_role = root_new ['parrot';'P6role']
    setprop specific_role, '$!orig_role', parrotrole
    tmp = parrotrole.'roles'()
    it = iter tmp
  roles_loop:
    unless it goto roles_loop_end
    tmp = shift it
    specific_role.'add_role'(tmp)
    goto roles_loop
  roles_loop_end:

    # Now create a meta-object (RoleHOW) to package this all up in.
    .local pmc metarole
    metarole = new ['RoleHOW']
    setprop specific_role, 'metaclass', metarole
    setattribute metarole, 'parrotclass', specific_role
    setattribute metarole, 'protoobject', specific_role
    setattribute metarole, 'shortname', $P0
    $P1 = box name
    setattribute metarole, 'longname', $P1

    # Clone all of the methods, to make sure they capture type variables as
    # they currently stand.
    .local pmc orig_role, meths, meth_iter
    parrotrole = getattribute metarole, 'parrotclass'
    orig_role = getprop '$!orig_role', parrotrole
    meths = orig_role.'methods'()
    meth_iter = iter meths
  it_loop:
    unless meth_iter goto it_loop_end
    $S0 = shift meth_iter
    $P0 = meths[$S0]
    $P1 = clone $P0
    $P2 = getprop '$!signature', $P0
    setprop $P1, '$!signature', $P2
    $I0 = isa $P0, 'Code'
    unless $I0 goto ret_pir_skip_rs
    $P2 = getattribute $P0, ['Sub'], 'proxy'
    $P2 = getprop '$!real_self', $P2
    $P3 = getattribute $P1, ['Sub'], 'proxy'
    setprop $P3, '$!real_self', $P2
  ret_pir_skip_rs:
    addmethod parrotrole, $S0, $P1
    goto it_loop
  it_loop_end:
    .return (parrotrole)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
