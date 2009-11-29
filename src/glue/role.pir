## $Id$

=head1 NAME

src/glue/role.pir - internals bits to do with roles

=head1 SUBS

=over 4

.namespace []

=item !create_master_role

Creates a master-role object, containing all the various role variants.

=cut

.sub '!create_master_role'
    .param pmc shortname
    .param pmc existing
    $I0 = isa existing, 'Perl6Role'
    unless $I0 goto need_new
    .return (existing)
  need_new:
    $P0 = new ['Perl6Role']
    setattribute $P0, '$!shortname', shortname
    .return ($P0)
.end


=item !create_parametric_role

Helper method for creating parametric roles from PIR.

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




=item !compose_role_attributes(class, role)

Helper method to compose the attributes of a role into a class.

=cut

.sub '!compose_role_attributes'
    .param pmc class
    .param pmc role

    # Need to get hold of attribute order list for the class.
    .local pmc attr_order_list
    attr_order_list = getprop '@!attribute_list', class
    unless null attr_order_list goto have_attr_order_list
    attr_order_list = root_new ['parrot';'ResizableStringArray']
    setprop class, '@!attribute_list', attr_order_list
  have_attr_order_list:

    .local pmc role_attrs, class_attrs, ra_iter, fixup_list
    .local string cur_attr
    role_attrs = inspect role, "attributes"
    class_attrs = class."attributes"()
    fixup_list = root_new ['parrot';'ResizableStringArray']
    ra_iter = iter role_attrs
  ra_iter_loop:
    unless ra_iter goto ra_iter_loop_end
    cur_attr = shift ra_iter

    # Check that this attribute doesn't conflict with one already in the class.
    $I0 = exists class_attrs[cur_attr]
    unless $I0 goto no_conflict

    # We have a name conflict. Let's compare the types. If they match, then we
    # can merge the attributes.
    .local pmc class_attr_type, role_attr_type
    $P0 = class_attrs[cur_attr]
    if null $P0 goto conflict
    class_attr_type = $P0['type']
    if null class_attr_type goto conflict
    $P0 = role_attrs[cur_attr]
    if null $P0 goto conflict
    role_attr_type = $P0['type']
    if null role_attr_type goto conflict
    $I0 = '!have_exact_same_type'(class_attr_type, role_attr_type)
    if $I0 goto merge

  conflict:
    $S0 = "Conflict of attribute '"
    $S0 = concat cur_attr
    $S0 = concat "' in composition of role '"
    $S1 = role
    $S0 = concat $S1
    $S0 = concat "'"
    'die'($S0)

  no_conflict:
    addattribute class, cur_attr
    push fixup_list, cur_attr
    push attr_order_list, cur_attr
  merge:
    goto ra_iter_loop
  ra_iter_loop_end:

    # Now we need, for any merged in attributes, to copy property data.
    .local pmc fixup_iter, class_props, role_props, props_iter
    class_attrs = class."attributes"()
    fixup_iter = iter fixup_list
  fixup_iter_loop:
    unless fixup_iter goto fixup_iter_loop_end
    cur_attr = shift fixup_iter
    role_props = role_attrs[cur_attr]
    class_props = class_attrs[cur_attr]
    props_iter = iter role_props
  props_iter_loop:
    unless props_iter goto props_iter_loop_end
    $S0 = shift props_iter
    $P0 = role_props[$S0]
    class_props[$S0] = $P0
    goto props_iter_loop
  props_iter_loop_end:
    goto fixup_iter_loop
  fixup_iter_loop_end:
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
