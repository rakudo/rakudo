## $Id$

=head1 NAME

src/glue/role.pir - internals bits to do with roles

=head1 SUBS

=over 4

.namespace []

=item !create_master_role

Creates a master-role object, containing all the various role variants.

=cut

.sub 'create_master_role'
    .param pmc shortname
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


.sub '&infix:<does>'
    .param pmc var
    .param pmc role
    .param pmc init_value      :optional
    .param int have_init_value :opt_flag

    # Get the class of the variable we're adding roles to.
    .local pmc p6meta, parrot_class
    parrot_class = class var

    # Derive a new class that does the role(s) specified.
    .local pmc derived
    derived = root_new ['parrot';'Class']
    addparent derived, parrot_class
    $I0 = isa role, ['Perl6Role']
    if $I0 goto one_role_select
    #$P0 = get_root_namespace ['parrot';'Role']
    #$P0 = get_class $P0
    $I0 = isa role, 'P6role'
    if $I0 goto one_role
    $I0 = isa role, ['List']
    if $I0 goto many_roles
  error:
    'die'("'does' expects a role or a list of roles")

  one_role_select:
    role = role.'!select'()
  one_role:
    addrole derived, role
    '!compose_role_attributes'(derived, role)
    goto added_roles

  many_roles:
    .local pmc role_it, cur_role
    role_it = iter role
  roles_loop:
    unless role_it goto roles_loop_end
    cur_role = shift role_it
    $I0 = isa cur_role, 'Role'
    if $I0 goto have_parrot_role
    $I0 = isa cur_role, 'Perl6Role'
    unless $I0 goto error
    cur_role = cur_role.'!select'()
  have_parrot_role:
    addrole derived, cur_role
    '!compose_role_attributes'(derived, cur_role)
    goto roles_loop
  roles_loop_end:
  added_roles:

    # Instantiate the class to make it form itself.
    $P0 = new derived

    # Create a new meta-class, but associate with existing proto-object.
    .local pmc p6meta, old_proto, new_proto
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    new_proto = p6meta.'register'(derived)
    $P0 = new_proto.'HOW'()
    old_proto = var.'WHAT'()
    setattribute $P0, 'protoobject', old_proto

    # Re-bless the object into the subclass.
    rebless_subclass var, derived

    # We need to set any initial attribute values up.
    .lex '$CLASS', new_proto
    $P0 = find_method new_proto, 'BUILD'
    $P0(var)

    # If we were given something to initialize with, do so.
    unless have_init_value goto no_init
    .local pmc attrs
    .local string attr_name
    attrs = inspect role, "attributes"
    attrs = attrs.'keys'()
    $I0 = elements attrs
    if $I0 != 1 goto attr_error
    attr_name = attrs[0]
    attr_name = substr attr_name, 2 # lop off sigil and twigil
    $P0 = var.attr_name()
    'infix:='($P0, init_value)
  no_init:

    # We're done - return.
    .return (var)

attr_error:
    'die'("Can only supply an initialization value to a role with one attribute")
.end


.sub '&infix:<but>'
    .param pmc var
    .param pmc role
    .param pmc value      :optional
    .param int have_value :opt_flag

    # First off, is the role actually a role?
    $I0 = isa role, 'Perl6Role'
    if $I0 goto have_role
    $I0 = isa role, 'Role'
    if $I0 goto have_role

    # If not, it may be an enum. If we don't have a value, get the class of
    # the thing passed as a role and find out.
    if have_value goto error
    .local pmc maybe_enum
    maybe_enum = role.'WHAT'()
    $P0 = getprop '$!is_enum', maybe_enum
    if null $P0 goto error
    unless $P0 goto error
    value = role
    role = maybe_enum
    goto have_role
    unless null role goto have_role

    # Did anything go wrong?
  error:
    'die'("The but operator can only be used with a role or enum value on the right hand side")

    # Now we have a role, copy the value and call does on the copy.
  have_role:
    $I0 = isa var, 'ObjectRef'
    unless $I0 goto not_obj_ref
    var = deref var
  not_obj_ref:
    var = clone var
    if null value goto no_value
    'infix:does'(var, role, value)
    goto return
  no_value:
    'infix:does'(var, role)
  return:
    .return (var)
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
    $I0 = '!SAMETYPE_EXACT'(class_attr_type, role_attr_type)
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
