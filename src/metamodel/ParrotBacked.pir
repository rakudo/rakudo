=head1 TITLE

Perl6::Metamodel::ParrotBacked - helps us with things backed by Parrot classes

=head1 DESCRIPTION

This is a low-level role (at the Parrot level) that helps us support having
metaclasses that use a Parrot-level class or role as backing for the metaclass.
It assumes it's stored in the attribute parrotclass. We use this for both
ClassHOW and RoleHOW, which back onto Parrot's Class and Role PMCs.

=cut

.namespace ['Perl6';'Metamodel';'ParrotBackend']

.sub 'init' :load :init :anon
    $P0 = get_hll_namespace ['Perl6';'Metamodel';'ParrotBackend']
    $P1 = root_new ['parrot';'Hash']
    $P1['namespace'] = $P0
    $P1 = new 'Role', $P1
.end


=item add_method(meta, name, code_ref)

Add a method to the given meta.

=cut

.sub 'add_method' :method
    .param pmc meta
    .param string name
    .param pmc meth
    $P0 = getattribute meta, 'parrotclass'
    addmethod $P0, name, meth
.end


=item add_attribute(meta, attribute)

Add an attribute.

=cut

.sub 'add_attribute' :method
    .param pmc meta
    .param pmc attribute

    # Add the attribute at the Parrot level.
    .local string name
    name = attribute.'name'()
    $P0 = getattribute meta, 'parrotclass'
    addattribute $P0, name

    # Add it to our attributes array.
    $P0 = getattribute meta, '$!attributes'
    push $P0, attribute
.end


=item attributes()

Gets the attributes that the class declares, returning a list of
Attribute descriptors.

=cut

.sub 'attributes' :method
    .param pmc obj
    .param pmc local :named('local') :optional
    .param pmc tree :named('tree') :optional

    # Transform false values to nulls.
    if null local goto local_setup
    if local goto local_setup
    local = null
  local_setup:
    if null tree goto tree_setup
    if tree goto tree_setup
    tree = null
  tree_setup:

    # Create result list and get Attribute proto.
    .local pmc result_list, attr_proto
    result_list = get_root_global [.RAKUDO_HLL], 'Array'
    result_list = result_list.'new'()
    attr_proto = get_root_global [.RAKUDO_HLL], 'Attribute'

    # Get list of parents whose attributes we are interested in, and put
    # this class on the start. With the local flag , that's just us.
    .local pmc parents, parents_it, cur_class, us
    unless null tree goto do_tree
    if null local goto all_parents
    parents = get_root_global [.RAKUDO_HLL], 'Array'
    parents = parents.'new'()
    goto parents_list_made
  all_parents:
    parents = self.'parents'(obj)
    goto parents_list_made
  do_tree:
    parents = self.'parents'(obj, 'local'=>1)
  parents_list_made:
    us = obj.'WHAT'()
    parents.'unshift'(us)
    parents_it = iter parents
  parents_it_loop:
    unless parents_it goto done
    cur_class = shift parents_it

    # If it's us, disregard :tree. Otherwise, if we have :tree, now we call
    # ourself recursively and push an array onto the result.
    if null tree goto tree_handled
    eq_addr cur_class, us, tree_handled
    $P0 = self.'attributes'(cur_class, 'tree'=>tree)
    $P0 = new 'Perl6Scalar', $P0
    result_list.'push'($P0)
    goto parents_it_loop
  tree_handled:

    # Get Parrot-level class.
    .local pmc parrot_class, attributes, attr_it, cur_attr_hash, cur_attr_info
    parrot_class = self.'get_parrotclass'(cur_class)

    # Iterate over attributes and build an Attribute descriptor for each one.
    attributes = parrot_class.'attributes'()
    attr_it = iter attributes
  attr_it_loop:
    unless attr_it goto attr_it_loop_end
    $S0 = shift attr_it
    cur_attr_hash = attributes[$S0]
    
    # Name
    $S0 = cur_attr_hash['name']
    
    # Type
    $P0 = cur_attr_hash['type']
    unless null $P0 goto type_done
    $P0 = get_root_global [.RAKUDO_HLL], 'Object'
  type_done:

    # Build
    $P1 = cur_attr_hash['init_value']
    unless null $P1 goto build_done
    $P1 = root_new [.RAKUDO_HLL; 'Failure']
  build_done:

    # Accessor
    $P2 = cur_attr_hash['accessor']
    unless null $P2 goto accessor_done
    $P2 = box 0
  accessor_done:
    $P3 = get_root_global [.RAKUDO_HLL], 'prefix:?'
    $P2 = $P3($P2)

    # rw
    $P4 = cur_attr_hash['rw']
    unless null $P4 goto rw_done
    $P4 = box 0
  rw_done:
    $P4 = $P3($P4)

    cur_attr_info = attr_proto.'new'('name' => $S0, 'type' => $P0, 'build' => $P1, 'accessor' => $P2, 'rw' => $P4)
    result_list.'push'(cur_attr_info)
    goto attr_it_loop
  attr_it_loop_end:
    goto parents_it_loop

  done:
    .return (result_list)
.end
