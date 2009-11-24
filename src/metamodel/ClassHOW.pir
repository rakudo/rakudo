## $Id$

=head1 TITLE

ClassHOW - metaclass for Perl 6 classes

=head1 DESCRIPTION

This class subclasses P6metaclass to give Perl 6 specific meta-class
behaviors. We do a bit of multiple inheritance to inherit from both
Object and P6metaclass, so ClassHOW is within the Perl6 object
hierarchy but so we can also deal with having a Parrot Class as our
backing store.

=cut

.namespace ['ClassHOW']

.sub 'onload' :anon :init :load
    .local pmc p6meta, classhowproto
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    
    # We need to specially construct our subclass of p6metaclass. We also
    # make it subclass Object.
    $P0 = newclass 'ClassHOW'
    $P1 = get_class 'P6metaclass'
    addparent $P0, $P1
    $P1 = get_hll_global 'Object'
    $P1 = p6meta.'get_parrotclass'($P1)
    addparent $P0, $P1

    # Extra attributes for interface consistency and compositiony stuff.
    addattribute $P0, '$!hides'
    addattribute $P0, '$!hidden'
    addattribute $P0, '$!composees'

    # Create proto-object for it.
    classhowproto = p6meta.'register'($P0)

    # Also want to get various methods from the ParrotBacked role, since we're
    # backed by a Parrot Class PMC and using it to store most things.
    .local pmc parrotbacked
    parrotbacked = get_class ['Perl6';'Metamodel';'ParrotBackend']
    p6meta.'compose_role'(classhowproto, parrotbacked)

    # Transform Object's metaclass to be of the right type.
    $P0 = new ['ClassHOW']
    $P1 = getattribute p6meta, 'parrotclass'
    setattribute $P0, 'parrotclass', $P1
    $P1 = getattribute p6meta, 'protoobject'
    setattribute $P0, 'protoobject', $P1
    $P1 = getattribute p6meta, 'longname'
    setattribute $P0, 'longname', $P1
    $P1 = getattribute p6meta, 'shortname'
    setattribute $P0, 'shortname', $P1
    set_hll_global ['Perl6Object'], '$!P6META', $P0
    $P1 = getattribute p6meta, 'parrotclass'
    setprop $P1, 'metaclass', $P0
    $P1 = get_hll_global 'Object'
    $P1 = typeof $P1
    setprop $P1, 'metaclass', $P0
.end

=head2 Methods on ClassHOW

=over

=item new()

Creates a new instance of the meta-class.

=cut

.sub 'new' :method
    .param pmc name :optional
    .local pmc how, parrotclass, nsarray, ns
    if null name goto anon_class

    # Named class - associate with Parrot namespace.
    $P0 = get_hll_global [ 'Perl6';'Grammar' ], 'parse_name'
    nsarray = $P0(name)
    ns = get_hll_namespace nsarray
    parrotclass = newclass ns
    goto have_parrotclass

    # Anonymous class - just create a new Parrot class and we're done.
  anon_class:
    parrotclass = new ['Class']

    # Stash in metaclass instance, and hand it back.
  have_parrotclass:
    how = new ['ClassHOW']
    setattribute how, 'parrotclass', parrotclass
    $P0 = root_new ['parrot';'ResizablePMCArray']
    setattribute how, '$!composees', $P0
    .return (how)
.end


=item add_composable

Stores something that we will compose (e.g. a role) at class composition time.

=cut

.sub 'add_composable' :method
    .param pmc meta
    .param pmc composee

    # XXX Picking a role variant should be done in the trait_mod, not here,
    # but zen slices aren't parsed yet.
    composee = composee.'postcircumfix:<[ ]>'()

    $P0 = getattribute meta, '$!composees'
    push $P0, composee
.end


=item applier_for

For now, we can't use a class as a composable thing. In the future we can
instead extract a role from the class (or rather, hand back a composer that
knows how to do that).

=cut

.sub 'applier_for' :method
    .param pmc meta
    .param pmc for
    die "A class can not be composed into another package yet"
.end


=item compose(meta)

Completes the creation of the metaclass and return a proto-object.

=cut

.sub 'compose' :method
    .param pmc meta
    .local pmc parrotclass
    parrotclass = getattribute meta, 'parrotclass'

    # Haz we already a proto-object? If so, we're done, so just hand
    # it back.
    $P0 = getattribute meta, 'protoobject'
    if null $P0 goto no_its_new
    .return ($P0)
  no_its_new:

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

    # If we have no parents explicitly given, inherit from Any.
    $P0 = inspect parrotclass, 'parents'
    if $P0 goto have_parents
    $P0 = get_hll_global 'Any'
    self.'add_parent'(meta, $P0)
  have_parents:

    # Finally, create proto object.
    .local pmc proto
    .local pmc name
    name = getattribute meta, 'longname'
    proto = self.'register'(parrotclass, 'how'=>meta)
    transform_to_p6opaque proto
    .return (proto)
.end


=item can(object, name)

=cut

.sub 'can' :method
    .param pmc obj
    .param string name
    push_eh not_found
    $P0 = find_method obj, name
    pop_eh
    .return ($P0)
  not_found:
    pop_eh
    $P0 = new ['Nil']
    .return ($P0)
.end


=item does(object, role)

Tests role membership.

=cut

.sub 'does' :method
    .param pmc obj
    .param pmc type

    # Check if we have a Perl6Role - needs special handling.
    $I0 = isa type, 'Perl6Role'
    unless $I0 goto not_p6role
    .tailcall type.'ACCEPTS'(obj)
  not_p6role:
    $I0 = does obj, type
    .const 'Sub' $P1 = '&prefix:<?>'
    .tailcall $P1($I0)
.end


=item parents()

Gets a list of this class' parents.

=cut

.sub 'parents' :method
    .param pmc obj
    .param pmc local   :named('local') :optional
    .param pmc tree    :named('tree') :optional
    
    # Create result list.
    .local pmc parrot_class, result_list, parrot_list, it
    result_list = get_root_global [.RAKUDO_HLL], 'Array'
    result_list = result_list.'new'()

    # We'll get the proto-object, then get the Parrot Class from that.
    obj = obj.'WHAT'()
    parrot_class = self.'get_parrotclass'(obj)

    # Fake top of Perl 6 hierarchy.
    $S0 = parrot_class.'name'()
    if $S0 == 'Perl6Object' goto done

    # If it's local can just use inspect.
    unless null tree goto do_tree
    if null local goto all_parents
  do_tree:
    parrot_list = inspect parrot_class, 'parents'
    it = iter parrot_list
    goto it_loop

    # If it's all parents, get the MRO and just waste the first item (which
    # is ourself).
  all_parents:
    parrot_list = inspect parrot_class, 'all_parents'
    it = iter parrot_list
    $P0 = shift it

    # Now loop and build result list. We package up things inside an
    # ObjectRef to make sure List and Array introspection doesn't go
    # horribly wrong.
  it_loop:
    unless it goto it_loop_end
    $P0 = shift it
    $I0 = isa $P0, 'PMCProxy'
    if $I0 goto it_loop
    parrot_class = self.'get_parrotclass'($P0)
    $S0 = parrot_class.'name'()
    if $S0 == 'P6object' goto done
    $P0 = getprop 'metaclass', $P0
    $P0 = $P0.'WHAT'()
    $P0 = new 'ObjectRef', $P0
    if null tree goto push_this
    $P1 = self.'parents'($P0, 'tree'=>tree)
    $P1.'unshift'($P0)
    $P0 = new 'Perl6Scalar', $P1
  push_this:
    result_list.'push'($P0)
    goto it_loop
  it_loop_end:
    goto done

  done:
    .return (result_list)
.end


=item methods(object)

Gets a list of methods.

=cut

.sub 'methods' :method
    .param pmc obj
    .param pmc adverbs :named :slurpy

    .local pmc local, tree, private
    local = adverbs['local']
    tree = adverbs['tree']
    private = adverbs['private']
    if null private goto private_setup
    if private goto private_setup
    private = null
  private_setup:

    .local pmc parrot_class, method_hash, result_list, it, cur_meth
    obj = obj.'WHAT'()
    parrot_class = self.'get_parrotclass'(obj)

    # Create array to put results in.
    result_list = get_root_global [.RAKUDO_HLL], 'Array'
    result_list = result_list.'new'()

    # Get methods for this class and build list of methods.
    method_hash = inspect parrot_class, "methods"
    it = iter method_hash
  it_loop:
    unless it goto it_loop_end
    $S0 = shift it
    unless null private goto private_done
    $S1 = substr $S0, 0, 1
    if $S1 == '!' goto it_loop
  private_done:
    cur_meth = method_hash[$S0]
    result_list.'push'(cur_meth)
    goto it_loop
  it_loop_end:

    # If we're in local mode or we reached the top of the hierarchy, we're done.
    $S0 = parrot_class
    if $S0 == 'Perl6Object' goto done
    if null local goto not_local
    if local goto done
  not_local:

    # Otherwise, need to get methods of our parent classes too. Recurse; if
    # we are wanting a hierarchical list then we push the resulting Array
    # straight on, so it won't flatten. Otherwise we do .list so what we
    # push will flatten.
    .local pmc parents, cur_parent, parent_methods
    parents = inspect parrot_class, 'parents'
    it = iter parents
  parent_it_loop:
    unless it goto parent_it_loop_end
    cur_parent = shift it
    $I0 = isa cur_parent, 'PMCProxy'
    if $I0 goto parent_it_loop
    cur_parent = getprop 'metaclass', cur_parent
    cur_parent = cur_parent.'WHAT'()
    parent_methods = self.'methods'(cur_parent, adverbs :flat :named)
    if null tree goto not_tree
    unless tree goto not_tree
    parent_methods = new 'Perl6Scalar', parent_methods
  not_tree:
    result_list.'push'(parent_methods)
    goto parent_it_loop
  parent_it_loop_end:

  done:
    .return (result_list)
.end


=item roles(object)

Gets a list of roles done by the class of this object.

=cut

.sub 'roles' :method
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

    # Create result list.
    .local pmc result_list
    result_list = get_root_global [.RAKUDO_HLL], 'Array'
    result_list = result_list.'new'()

    # Get list of parents whose roles we are interested in, and put
    # us on the start. With the local flag, that's just us.
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
    $P0 = self.'roles'(cur_class, 'tree'=>tree)
    $P0 = new 'Perl6Scalar', $P0
    result_list.'push'($P0)
    goto parents_it_loop
  tree_handled:

    # The list of roles is flattened out when we actually compose, so we
    # don't inspect the Parrot class, but rather the to-compose list that
    # is attached to it.
    .local pmc roles, role_it, cur_role
    roles = getattribute self, '$!roles'
    if null roles goto done
    role_it = iter roles
  role_it_loop:
    unless role_it goto role_it_loop_end
    cur_role = shift role_it
    result_list.'push'(cur_role)
    goto role_it_loop
  role_it_loop_end:
    goto parents_it_loop

  done:
    .return (result_list)
.end


=item WHAT

Overridden since WHAT inherited from P6metaclass doesn't quite work out.
XXX Work out exactly why.

=cut

.sub 'WHAT' :method
    $P0 = getattribute self, 'protoobject'
    if null $P0 goto proto_of_how
    .return ($P0)
  proto_of_how:
    $P0 = self.'HOW'()
    .tailcall $P0.'WHAT'()
.end


=item hides

Accessor for hides property.

=cut

.sub 'hides' :method
    $P0 = getattribute self, '$!hides'
    unless null $P0 goto done
    $P0 = get_hll_global 'Array'
    $P0 = $P0.'new'()
    setattribute self, '$!hides', $P0
  done:
    .return ($P0)
.end


=item hidden

Accessor for hidden property.

=cut

.sub 'hidden' :method
    $P0 = getattribute self, '$!hidden'
    unless null $P0 goto done
    $P0 = get_hll_global ['Bool'], 'False'
    $P0 = new ['Perl6Scalar'], $P0
    setattribute self, '$!hidden', $P0
  done:
    .return ($P0)
.end


=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
