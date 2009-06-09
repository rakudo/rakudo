## $Id$

=head1 TITLE

ClassHOW - default metaclass

=head1 DESCRIPTION

This file for now actually just adds a method or two into P6metaclass. In the
long run, we probably need to subclass that, and make sure we have all of the
methods in here that are defined in the HOW API.

=head2 Methods on P6metaclass

=over

=item does(object, role)

Tests role membership.

=cut

.namespace ['P6metaclass']
.sub 'does' :method
    .param pmc obj
    .param pmc type

    # Check if we have a Perl6Role - needs special handling.
    $I0 = isa type, 'Perl6Role'
    unless $I0 goto not_p6role
    .tailcall type.'ACCEPTS'(obj)
  not_p6role:
    $I0 = does obj, type
    .const 'Sub' $P1 = 'prefix:?'
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
    if $S0 != 'Perl6Object' goto not_object
    unless null local goto done
    $P0 = get_hll_global 'Object'
    result_list.'push'($P0)
    goto done
  not_object:

    # If it's local can just use inspect.
    unless null tree goto do_tree
    if null local goto all_parents
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
    result_list.'push'($P0)
    goto it_loop
  it_loop_end:
    goto done

  do_tree:
    'die'(':tree not yet implemented')

  done:
    .return (result_list)
.end


=item methods(object)

Gets a list of methods.

XXX Spec and implement various flags it takes, which currently aren't in S12.

XXX Fix bugs with introspecting some built-in classes (List, Str...)

=cut

.sub 'methods' :method
    .param pmc obj
    .param pmc adverbs :named :slurpy

    .local pmc local, tree, private
    local = adverbs['local']
    tree = adverbs['tree']
    private = adverbs['private']

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
    parent_methods = self.'methods'(cur_parent)
    if null tree goto not_tree
    if tree goto flatten_done
  not_tree:
    parent_methods = parent_methods.'list'()
  flatten_done:
    result_list.'push'(parent_methods)
    goto parent_it_loop
  parent_it_loop_end:

  done:
    .return (result_list)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
