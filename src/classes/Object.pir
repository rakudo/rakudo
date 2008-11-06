## $Id$

=head1 TITLE

Object - Perl 6 Object class

=head1 DESCRIPTION

This file sets up the base classes and methods for Perl 6's
object system.  Differences (and conflicts) between Parrot's
object model and the Perl 6 model means we have to do a little
name and method trickery here and there, and this file takes
care of much of that.

=head2 Functions

=over

=item onload()

Perform initializations and create the base classes.

=cut

.namespace []
.sub 'onload' :anon :init :load
    .local pmc p6meta
    load_bytecode 'P6object.pbc'
    $P0 = get_root_global ['parrot'], 'P6metaclass'
    $P0.'new_class'('Perl6Object', 'name'=>'Object')
    p6meta = $P0.'HOW'()
    set_hll_global ['Perl6Object'], '$!P6META', p6meta
.end

=item infix:=(source)  (assignment method)

Assigns C<source> to C<target>.  We use the 'item' method to allow Lists
and Mappings to be converted into Array(ref) and Hash(ref).

=cut

.namespace ['Perl6Object']
.sub 'infix:=' :method
    .param pmc source
    $I0 = can source, 'item'
    unless $I0 goto have_source
    source = source.'item'()
  have_source:

    $I0 = isa self, 'Mutable'
    unless $I0 goto copy
    assign self, source
    goto end

  copy:
    .local pmc type
    getprop type, 'type', self
    if null type goto do_assign
    $I0 = type.'ACCEPTS'(source)
    if $I0 goto do_assign
    die "Type mismatch in assignment."

  do_assign:
    eq_addr self, source, end
    copy self, source
  end:
    .return (self)
.end


=back

=head2 Object methods

=over 4

=item hash()

Return the scalar as a Hash.

=cut

.namespace ['Perl6Object']

.sub 'hash' :method
    $P0 = self.'list'()
    .return $P0.'hash'()
.end

=item item()

Return the scalar component of the invocant.  For most objects,
this is simply the invocant itself.

=cut

.namespace []
.sub 'item'
    .param pmc x               :slurpy
    $I0 = elements x
    unless $I0 == 1 goto have_x
    x = shift x
  have_x:
    $I0 = can x, 'item'
    unless $I0 goto have_item
    x = x.'item'()
  have_item:
    .return (x)
.end

.namespace ['Perl6Object']
.sub 'item' :method
    .return (self)
.end


=item list()

Return the list component of the invocant.  For most (Scalar)
objects, we create a List containing the invocant.

=cut

.sub 'list' :method
    $P0 = new 'List'
    push $P0, self
    .return ($P0)
.end


=item defined()

Return true if the object is defined.

=cut

.sub 'defined' :method
    $P0 = get_hll_global ['Bool'], 'True'
    .return ($P0)
.end

.sub '' :method :vtable('defined')
    $I0 = self.'defined'()
    .return ($I0)
.end


=item Str()

Return a string representation of the object

=cut

.sub 'Str' :method
    $P0 = new 'ResizableStringArray'
    $P1 = self.'WHAT'()
    push $P0, $P1
    $I0 = get_addr self
    push $P0, $I0
    $S0 = sprintf "%s<0x%x>", $P0
    .return ($S0)
.end

.sub '' :method :vtable('get_string')
    $S0 = self.'Str'()
    .return ($S0)
.end


=item new()

Create a new object having the same class as the invocant.

=cut

.sub 'new' :method
    .param pmc init_parents :slurpy
    .param pmc init_this    :named :slurpy

    # Instantiate.
    .local pmc p6meta
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    $P0 = p6meta.get_parrotclass(self)
    $P1 = new $P0

    # If this proto object has a WHENCE auto-vivification, we should use
    # put any values it contains but that init_this does not into init_this.
    .local pmc whence
    whence = self.'WHENCE'()
    unless whence goto no_whence
    .local pmc this_whence_iter
    this_whence_iter = new 'Iterator', whence
  this_whence_iter_loop:
    unless this_whence_iter goto no_whence
    $S0 = shift this_whence_iter
    $I0 = exists init_this[$S0]
    if $I0 goto this_whence_iter_loop
    $P2 = whence[$S0]
    init_this[$S0] = $P2
    goto this_whence_iter_loop
  no_whence:

    # Now we will initialize each attribute in the class itself and it's
    # parents with an Undef or the specified initialization value. Note that
    # the all_parents list includes ourself.
    .local pmc all_parents, class_iter
    all_parents = inspect $P0, "all_parents"
    class_iter = new 'Iterator', all_parents
  class_iter_loop:
    unless class_iter goto class_iter_loop_end
    .local pmc cur_class
    cur_class = shift class_iter

    # If it's PMCProxy, then skip over it, since it's attribute is the delegate
    # instance of a parent PMC class, which we should not change to Undef.
    .local int is_pmc_proxy
    is_pmc_proxy = isa cur_class, "PMCProxy"
    if is_pmc_proxy goto class_iter_loop_end

    # If this the current class?
    .local pmc init_attribs
    eq_addr cur_class, $P0, current_class

    # If it's not the current class, need to see if we have any attributes.
    # Go through the provided init_parents to see if we have anything that
    # matches.
    .local pmc ip_iter, cur_ip
    ip_iter = new 'Iterator', init_parents
  ip_iter_loop:
    unless ip_iter goto ip_iter_loop_end
    cur_ip = shift ip_iter

    # We will check if their HOW matches.
    $P2 = p6meta.'get_parrotclass'(cur_ip)
    eq_addr cur_class, $P2, found_parent_init

    goto found_init_attribs
  ip_iter_loop_end:

    # If we get here, found nothing.
    init_attribs = new 'Hash'
    goto parent_init_search_done

    # We found some parent init data, potentially.
  found_parent_init:
    init_attribs = cur_ip.WHENCE()
    $I0 = 'defined'(init_attribs)
    if $I0 goto parent_init_search_done
    init_attribs = new 'Hash'
  parent_init_search_done:
    goto found_init_attribs

    # If it's the current class, we will take the init_this hash.
  current_class:
    init_attribs = init_this
  found_init_attribs:

    # Now go through attributes of the current class and iternate over them.
    .local pmc attribs, iter
    attribs = inspect cur_class, "attributes"
    iter = new 'Iterator', attribs
  iter_loop:
    unless iter goto iter_end
    $S0 = shift iter

    # See if we have an init value; use Undef if not.
    .local int got_init_value
    $S1 = substr $S0, 2
    got_init_value = exists init_attribs[$S1]
    if got_init_value goto have_init_value
    $P2 = new 'Undef'
    goto init_done
  have_init_value:
    $P2 = init_attribs[$S1]
    delete init_attribs[$S1]
  init_done:

    # Is it a scalar? If so, want a scalar container with the type set on it.
    .local string sigil
    sigil = substr $S0, 0, 1
    if sigil != '$' goto no_scalar
    .local pmc attr_info, type
    attr_info = attribs[$S0]
    if null attr_info goto set_attrib
    type = attr_info['type']
    if null type goto set_attrib
    if got_init_value goto no_proto_init
    $I0 = isa type, 'P6protoobject'
    unless $I0 goto no_proto_init
    set $P2, type
  no_proto_init:
    $P2 = new 'Perl6Scalar', $P2
    setprop $P2, 'type', type
    goto set_attrib
  no_scalar:

    # Is it an array? If so, initialize to Perl6Array.
    if sigil != '@' goto no_array
    $P2 = new 'Perl6Array'
    goto set_attrib
  no_array:

    # Is it a Hash? If so, initialize to Perl6Hash.
    if sigil != '%' goto no_hash
    $P2 = new 'Perl6Hash'
    goto set_attrib
  no_hash:

  set_attrib:
    push_eh set_attrib_eh
    setattribute $P1, cur_class, $S0, $P2
  set_attrib_eh:
    pop_eh
    goto iter_loop
  iter_end:

    # Do we have anything left in the hash? If so, unknown.
    $I0 = elements init_attribs
    if $I0 == 0 goto init_attribs_ok
    'die'("You passed an initialization parameter that does not have a matching attribute.")
  init_attribs_ok:

    # Next class.
    goto class_iter_loop
  class_iter_loop_end:

    .return ($P1)
.end

=item WHENCE()

Return the invocant's auto-vivification closure.

=cut

.sub 'WHENCE' :method
    $P0 = self.'WHAT'()
    $P1 = $P0.'WHENCE'()
    .return ($P1)
.end

=item REJECTS(topic)

Define REJECTS methods for objects (this would normally
be part of the Pattern role, but we put it here for now
until we get roles).

=cut

.sub 'REJECTS' :method
    .param pmc topic
    $P0 = self.'ACCEPTS'(topic)
    n_not $P0, $P0
    .return ($P0)
.end

=item true()

Defines the .true method on all objects via C<prefix:?>.

=cut

.sub 'true' :method
 .return 'prefix:?'(self)
.end

=item get_bool (vtable)

Returns true if the object is defined, false otherwise.

=cut

.sub '' :vtable('get_bool')
    $I0 = 'defined'(self)
    .return ($I0)
.end

=item print()

=item say()

Print the object

=cut

.sub 'print' :method
    $P0 = get_hll_global 'print'
    .return $P0(self)
.end

.sub 'say' :method
    $P0 = get_hll_global 'say'
    .return $P0(self)
.end

=item WHERE

Gets the memory address of the object.

=cut

.sub 'WHERE' :method
    $I0 = get_addr self
    .return ($I0)
.end

=item WHICH

Gets the object's identity value

=cut

.sub 'WHICH' :method
    # For normal objects, this can just be the memory address.
    .return self.'WHERE'()
.end

=back

=head2 Private methods

=over 4

=item !cloneattr(attrlist)

Create a clone of self, also cloning the attributes given by attrlist.

=cut

.sub '!cloneattr' :method
    .param string attrlist
    .local pmc p6meta, result
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    $P0 = p6meta.'get_parrotclass'(self)
    result = new $P0

    .local pmc attr_it
    attr_it = split ' ', attrlist
  attr_loop:
    unless attr_it goto attr_end
    $S0 = shift attr_it
    unless $S0 goto attr_loop
    $P1 = getattribute self, $S0
    $P1 = clone $P1
    setattribute result, $S0, $P1
    goto attr_loop
  attr_end:
    .return (result)
.end


.sub '!.?' :method
    .param string method_name
    .param pmc pos_args     :slurpy
    .param pmc named_args   :slurpy :named

    # For now we won't worry about signature, just if a method exists.
    $I0 = can self, method_name
    if $I0 goto invoke
    .return '!FAIL'('Undefined value returned by invocation of undefined method')

    # If we do have a method, call it.
  invoke:
    .return self.method_name(pos_args :flat, named_args :named :flat)
.end


.sub '!.*' :method
    .param string method_name
    .param pmc pos_args     :slurpy
    .param pmc named_args   :slurpy :named

    # Return an empty list if no methods exist at all.
    $I0 = can self, method_name
    if $I0 goto invoke
    .return 'list'()

    # Now find all methods and call them - since we know there are methods,
    # we just pass on to infix:.+.
  invoke:
    .return self.'!.+'(method_name, pos_args :flat, named_args :named :flat)
.end


.sub '!.+' :method
    .param string method_name
    .param pmc pos_args     :slurpy
    .param pmc named_args   :slurpy :named

    # We need to find all methods we could call with the right name.
    .local pmc p6meta, result_list, class, mro, it, cap_class, failure_class
    result_list = 'list'()
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    class = self.'HOW'()
    class = p6meta.get_parrotclass(class)
    mro = inspect class, 'all_parents'
    it = iter mro
    cap_class = get_hll_global 'Capture'
    failure_class = get_hll_global 'Failure'
  mro_loop:
    unless it goto mro_loop_end
    .local pmc cur_class, meths, cur_meth
    cur_class = shift it
    meths = inspect cur_class, 'methods'
    cur_meth = meths[method_name]
    if null cur_meth goto mro_loop

    # If we're here, found a method. Invoke it and add capture of the results
    # to the result list.
    .local pmc pos_res, named_res, cap
    (pos_res :slurpy, named_res :named :slurpy) = cur_meth(self, pos_args :flat, named_args :named :flat)
    cap = 'prefix:\\'(pos_res :flat, named_res :flat :named)
    push result_list, cap
    goto mro_loop
  mro_loop_end:

    # Make sure we got some elements, or we have to die.
    $I0 = elements result_list
    if $I0 == 0 goto failure
    .return (result_list)
  failure:
    $S0 = "Could not invoke method '"
    concat $S0, method_name
    concat $S0, "' on invocant of type '"
    $S1 = self.WHAT()
    concat $S0, $S1
    concat $S0, "'"
    'die'($S0)
.end


.sub '!.^' :method
    .param string method_name
    .param pmc pos_args     :slurpy
    .param pmc named_args   :slurpy :named

    # Get the HOW or the object and do the call on that.
    .local pmc how
    how = self.'HOW'()
    .return how.method_name(self, pos_args :flat, named_args :flat :named)
.end


.namespace ['P6protoobject']

=back

=head2 Methods on P6protoobject

=over

=item WHENCE()

Returns the protoobject's autovivification closure.

=cut

.sub 'WHENCE' :method
    .local pmc props, whence
    props = getattribute self, '%!properties'
    if null props goto ret_undef
    whence = props['WHENCE']
    if null whence goto ret_undef
    .return (whence)
  ret_undef:
    whence = new 'Undef'
    .return (whence)
.end


=item defined()

=cut

.sub 'defined' :method
    $P0 = get_hll_global ['Bool'], 'False'
    .return ($P0)
.end


=item item()

Returns itself in item context.

=cut

.sub 'item' :method
    .return (self)
.end


=item list()

Returns a list containing itself in list context.

=cut

.sub 'list' :method
    $P0 = get_hll_global 'list'
    .return $P0(self)
.end


=item get_pmc_keyed(key)    (vtable method)

Returns a proto-object with an autovivification closure attached to it.

=cut

.sub get_pmc_keyed :vtable :method
    .param pmc what

    # We'll build auto-vivification hash of values.
    .local pmc WHENCE, key, val
    WHENCE = new 'Hash'

    # What is it?
    $S0 = what.'WHAT'()
    if $S0 == 'Pair' goto from_pair
    if $S0 == 'List' goto from_list
    'die'("Auto-vivification closure did not contain a Pair")

  from_pair:
    # Just a pair.
    key = what.'key'()
    val = what.'value'()
    WHENCE[key] = val
    goto done_whence

  from_list:
    # List.
    .local pmc list_iter, cur_pair
    list_iter = new 'Iterator', what
  list_iter_loop:
    unless list_iter goto done_whence
    cur_pair = shift list_iter
    key = cur_pair.'key'()
    val = cur_pair.'value'()
    WHENCE[key] = val
    goto list_iter_loop
  done_whence:

    # Now create a clone of the protoobject.
    .local pmc protoclass, res, props, tmp
    protoclass = class self
    res = new protoclass

    # Attach the WHENCE property.
    props = getattribute self, '%!properties'
    unless null props goto have_props
    props = new 'Hash'
  have_props:
    props['WHENCE'] = WHENCE
    setattribute res, '%!properties', props

    .return (res)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
