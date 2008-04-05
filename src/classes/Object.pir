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

.namespace ['Perl6Object']

.sub 'onload' :anon :init :load
    ##  create a new 'Object' base class.  We can't call it 'Object'
    ##  because Parrot has already taken that classname (RT#43419).
    .local pmc objectclass
    objectclass = newclass 'Perl6Object'

    ##  need a place to store variable type, if we have one; this is
    ##  needed per value because we have no container that exists
    ##  between assignments
    addattribute objectclass, 'vartype'

    ##  create a Perl6Protoobject class.  We don't call it 'Protoobject'
    ##  to avoid conflicts with the Protoobject class used by PCT and PGE.
    .local pmc protoclass
    protoclass = subclass objectclass, 'Perl6Protoobject'
    addattribute protoclass, 'shortname'
    addattribute protoclass, 'HOW'

    ##  create the protoobject for the new class, initialize its
    ##  shortname, and set up the symbol/type mappings.
    .local pmc protoobject
    protoobject = new protoclass
    $P1 = new 'String'
    $P1 = 'Object'
    setattribute protoobject, 'shortname', $P1
    setattribute protoobject, 'HOW', objectclass
    set_hll_global 'Object', protoobject
    set_hll_global 'Perl6Object', protoobject
.end


=item make_proto(class [, 'name'=>name] )

Create protoobjects and mappings for C<class>, using C<name>
as the Perl 6 name for the class.  The C<class> argument can
be a Parrot Class object, or anything that will obtain a
Parrot class via the C<get_class> opcode.

=cut

.sub 'make_proto'
    .param pmc class
    .param string name         :optional :named('name')
    .param int has_name        :opt_flag

    ##  get the Parrot class object if we don't already have it
    $I0 = isa class, 'Class'
    if $I0 goto have_class
    class = get_class class
  have_class:

    ##  if the class is already a Perl6Object, we have methods already.
    ##  if it's a PMCProxy, we have to add methods to the namespace.
    ##  otherwise, we just add Perl6Object as a parent class
    $I0 = isa class, 'Perl6Object'
    if $I0 goto object_methods_done
    $I0 = isa class, 'PMCProxy'
    if $I0 goto object_methods_proxy
    $P0 = get_class 'Perl6Object'
    class.'add_parent'($P0)
    goto object_methods_done
  object_methods_proxy:
    ##  for PMCProxy classes, we have to add Perl6Object's methods
    ##  directly as subs into the class' namespace.
    ##  get the class' namespace object
    .local pmc ns
    ns = class.'get_namespace'()
    ##  iterate over Perl6Object's methods, adding them to the namespace
    .local pmc methods, iter
    $P0 = get_class 'Perl6Object'
    methods = $P0.'methods'()
    iter = new 'Iterator', methods
  iter_loop:
    unless iter goto iter_end
    $S0 = shift iter
    ##  if the class/namespace already has the named sub, skip it
    $P0 = ns.find_sub($S0)
    unless null $P0 goto iter_loop
    $P0 = methods[$S0]
    ns.add_sub($S0, $P0)
    goto iter_loop
  iter_end:
  object_methods_done:

    ##  get the associated namespace and shortname
    .local pmc shortname
    ns = split '::', name
    shortname = pop ns

    ##  create a new class for the protoobject
    .local pmc protoclass, protoobject
    protoclass = new 'Class'
    $P0 = get_class 'Perl6Protoobject'
    protoclass.'add_parent'($P0)
    protoclass.'add_parent'(class)

    ##  set up the protoobject and its attributes
    protoobject = new protoclass
    setattribute protoobject, 'shortname', shortname
    setattribute protoobject, 'HOW', class

    ##  register the protoobject under its Parrot name and
    ##  its Perl 6 name.
    .local pmc sample
    sample = new class
    $S0 = typeof sample
    set_hll_global $S0, protoobject
    $S0 = shortname
    set_hll_global ns, $S0, protoobject

    .return (protoobject)
.end

=item !keyword_class(name)

Internal helper method to create a class.

=cut

.sub '!keyword_class' :method
    .param string name
    .local pmc class, resolve_list, methods, iter

    # Create class.
    class = newclass name

    # Set resolve list to include all methods of the class.
    methods = inspect class, 'methods'
    iter = new 'Iterator', methods
    resolve_list = new 'ResizableStringArray'
resolve_loop:
    unless iter goto resolve_loop_end
    $P0 = shift iter
    push resolve_list, $P0
    goto resolve_loop
resolve_loop_end:
    class.resolve_method(resolve_list)

    .return(class)
.end

=item !keyword_role(name)

Internal helper method to create a role.

=cut

.sub '!keyword_role' :method
    .param string name
    .local pmc info, role

    # Need to make sure it ends up attached to the right
    # namespace.
    info = new 'Hash'
    info['name'] = name
    $P0 = new 'ResizablePMCArray'
    $P0[0] = name
    info['namespace'] = $P0

    # Create role.
    role = new 'Role', info

    # Stash in namespace.
    $P0 = new 'ResizableStringArray'
    set_hll_global $P0, name, role

    .return(role)
.end

=item !keyword_does(class, role_name)

Internal helper method to implement the functionality of the does keyword.

=cut

.sub '!keyword_does' :method
    .param pmc class
    .param string role_name
    .local pmc role
    role = get_hll_global role_name
    addrole class, role
.end

=item !keyword_has(class, attr_name)

Adds an attribute with the given name to the class.

=cut

.sub '!keyword_has' :method
    .param pmc class
    .param string attr_name
    addattribute class, attr_name
.end

=back

=head2 Object methods

=over

=item new()

Create a new object having the same class as the invocant.

=cut

.sub 'new' :method
    .param pmc init_attribs :named :slurpy

    # Instantiate.
    $P0 = self.'HOW'()
    $P1 = new $P0

    # Initialize each attribute with an Undef or the supplied value.
    .local pmc attribs, iter
    attribs = inspect $P0, "attributes"
    iter = new 'Iterator', attribs
  iter_loop:
    unless iter goto iter_end
    $S0 = shift iter
    $S1 = substr $S0, 2
    $I0 = exists init_attribs[$S1]
    if $I0 goto have_init_value
    $P2 = new 'Undef'
    goto init_done
  have_init_value:
    $P2 = init_attribs[$S1]
  init_done:
    push_eh set_attrib_eh
    setattribute $P1, $S0, $P2
set_attrib_eh:
    goto iter_loop
  iter_end:

    .return ($P1)
.end


=item isa($class)

Returns true if the invocant is of type $class.

=cut

.sub 'isa' :method
    .param string x
    $S0 = self.'WHAT'()
    $I0 = iseq $S0, x
    .return ($I0)
.end


=item WHAT()

Return the invocant's protoobject.

=cut

.sub 'WHAT' :method
    $S0 = typeof self
    $P0 = get_hll_global $S0
    .return ($P0)
.end

=item HOW()

Return the invocant's metaclass object (in Parrot, this is the
class object for the invocant).

=cut

.sub 'HOW' :method
    $P0 = self.'WHAT'()
    $P1 = $P0.'HOW'()
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

=item say()

Print the object

=cut

.sub 'say' :method
    $P0 = get_hll_global 'say'
    .return $P0(self)
.end

=back

=head2 Protoobject methods

Protoobjects are described in Synopsis 12, these are objects
that are "empty" instances that differ in definedness and how
they respond to certain methods.

=over

=item get_string()   (vtable method)

Returns the short name of the class (prototype objects stringify
to the short name).

=cut

.namespace ['Perl6Protoobject']

.sub 'get_string' :vtable :method
    $P0 = getattribute self, 'shortname'
    $S0 = $P0
    .return ($S0)
.end

=item defined()   (vtable method)

Returns false (prototype objects evaluate as undef).

=cut

.sub 'defined' :vtable :method
    .return (0)
.end

=item HOW()

Returns the metaclass (Parrot class) of the protoobject.

=cut

.sub 'HOW' :method
    $P0 = getattribute self, 'HOW'
    .return ($P0)
.end

=item WHAT()

Returns the invocant's protoobject, which in the case of a protoobject
is just itself.

=cut

.sub 'WHAT' :method
    .return (self)
.end

=item ACCEPTS(topic)

=cut

.sub 'ACCEPTS' :method
    .param pmc topic
    $P0 = self.'HOW'()
    $I0 = does topic, $P0
    .return 'prefix:?'($I0)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
