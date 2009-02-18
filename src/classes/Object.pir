## $Id$

=head1 TITLE

Object - Perl 6 Object class

=head1 DESCRIPTION

This file sets up the base classes and methods for Perl 6's
object system.  Differences (and conflicts) between Parrot's
object model and the Perl 6 model means we have to do a little
name and method trickery here and there, and this file takes
care of much of that.

=cut

.namespace []
.sub '' :anon :init :load
    .local pmc p6meta
    load_bytecode 'PCT.pbc'
    $P0 = get_root_global ['parrot'], 'P6metaclass'
    $P0.'new_class'('Perl6Object', 'name'=>'Object')
    p6meta = $P0.'HOW'()
    set_hll_global ['Perl6Object'], '$!P6META', p6meta
.end

=head2 Methods

=over 4

=item clone()

Returns a copy of the object.

NOTE: Don't copy what this method does; it's a tad inside-out. We should be
overriding the clone vtable method to call .clone() really. But if we do that,
we can't current get at the Object PMC's clone method, so for now we do it
like this.

=cut

.namespace ['Perl6Object']
.sub 'clone' :method
    .param pmc new_attrs :slurpy :named

    # Make a clone.
    .local pmc result
    self = '!DEREF'(self)
    result = clone self

    # Set any new attributes.
    .local pmc p6meta, parrotclass, attributes, it
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    parrotclass = p6meta.'get_parrotclass'(result)
    if null parrotclass goto attrinit_done
    attributes = inspect parrotclass, 'attributes'
    it = iter attributes
  attrinit_loop:
    unless it goto attrinit_done
    .local string attrname, shortname
    attrname = shift it
    shortname = substr attrname, 2
    $I0 = exists new_attrs[shortname]
    unless $I0 goto attrinit_loop
    $P0 = getattribute result, attrname
    $P1 = new_attrs[shortname]
    'infix:='($P0, $P1)
    goto attrinit_loop
  attrinit_done:

    .return (result)
.end


=item defined()

Return true if the object is defined.

=cut

.namespace ['Perl6Object']
.sub 'defined' :method
    $P0 = get_hll_global ['Bool'], 'True'
    .return ($P0)
.end


=item hash

Return invocant in hash context.

=cut

.namespace ['Perl6Object']
.sub 'hash' :method
    .tailcall self.'Hash'()
.end

.namespace []
.sub 'hash'
    .param pmc values :slurpy
    .tailcall values.'Hash'()
.end

=item item

Return invocant in item context.  Default is to return self.

=cut

.namespace ['Perl6Object']
.sub 'item' :method
    .return (self)
.end

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


=item iterator

=cut

.namespace ['Perl6Object']
.sub 'iterator' :method
    $P0 = self.'list'()
    .tailcall $P0.'iterator'()
.end


=item list

Return invocant in list context.  Default is to return a List containing self.

=cut

.namespace ['Perl6Object']
.sub 'list' :method
    $P0 = new 'List'
    push $P0, self
    .return ($P0)
.end

=item print()

Print the object.

=cut

.namespace ['Perl6Object']
.sub 'print' :method
    $P0 = get_hll_global 'print'
    .tailcall $P0(self)
.end

=item say()

Print the object, followed by a newline.

=cut

.namespace ['Perl6Object']
.sub 'say' :method
    $P0 = get_hll_global 'say'
    .tailcall $P0(self)
.end

=item true()

Boolean value of object -- defaults to C<.defined> (S02).

=cut

.namespace ['Perl6Object']
.sub 'true' :method
    .tailcall self.'defined'()
.end

=back

=head2 Coercion methods

=over 4

=item Array()

=cut

.namespace ['Perl6Object']
.sub 'Array' :method
    $P0 = new 'Perl6Array'
    $P0.'!STORE'(self)
    .return ($P0)
.end

=item Hash()

=cut

.namespace ['Perl6Object']
.sub 'Hash' :method
    $P0 = new 'Perl6Hash'
    $P0.'!STORE'(self)
    .return ($P0)
.end

=item Iterator()

=cut

.sub 'Iterator' :method
    $P0 = self.'list'()
    .tailcall $P0.'Iterator'()
.end

=item Scalar()

Default Scalar() gives reference type semantics, returning
an object reference (unless the invocant already is one).

=cut

.namespace ['Perl6Object']
.sub 'Scalar' :method
    $I0 = isa self, 'Perl6Scalar'
    unless $I0 goto not_ref
    .return (self)
  not_ref:
    $P0 = new 'Perl6Scalar', self
    .return ($P0)
.end

=item Str()

Return a string representation of the invocant.  Default is
the object's type and address.

=cut

.namespace ['Perl6Object']
.sub 'Str' :method
    $P0 = new 'ResizableStringArray'
    $P1 = self.'WHAT'()
    push $P0, $P1
    $I0 = get_addr self
    push $P0, $I0
    $S0 = sprintf "%s<0x%x>", $P0
    .return ($S0)
.end

=back

=head2 Object constructor methods

=over 4

=cut

.namespace ['Perl6Object']
.sub 'bless' :method
    .param pmc posargs         :slurpy
    .param pmc attrinit        :slurpy :named

    .local pmc candidate
    candidate = self.'CREATE'()
    .tailcall self.'BUILDALL'(candidate, attrinit, posargs)
.end


.sub 'BUILD' :method
    .param pmc candidate
    .param pmc attrinit        :slurpy :named

    .local pmc p6meta, parrotclass, attributes, it
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    parrotclass = p6meta.'get_parrotclass'(self)
    attributes = inspect parrotclass, 'attributes'
    it = iter attributes
  attrinit_loop:
    unless it goto attrinit_done
    .local string attrname
    .local pmc attrhash, itypeclass
    attrname = shift it
    attrhash = attributes[attrname]
    itypeclass = attrhash['itype']
    unless null itypeclass goto attrinit_itype
    $S0 = substr attrname, 0, 1
    if $S0 == '@' goto attrinit_array
    if $S0 == '%' goto attrinit_hash
    itypeclass = get_class ['Perl6Scalar']
    goto attrinit_itype
  attrinit_array:
    itypeclass = get_class ['Perl6Array']
    goto attrinit_itype
  attrinit_hash:
    itypeclass = get_class ['Perl6Hash']
  attrinit_itype:
    .local pmc attr
    attr = new itypeclass
    setattribute candidate, parrotclass, attrname, attr
    $P0 = attrhash['type']
    setprop attr, 'type', $P0
    .local string keyname
    $I0 = index attrname, '!'
    if $I0 < 0 goto attrinit_loop
    inc $I0
    keyname = substr attrname, $I0
    $P0 = attrinit[keyname]
    unless null $P0 goto attrinit_assign
    $P0 = attrhash['init_value']
    if null $P0 goto attrinit_loop
  attrinit_assign:
    'infix:='(attr, $P0)
    goto attrinit_loop
  attrinit_done:
    .return (candidate)
.end


.sub 'BUILDALL' :method
    .param pmc candidate
    .param pmc attrinit
    .param pmc posargs

    .include 'iterator.pasm'
    .local pmc p6meta, parents, it
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    $P0 = p6meta.'get_parrotclass'(self)
    parents = inspect $P0, 'all_parents'
    it = iter parents
    set it, .ITERATE_FROM_END
  parents_loop:
    # Loop through all of the parent classes, in reverse mro.
    # For each parent class, call its BUILD method with the
    # appropriate arguments.
    unless it goto parents_done
    $P0 = pop it
    $I0 = isa $P0, 'PMCProxy'
    if $I0 goto parents_loop
    .local pmc parentproto
    $P0 = getprop 'metaclass', $P0
    parentproto = $P0.'WHAT'()
    $I0 = can parentproto, 'BUILD'
    unless $I0 goto parents_loop
    # Look through posargs for a corresponding protoobject
    # with a WHENCE property.  If found, that WHENCE property
    # is used as the arguments to the parent class BUILD.
    .local pmc pos_it, argproto
    pos_it = iter posargs
  posargs_loop:
    unless pos_it goto posargs_done
    argproto = shift pos_it
    $P1 = argproto.'HOW'()
    ne_addr $P0, $P1, posargs_loop
    $P0 = argproto.'WHENCE'()
    if null $P0 goto posargs_done
    parentproto.'BUILD'(candidate, $P0 :flat :named)
    goto parents_loop
  posargs_done:
    parentproto.'BUILD'(candidate, attrinit :flat :named)
    goto parents_loop
  parents_done:
    .return (candidate)
.end


=item CREATE()

Create a candidate object of the type given by the invocant.

=cut

.sub 'CREATE' :method
    .local pmc p6meta
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    $P0 = p6meta.'get_parrotclass'(self)
    $P1 = new $P0
    .return ($P1)
.end


=item new()

Create a new object having the same class as the invocant.

=cut

.sub 'new' :method
    .param pmc posargs         :slurpy
    .param pmc attrinit        :slurpy :named

    .tailcall self.'bless'(posargs :flat, attrinit :flat :named)
.end

=item 'PARROT'

Report the object's true nature.

=cut

.sub 'PARROT' :method
    .local pmc obj
    .local string result
    obj = self
    result = ''
  deref_loop:
    $I0 = isa obj, 'ObjectRef'
    unless $I0 goto deref_done
    $I0 = isa obj, 'Perl6Scalar'
    if $I0 goto deref_scalar
    result .= 'ObjectRef->'
    goto deref_next
  deref_scalar:
    result .= 'Perl6Scalar->'
  deref_next:
    obj = deref obj
    goto deref_loop
  deref_done:
    $P0 = typeof obj
    $S0 = $P0
    result .= $S0
    .return (result)
.end


=item REJECTS(topic)

Define REJECTS methods for objects (this would normally
be part of the Pattern role, but we put it here for now
until we get roles).

=cut

.sub 'REJECTS' :method
    .param pmc topic
    $P0 = self.'ACCEPTS'(topic)
    $P1 = not $P0
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
    .tailcall self.'WHERE'()
.end

=back

=head2 Private methods

=over 4

=item !cloneattr(attrlist)

Create a clone of self, also cloning the attributes given by attrlist.

=cut

.namespace ['Perl6Object']
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
    if null $P1 goto null_attr
    $P1 = clone $P1
  null_attr:
    setattribute result, $S0, $P1
    goto attr_loop
  attr_end:
    .return (result)
.end

=item !.?

Helper method for implementing the .? operator. Calls at most one matching
method, and returns undef if there are none.

=cut

.sub '!.?' :method
    .param string method_name
    .param pmc pos_args     :slurpy
    .param pmc named_args   :slurpy :named

    # Get all possible methods.
    .local pmc methods
    methods = self.'!MANY_DISPATCH_HELPER'(method_name, pos_args, named_args)

    # Do we have any?
    $I0 = elements methods
    if $I0 goto invoke
    .tailcall '!FAIL'('Undefined value returned by invocation of undefined method')

    # If we do have a method, call it.
  invoke:
    $P0 = methods[0]
    .tailcall self.$P0(pos_args :flat, named_args :named :flat)
.end

=item !.*

Helper method for implementing the .* operator. Calls one or more matching
methods.

=cut

.sub '!.*' :method
    .param string method_name
    .param pmc pos_args     :slurpy
    .param pmc named_args   :slurpy :named

    # Get all possible methods.
    .local pmc methods
    methods = self.'!MANY_DISPATCH_HELPER'(method_name, pos_args, named_args)

    # Build result capture list.
    .local pmc pos_res, named_res, cap, result_list, it, cur_meth
    $P0 = get_hll_global 'list'
    result_list = $P0()
    it = iter methods
  it_loop:
    unless it goto it_loop_end
    cur_meth = shift it
    (pos_res :slurpy, named_res :named :slurpy) = cur_meth(self, pos_args :flat, named_args :named :flat)
    cap = 'prefix:\\'(pos_res :flat, named_res :flat :named)
    push result_list, cap
    goto it_loop
  it_loop_end:

    .return (result_list)
.end


=item !.+

Helper method for implementing the .+ operator. Calls one or more matching
methods, dies if there are none.

=cut

.sub '!.+' :method
    .param string method_name
    .param pmc pos_args     :slurpy
    .param pmc named_args   :slurpy :named

    # Use !.* to produce a (possibly empty) list of result captures.
    .local pmc result_list
    result_list = self.'!.*'(method_name, pos_args :flat, named_args :flat :named)

    # If we got no elements at this point, we must die.
    $I0 = elements result_list
    if $I0 == 0 goto failure
    .return (result_list)
  failure:
    $S0 = "Could not invoke method '"
    concat $S0, method_name
    concat $S0, "' on invocant of type '"
    $S1 = self.'WHAT'()
    concat $S0, $S1
    concat $S0, "'"
    'die'($S0)
.end


=item !MANY_DISPATCH_HELPER

This is a helper for implementing .+, .? and .*. In the future, it may well be
the basis of WALK also. It returns all methods we could possible call.

=cut

.sub '!MANY_DISPATCH_HELPER' :method
    .param string method_name
    .param pmc pos_args
    .param pmc named_args

    # We need to find all methods we could call with the right name.
    .local pmc p6meta, result_list, class, mro, it
    $P0 = get_hll_global 'list'
    result_list = $P0()
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    class = self.'WHAT'()
    class = p6meta.'get_parrotclass'(class)
    mro = inspect class, 'all_parents'
    it = iter mro
  mro_loop:
    unless it goto mro_loop_end
    .local pmc cur_class, meths, cur_meth
    cur_class = shift it
    meths = inspect cur_class, 'methods'
    cur_meth = meths[method_name]
    if null cur_meth goto mro_loop

    # If we're here, found a method. But is it a multi?
    $I0 = isa cur_meth, "Perl6MultiSub"
    if $I0 goto multi_dispatch

    # Single dispatch - add to the result list.
    push result_list, cur_meth
    goto mro_loop

    # Multiple dispatch; get all applicable candidates.
  multi_dispatch:
    .local pmc possibles, possibles_it
    possibles = cur_meth.'find_possible_candidates'(self, pos_args :flat)
    possibles_it = iter possibles
  possibles_it_loop:
    unless possibles_it goto possibles_it_loop_end
    cur_meth = shift possibles_it
    push result_list, cur_meth
    goto possibles_it_loop
  possibles_it_loop_end:
    goto mro_loop
  mro_loop_end:

    .return (result_list)
.end

=item !.^

Helper for doing calls on the metaclass.

=cut

.sub '!.^' :method
    .param string method_name
    .param pmc pos_args     :slurpy
    .param pmc named_args   :slurpy :named

    # Get the HOW or the object and do the call on that.
    .local pmc how
    how = self.'HOW'()
    .tailcall how.method_name(self, pos_args :flat, named_args :flat :named)
.end

=back

=head2 Vtable functions

=cut

.namespace ['Perl6Object']
.sub '' :vtable('decrement') :method
    $P0 = self.'pred'()
    'infix:='(self, $P0)
    .return(self)
.end

.sub '' :vtable('defined') :method
    $I0 = self.'defined'()
    .return ($I0)
.end

.sub '' :vtable('get_bool') :method
    $I0 = self.'true'()
    .return ($I0)
.end

.sub '' :vtable('get_iter') :method
    .tailcall self.'Iterator'()
.end

.sub '' :vtable('get_string') :method
    $S0 = self.'Str'()
    .return ($S0)
.end

.sub '' :vtable('increment') :method
    $P0 = self.'succ'()
    'infix:='(self, $P0)
    .return(self)
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
