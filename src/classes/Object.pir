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

# A few useful constants (just here so they're available going forward).
.const int SIG_ELEM_SLURPY_POS                  = 8
.const int SIG_ELEM_SLURPY_NAMED                = 16
.const int SIG_ELEM_SLURPY                      = 56
.const int SIG_ELEM_INVOCANT                    = 64
.const int SIG_ELEM_MULTI_INVOCANT              = 128
.const int SIG_ELEM_INVOCANT_AND_MULTI_INVOCANT = 192
.const int SIG_ELEM_IS_RW                       = 256
.const int SIG_ELEM_IS_COPY                     = 512
.const int SIG_ELEM_IS_REF                      = 1024
.const int SIG_ELEM_IS_OPTIONAL                 = 2048

=head2 Methods

=over 4

=item clone()

Returns a copy of the object.

NOTE: Don't copy what this method does; it's a tad inside-out. We should be
overriding the clone vtable method to call .clone() really. But if we do that,
we can't current get at the Object PMC's clone method, so for now we do it
like this.

=cut

.macro fixup_cloned_sub(orig, copy)
    .local pmc tmp, tmp2
    tmp = getprop '$!signature', .orig
    if null tmp goto sub_fixup_done
    setprop .copy, '$!signature', tmp
    .local pmc oclass, sclass
    oclass = typeof .orig
    sclass = get_class ['Sub']
    $I0 = issame oclass, sclass
    if $I0 goto sub_fixup_done
    tmp = getattribute .orig, ['Sub'], 'proxy'
    tmp = getprop '$!real_self', tmp
    if null tmp goto sub_fixup_done
    tmp2 = getattribute .copy, ['Sub'], 'proxy'
    setprop tmp2, '$!real_self', tmp
  sub_fixup_done:
.endm

.namespace ['Perl6Object']
.sub 'clone' :method
    .param pmc new_attrs :slurpy :named

    # Make a clone.
    .local pmc result
    self = deobjectref self
    result = clone self

    # Set any new attributes.
    .local pmc p6meta, parrotclass, attributes, it
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    parrotclass = p6meta.'get_parrotclass'(result)
    if null parrotclass goto attrinit_done
    attributes = inspect parrotclass, 'attributes'
    it = parrotclass.'attriter'()
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

    .fixup_cloned_sub(self, result)
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
.sub '' :method('list')
    $P0 = new ['List']
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
    $P0 = new ['Perl6Array']
    $P0.'!STORE'(self)
    .return ($P0)
.end

=item Hash()

=cut

.namespace ['Perl6Object']
.sub 'Hash' :method
    $P0 = new ['Perl6Hash']
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
    $P0 = root_new ['parrot';'Perl6Scalar'], self
    .return ($P0)
.end

=item Str()

Return a string representation of the invocant.  Default is
the object's type and address.

=cut

.namespace ['Perl6Object']
.sub 'Str' :method
    $P0 = root_new ['parrot';'ResizableStringArray']
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
    .param pmc candidate
    .param pmc posargs         :slurpy
    .param pmc attrinit        :slurpy :named

    $I0 = isa candidate, 'Whatever'
    unless $I0 goto have_candidate
    candidate = self.'CREATE'('P6opaque')
  have_candidate:

    .tailcall self.'BUILDALL'(candidate, attrinit, posargs)
.end


.sub 'BUILD' :method
    .param pmc attrinit        :slurpy :named

    .local pmc p6meta, parentproto, parrotclass, attributes, it
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    parentproto = find_caller_lex '$CLASS'
    parrotclass = p6meta.'get_parrotclass'(parentproto)
    attributes = inspect parrotclass, 'attributes'
    it = parrotclass.'attriter'()
  attrinit_loop:
    unless it goto attrinit_done
    .local string attrname, keyname
    .local pmc attr, attrhash
    attrname = shift it
    attr = getattribute self, parrotclass, attrname
    attrhash = attributes[attrname]
    $I0 = index attrname, '!'
    if $I0 < 0 goto attrinit_loop
    inc $I0
    keyname = substr attrname, $I0
    $P0 = attrinit[keyname]
    unless null $P0 goto attrinit_assign
    $P0 = attrhash['init_value']
    if null $P0 goto attrinit_loop
    $P0 = $P0(self, attr)
  attrinit_assign:
    'infix:='(attr, $P0)
    goto attrinit_loop
  attrinit_done:
    .return (self)
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
    .lex '$CLASS', parentproto
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
    $P1 = find_method parentproto, 'BUILD'
    $P1(candidate, $P0 :flat :named)
    goto parents_loop
  posargs_done:
    $P1 = find_method parentproto, 'BUILD'
    $P1(candidate, attrinit :flat :named)
    goto parents_loop
  parents_done:
    .return (candidate)
.end


=item CREATE()

Create a candidate object of the type given by the invocant.

XXX This had probably best really just tailcall .^CREATE; move this stuff later.

=cut

.sub 'CREATE' :method
    .param string repr    :optional
    .param int have_repr  :opt_flag

    # Default to P6opaque.
    if have_repr goto repr_done
    repr = 'P6opaque'
  repr_done:

    # If we already have an "example" of how this representation looks for the
    # current class, just clone it.
    .local pmc how
    .local string repr_lookup
    how = self.'HOW'()
    repr_lookup = concat 'repr_', repr
    $P0 = getprop repr_lookup, how
    if null $P0 goto no_example
    $P0 = clone $P0
    .return ($P0)

  no_example:
    if repr != 'P6opaque' goto unknown_repr

    # P6opaque. Create example.
    .local pmc p6meta, parrot_class, example
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    parrot_class = p6meta.'get_parrotclass'(self)
    example = new parrot_class

    # Set up attribute containers along with their types and any other
    # traits. (We could do this while constructing the class too, but
    # that would have the unfortunate side-effect of increased startup
    # cost, which we're currently wanting to avoid. Let's see how far
    # we can go while doing the init here.)
    .local pmc parents, cur_class, attributes, class_it, it, traits
    parents = inspect parrot_class, 'all_parents'
    class_it = iter parents
  classinit_loop:
    unless class_it goto classinit_loop_end
    cur_class = shift class_it
    attributes = inspect cur_class, 'attributes'
    it = cur_class.'attriter'()
  attrinit_loop:
    unless it goto attrinit_done
    .local string attrname
    .local pmc attrhash, itypeclass
    attrname = shift it
    $I0 = index attrname, '!'
    if $I0 < 0 goto attrinit_loop
    attrhash = attributes[attrname]
    itypeclass = attrhash['itype']
    $S0 = substr attrname, 0, 1
    unless null itypeclass goto attrinit_itype
    if $S0 == '@' goto attrinit_array
    if $S0 == '%' goto attrinit_hash
    $P0 = get_root_namespace ['parrot';'Perl6Scalar']
    itypeclass = get_class $P0
    goto attrinit_itype
  attrinit_array:
    itypeclass = get_class ['Perl6Array']
    goto attrinit_itype
  attrinit_hash:
    itypeclass = get_class ['Perl6Hash']
  attrinit_itype:
    .local pmc attr
    attr = new itypeclass
    setattribute example, cur_class, attrname, attr
    traits = attrhash['traits']
    if null traits goto traits_done
    $P0 = getprop 'metaclass', cur_class
    if null $P0 goto traits_done
    traits(attr, $P0)
  traits_done:
    goto attrinit_loop
  attrinit_done:
    # Only go to next class if we didn't already reach the top of the Perl 6
    # hierarchy.
    $S0 = cur_class
    if $S0 != 'Perl6Object' goto classinit_loop
  classinit_loop_end:
    
    # Turn the example from a Parrot Object into a p6opaque; we'll ideally be
    # able to create it as one in the future.
    transform_to_p6opaque example

    # Stash the example, clone it and we're done.
    setprop how, repr_lookup, example
    $P0 = clone example
    .return ($P0)

  unknown_repr:
    'die'('Unknown representation: ', repr)
.end


=item new()

Create a new object having the same class as the invocant.

=cut

.sub 'new' :method
    .param pmc posargs         :slurpy
    .param pmc attrinit        :slurpy :named
    .local pmc candidate
    candidate = self.'CREATE'('P6opaque')
    .tailcall self.'bless'(candidate, posargs :flat, attrinit :flat :named)
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


=item !STORE(source)

Store C<source> into C<self>, performing type checks
as needed.  (This method is listed with the other public
methods simply because I expect it may switch to public
in the future.)

=cut

.sub '!STORE' :method :subid('Object::!STORE')
    .param pmc source
    source = '!CALLMETHOD'('Scalar', source)
    $I0 = defined source
    unless $I0 goto do_store
    .local pmc type
    getprop type, 'type', self
    if null type goto do_store
    $I0 = isa type, 'NameSpace'
    if $I0 goto do_store
    $I0 = type.'ACCEPTS'(source)
    unless $I0 goto err_type
  do_store:
    source = deobjectref source
    eq_addr self, source, store_done
    copy self, source
    .fixup_cloned_sub(source, self)
  store_done:
    .return (self)

  err_type:
    $S0 = '!make_type_fail_message'('Assignment', source, type)
    'die'($S0)
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


=item !rebox

If we end up with an object that isn't a subclass of Perl6Object
(e.g., a parrot Integer, Float, or Str), the C<!rebox> method will
adjust it.

=cut

.namespace ['Perl6Object']
.sub '!rebox' :method
    $I0 = isa self, ['Perl6Object']
    if $I0 goto done
    .local pmc p6meta
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    $P0 = self.'WHAT'()
    $P0 = p6meta.'get_parrotclass'($P0)
    $P0 = new $P0
    assign $P0, self
    copy self, $P0
  done:
.end
    

=item !.?

Helper method for implementing the .? operator. Calls at most one matching
method, and returns undef if there are none.

=cut

.sub '!.?' :method
    .param pmc methods
    .param string method_name
    .param pmc pos_args     :slurpy
    .param pmc named_args   :slurpy :named

    # If we were already given a list, just check it's non-empty and use that.
    if null methods goto no_list
  retry:
    $I0 = elements methods
    unless $I0 goto error
    $P0 = methods[0]
    $I0 = isa $P0, 'Perl6MultiSub'
    unless $I0 goto ready_to_call
    $P0 = $P0.'find_possible_candidates'(self, pos_args :flat, named_args :named :flat)
    $P0 = $P0[0]
    unless null $P0 goto ready_to_call
    $P0 = shift methods
    goto retry
  ready_to_call:
    .tailcall self.$P0(pos_args :flat, named_args :named :flat)

    # If there's no list, use .can to try and get us one.
  no_list:
    $P0 = self.'HOW'()
    $P0 = $P0.'can'(self, method_name)
    unless $P0 goto error
    push_eh check_error
    .tailcall $P0(self, pos_args :flat, named_args :named :flat)
  check_error:
    .local pmc exception
    .get_results (exception)
    pop_eh
    if exception == "No candidates found to invoke" goto error
    rethrow exception

  error:
    .tailcall '!FAIL'('Undefined value returned by invocation of undefined method')
.end


=item !.*

Helper method for implementing the .* operator. Calls one or more matching
methods.

=cut

.sub '!.*' :method
    .param pmc methods
    .param string method_name
    .param pmc pos_args     :slurpy
    .param pmc named_args   :slurpy :named

    # Set up result list.
    .local pmc result_list
    $P0 = get_hll_global 'list'
    result_list = $P0()

    # Get all possible methods, unless we already were given a list.
    unless null methods goto have_methods
    $P0 = self.'HOW'()
    methods = $P0.'can'(self, method_name)
    unless methods goto it_loop_end
  have_methods:

    # Call each method, expanding out any multis along the way.
    .local pmc pos_res, named_res, cap, it, multi_it, cur_meth
    it = iter methods
  it_loop:
    unless it goto it_loop_end
    cur_meth = shift it
    $I0 = isa cur_meth, 'Perl6MultiSub'
    if $I0 goto is_multi
    push_eh check_error
    (pos_res :slurpy, named_res :named :slurpy) = cur_meth(self, pos_args :flat, named_args :named :flat)
    pop_eh
    cap = 'prefix:\\'(pos_res :flat, named_res :flat :named)
    push result_list, cap
    goto it_loop
  is_multi:
    $P0 = cur_meth.'find_possible_candidates'(self, pos_args :flat, named_args :named :flat)
    multi_it = iter $P0
  multi_it_loop:
    unless multi_it goto it_loop
    cur_meth = shift multi_it
    (pos_res :slurpy, named_res :named :slurpy) = cur_meth(self, pos_args :flat, named_args :named :flat)
    cap = 'prefix:\\'(pos_res :flat, named_res :flat :named)
    push result_list, cap
    goto multi_it_loop
  check_error:
    .local pmc exception
    .get_results (exception)
    pop_eh
    if exception == "No candidates found to invoke" goto it_loop
    rethrow exception
  it_loop_end:

    .return (result_list)
.end


=item !.+

Helper method for implementing the .+ operator. Calls one or more matching
methods, dies if there are none.

=cut

.sub '!.+' :method
    .param pmc methods
    .param string method_name
    .param pmc pos_args     :slurpy
    .param pmc named_args   :slurpy :named

    # Use !.* to produce a (possibly empty) list of result captures.
    .local pmc result_list
    result_list = self.'!.*'(methods, method_name, pos_args :flat, named_args :flat :named)

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


=item !.^

Helper for doing calls on the metaclass.

=cut

.sub '!.^' :method
    .param pmc method
    .param string method_name
    .param pmc pos_args     :slurpy
    .param pmc named_args   :slurpy :named

    # Get the HOW or the object and do the call on that.
    .local pmc how
    how = self.'HOW'()
    if null method goto by_name
    .tailcall '!dispatch_method_indirect'(how, method, self, pos_args :flat, named_args :flat :named)
  by_name:
    .tailcall how.method_name(self, pos_args :flat, named_args :flat :named)
.end


=item !.=

Helper for doing .= calls.

=cut

.sub '!.=' :method
    .param pmc method
    .param string method_name
    .param pmc pos_args     :slurpy
    .param pmc named_args   :slurpy :named

    # Get result and assign it to self. (XXX Also while $/ is not accessed
    # as a context var properly, need to cheat bit with that to keep
    # some other things happy.)
    $P0 = find_lex_skip_current '$/'
    .lex '$/', $P0
    if null method goto by_name
    $I0 = elements method
    if $I0 != 1 goto too_many_methods
    method = method[0]
    ($P0) = self.method(pos_args :flat, named_args :flat :named)
    goto called
  by_name:
    ($P0) = self.method_name(pos_args :flat, named_args :flat :named)
  called:
    $P1 = getinterp
    $P1 = $P1['lexpad'; 1]
    if null $P1 goto done
    $P1['$/'] = $P0
  done:
    .tailcall 'infix:='(self, $P0)
  too_many_methods:
    'die'('.= indirect form can only be used to supply a single method')
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

.sub '' :vtable('get_integer') :method
    .tailcall self.'Int'()
.end

.sub '' :vtable('get_iter') :method
    .tailcall self.'Iterator'()
.end

.sub '' :vtable('get_string') :method
    $S0 = self.'Str'()
    .return ($S0)
.end

.sub '' :vtable('get_number') :method
    $N0 = self.'Num'()
    .return ($N0)
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
