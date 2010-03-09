## $Id$

=head1 TITLE

Mu - Perl 6 Mu class

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
.const int SIG_ELEM_IS_PARCEL                   = 1024
.const int SIG_ELEM_IS_OPTIONAL                 = 2048
.const int SIG_ELEM_IS_CAPTURE                  = 32768

=head2 Methods

=over 4

=item clone()

Returns a copy of the object.

NOTE: Don't copy what this method does; it's a tad inside-out. We should be
overriding the clone vtable method to call .clone() really. But if we do that,
we can't current get at the Object PMC's clone method, so for now we do it
like this.

=cut

.namespace ['Mu']
.sub 'clone' :method
    .param pmc new_attrs :slurpy :named

    # Make a clone.
    .local pmc result
    self = deobjectref self
    result = clone self

    # Set any new attributes.
    .local pmc p6meta, parrotclass, attributes, it
    p6meta = get_hll_global ['Mu'], '$!P6META'
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
    '&infix:<=>'($P0, $P1)
    goto attrinit_loop
  attrinit_done:

    .return (result)
.end

=item defined()

=cut

.sub 'defined' :method
    $P0 = get_hll_global ['Bool'], 'True'
    .return ($P0)
.end

=back

=head2 Object constructor methods

=over 4

=cut

.namespace ['Mu']
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

    .local pmc p6meta, parentproto, how, parrotclass, attributes, it
    p6meta = get_hll_global ['Mu'], '$!P6META'
    parentproto = find_caller_lex '$CLASS'
    how = parentproto.'HOW'()
    parrotclass = getattribute how, 'parrotclass'
    attributes = getattribute how, '$!attributes'
    if null attributes goto attrinit_done
    it = iter attributes
  attrinit_loop:
    unless it goto attrinit_done
    .local string attrname, keyname
    .local pmc attr_info, attr
    attr_info = shift it
    attrname = attr_info.'name'()
    attr = getattribute self, parrotclass, attrname
    $I0 = index attrname, '!'
    if $I0 < 0 goto attrinit_loop
    inc $I0
    keyname = substr attrname, $I0
    $P0 = attrinit[keyname]
    unless null $P0 goto attrinit_assign
    $P0 = attr_info.'build'()
    if null $P0 goto attrinit_loop
    $I0 = defined $P0
    unless $I0 goto attrinit_loop
    $P0 = $P0(self, attr)
  attrinit_assign:
    '&infix:<=>'(attr, $P0)
    goto attrinit_loop
  attrinit_done:
    .return (self)
.end


.sub 'BUILDALL' :method
    .param pmc candidate
    .param pmc attrinit
    .param pmc posargs

    .local int num_pos_args
    .local int cur_pos_arg
    num_pos_args = elements posargs

    .include 'iterator.pasm'
    .local pmc p6meta, parents, it, build_method
    p6meta = get_hll_global ['Mu'], '$!P6META'
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
    build_method = find_method_null_ok parentproto, 'BUILD'
    if null build_method goto parents_loop
    .lex '$CLASS', parentproto
    # Look through posargs for a corresponding protoobject
    # with a WHENCE property.  If found, that WHENCE property
    # is used as the arguments to the parent class BUILD.
    .local pmc argproto
    cur_pos_arg = 0
  posargs_loop:
    if cur_pos_arg >= num_pos_args goto posargs_done
    argproto = posargs[cur_pos_arg]
    inc cur_pos_arg
    $P1 = argproto.'HOW'()
    ne_addr $P0, $P1, posargs_loop
    $P0 = argproto.'WHENCE'()
    if null $P0 goto posargs_done
    build_method(candidate, $P0 :flat :named)
    goto parents_loop
  posargs_done:
    build_method(candidate, attrinit :flat :named)
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
    p6meta = get_hll_global ['Mu'], '$!P6META'
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
    $S0 = substr attrname, 0, 1
    if $S0 == '@' goto attrinit_array
    if $S0 == '%' goto attrinit_hash
    .local pmc attr
    attr = new ['Perl6Scalar']
    setprop attr, 'scalar', attr
    goto attrinit_rw
  attrinit_array:
    attr = new ['Array']
    goto attrinit_rw
  attrinit_hash:
    attr = '&CREATE_HASH_LOW_LEVEL'()
  attrinit_rw:
    setprop attr, 'rw', attr
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
    if $S0 != 'Mu' goto classinit_loop
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
    $P1 = '&prefix:<!>'($P0)
    .return ($P1)
.end


=item !STORE(source)

Store C<source> into C<self>, performing type checks
as needed.  (This method is listed with the other public
methods simply because I expect it may switch to public
in the future.)

=cut

.sub '!STORE' :method :subid('Mu::!STORE')
    .param pmc source

    # Get hold of the source object to assign.
    $I0 = can source, '!FETCH'
    if $I0 goto source_fetch
    source = deobjectref source
    source = new ['ObjectRef'], source
    goto have_source
  source_fetch:
    source = source.'!FETCH'()
    source = deobjectref source
  have_source:

    # If source and destination are the same, we're done.
    eq_addr self, source, store_done
    
    # Need a type-check?
    .local pmc type
    type = getprop 'type', self
    if null type goto type_check_done
    $I0 = type.'ACCEPTS'(source)
    unless $I0 goto type_check_failed
  
    # All is well - do the assignment.
  type_check_done:
    copy self, source
    
  store_done:
    .return (self)

  type_check_failed:
    # XXX TODO: Awesomize this error.
    '&die'('Type check failed for assignment')
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

=head2 Vtable functions

=cut

.namespace ['Mu']
.sub '' :vtable('concatenate') :method
    .param pmc arg
    .param pmc dest
    $S0 = self
    $S1 = arg
    $S2 = concat $S0, $S1
    $P2 = box $S2
    .return ($P2)
.end

.sub '' :vtable('concatenate_str') :method
    .param string arg
    .param pmc dest
    $S0 = self
    $S1 = concat $S0, arg
    $P1 = box $S1
    .return ($P1)
.end

.sub '' :vtable('decrement') :method
    $P0 = self.'pred'()
    '&infix:<=>'(self, $P0)
    .return(self)
.end

.sub '' :vtable('defined') :method
    $I0 = self.'defined'()
    .return ($I0)
.end

.sub '' :vtable('elements') :method
    $I0 = self.'elems'()
    .return ($I0)
.end

.sub '' :vtable('get_bool') :method
    $I0 = self.'Bool'()
    .return ($I0)
.end

.sub '' :vtable('get_integer') :method
    .tailcall self.'Int'()
.end

.sub '' :vtable('get_iter') :method
    $P0 = self.'iterator'()
    $P1 = get_hll_global 'ParrotIter'
    $P1 = $P1.'new'($P0)
    .return ($P1)
.end

.sub '' :vtable('get_string') :method
    $S0 = self.'Str'()
    .return ($S0)
.end

.sub '' :vtable('get_string_keyed_int') :method
    .param int i
    $S0 = self.'postcircumfix:<[ ]>'(i)
    .return ($S0)
.end

.sub '' :vtable('get_number') :method
    $N0 = self.'Num'()
    .return ($N0)
.end

.sub '' :vtable('increment') :method
    $P0 = self.'succ'()
    '&infix:<=>'(self, $P0)
    .return (self)
.end

.sub '' :vtable('shift_pmc') :method
    .tailcall self.'shift'()
.end

.sub 'list' :method
    .tailcall '&infix:<,>'(self)
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
