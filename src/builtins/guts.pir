## $Id$

=head1 NAME

src/builtins/guts.pir - subs that are part of the internals, not for users

=head1 SUBS

=over 4

=item !EXPORT(symbols, from :named('from') [, to :named('to')] )

Export symbols in namespace C<from> to the namespace given by C<to>.
If C<to> isn't given, then exports into the HLL global namespace.
This function differs somewhat from Parrot's C<Exporter> PMC in that
it understands how to properly merge C<MultiSub> PMCs.

=cut

.namespace []
.sub '!EXPORT'
    .param string symbols
    .param pmc from            :named('from')
    .param pmc to              :named('to') :optional
    .param int has_to          :opt_flag

    if has_to goto have_to
    to = get_hll_namespace
  have_to:

    .local pmc list
    list = split ',', symbols
  list_loop:
    unless list goto list_end
    .local string symbol
    .local pmc value
    symbol = shift list
    value = from[symbol]
    $I0 = isa value, 'MultiSub'
    unless $I0 goto store_value
    $P0 = to[symbol]
    if null $P0 goto store_value
    $I0 = isa $P0, 'MultiSub'
    unless $I0 goto err_type_conflict
    $I0 = elements $P0
    splice $P0, value, $I0, 0
    goto list_loop
  store_value:
    to[symbol] = value
    goto list_loop
  list_end:
    .return ()

  err_type_conflict:
    $S0 = concat "Unable to add Multisub '", symbol
    $S0 .= "' to existing value"
    die $S0
.end


=item !CALLMETHOD('method', obj)

Invoke a method on a possibly foreign object.  If the object
supports the requested method, we use it, otherwise we assume
the object is foreign and try using the corresponding method
from C<Any>.

=cut

.namespace []
.sub '!CALLMETHOD'
    .param string method
    .param pmc obj
    $I0 = isa obj, 'Perl6Scalar'
    if $I0 goto any_method
    $I0 = can obj, method
    unless $I0 goto any_method
    .tailcall obj.method()
  any_method:
    .local pmc anyobj
    anyobj = get_global '$!ANY'
    unless null anyobj goto any_method_1
    anyobj = new 'Any'
    set_global '$!ANY', anyobj
  any_method_1:
    $P0 = find_method anyobj, method
    .tailcall obj.$P0()
.end


=item !VAR

Helper function for implementing the VAR and .VAR macros.

=cut

.sub '!VAR'
    .param pmc variable
    $I0 = isa variable, 'Perl6Scalar'
    unless $I0 goto nothing
    $P0 = new 'MutableVAR', variable
    .return ($P0)
  nothing:
    .return (variable)
.end


=item !SAMETYPE_EXACT

Takes two types and returns true if they match exactly (not accounting for any
subtyping relations, etc).

=cut

.sub '!SAMETYPE_EXACT'
    .param pmc t1
    .param pmc t2

    # If they have equal address, obviously the same.
    .local pmc t1meta, t2meta
    t1meta = t1.'HOW'()
    t2meta = t2.'HOW'()
    eq_addr t1meta, t2meta, same

    # If they are junctions, compare inside them recursively.
    $I0 = isa t1, 'Junction'
    unless $I0 goto not_junc
    $I1 = isa t2, 'Junction'
    unless $I0 == $I1 goto not_junc
    .local pmc j1, j2
    .local int max, i
    j1 = t1.'!eigenstates'()
    j2 = t1.'!eigenstates'()
    max = elements j1
    i = 0
  junc_loop:
    if i >= max goto junc_loop_end
    $P0 = j1[i]
    $P1 = j2[i]
    $I0 = '!SAMETYPE_EXACT'($P0, $P1)
    unless $I0 goto not_same
    inc i
    goto junc_loop
  junc_loop_end:
  not_junc:

  not_same:
    .return(0)
  same:
    .return (1)
.end


=item !CREATE_SUBSET_TYPE

Creates a subset type. Basically, we make an anonymous subclass of the
original type, attach the refinement and override ACCEPTS. We also chase up
to find a real, non-subtype and stash that away for fast access later.

=cut

.sub '!CREATE_SUBSET_TYPE'
    .param pmc refinee
    .param pmc refinement

    .local pmc p6meta
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'

    # Check if the refinee is a refinement type itself; if so, get the real
    # base type we're refining.
    .local pmc real_type, real_type_pc
    real_type = getprop 'subtype_realtype', refinee
    unless null $P0 goto got_real_type
    real_type = refinee
  got_real_type:

    # Create subclass, register it with the real type's proto.
    .local pmc parrot_class, subset
    parrot_class = p6meta.'get_parrotclass'(refinee)
    subset = subclass parrot_class
    p6meta.'register'(subset, 'protoobject' => real_type)

    # Override accepts.
    .local pmc parrotclass
    .const 'Sub' $P0 = "!SUBTYPE_ACCEPTS"
    subset.'add_method'('ACCEPTS', $P0)

    # Instantiate it - we'll only ever create this one instance.
    subset = subset.'new'()

    # Mark it a subtype and stash away real type, refinee  and refinement.
    setprop subset, 'subtype_realtype', real_type
    setprop subset, 'subtype_refinement', refinement
    setprop subset, 'subtype_refinee', refinee

    .return (subset)
.end
.sub "!SUBTYPE_ACCEPTS" :anon :method
    .param pmc topic

    # Get refinement and check against that.
    .local pmc refinement
    refinement = getprop 'subtype_refinement', self
    $P0 = refinement(topic)
    unless $P0 goto false

    # Recurse up the tree.
    .local pmc refinee
    refinee = getprop 'subtype_refinee', self
    $P0 = refinee.'ACCEPTS'(topic)
    unless $P0 goto false

  true:
    $P0 = get_hll_global ['Bool'], 'True'
    .return ($P0)
  false:
    $P0 = get_hll_global ['Bool'], 'False'
    .return ($P0)
.end


=item !TOPERL6MULTISUB

At the moment, we don't have the abilility to have Parrot use our own MultiSub
type, nor are we ready to (because built-ins need to get Perl 6 signatures
first). So for now we just transform multis in user code like this.

=cut

.sub '!TOPERL6MULTISUB'
    .param pmc sub

    # Look up what's currently installed in the namespace for this sub; if it
    # is already a Perl6MultiSub, leave it.
    .local pmc namespace, current_thing
    .local string name
    namespace = sub.'get_namespace'()
    name = sub
    current_thing = namespace[name]
    if null current_thing goto error
    $S0 = typeof current_thing
    if $S0 == 'MultiSub' goto not_perl6_multisub
    .return()
    # It's not a Perl6MultiSub, create one and put contents into it.
  not_perl6_multisub:
    .local pmc p6multi, sub_iter
    p6multi = new 'Perl6MultiSub'
    sub_iter = iter current_thing
  iter_loop:
    unless sub_iter goto iter_loop_end
    $P0 = shift sub_iter
    push p6multi, $P0
    goto iter_loop
  iter_loop_end:

    # If the namespace is associated with a class, need to remove the method
    # entry in that; inserting the new multi into the namespace will then
    # also add it back to the class.
    .local pmc class
    class = get_class namespace
    if null class goto class_done
    class.'remove_method'(name)
    $I0 = isa class, 'Class'
    if $I0 goto class_done
    ##  class isn't really a Class, it's (likely) a Role
    class.'add_method'(name, p6multi)
  class_done:

    # Make new namespace entry.
    namespace[name] = p6multi
    .return()

  error:
    'die'('Sub lookup failed')
.end


=item !UNIT_START

=cut

.sub '!UNIT_START'
    .param pmc unitmain
    .param pmc args

    args = 'list'(args)
    if args goto start_main
    .tailcall unitmain()

  start_main:
    ## We're running as main program
    ## Remove program argument (0) and set up @ARGS global
    $P0 = shift args
    args = args.'Array'()
    set_hll_global '@ARGS', args
    ## run unitmain
    .local pmc result, MAIN
    result = unitmain()
    ## if there's a MAIN sub in unitmain's namespace, run it also
    $P0 = unitmain.'get_namespace'()
    MAIN = $P0['MAIN']
    if null MAIN goto done
    args = get_hll_global '@ARGS'
    result = MAIN(args :flat)
  done:
    .return (result)
.end


=item !capture

Combine slurpy positional and slurpy named args into a list.
Note that original order may be lost -- that's the nature
of captures.

=cut

.sub '!capture'
    .param pmc args            :slurpy
    .param pmc options         :slurpy :named
    unless options goto done
    .local pmc it
    it = iter options
  iter_loop:
    unless it goto done
    $S0 = shift it
    $P0 = options[$S0]
    $P0 = 'infix:=>'($S0, $P0)
    push args, $P0
    goto iter_loop
  done:
    .tailcall args.'list'()
.end


=item !meta_create(type, name, also)

Create a metaclass object for C<type> with the given C<name>.
This simply creates a handle on which we can hang methods, attributes,
traits, etc. -- the class itself isn't created until the class
is composed (see C<!meta_compose> below).

=cut

.sub '!meta_create'
    .param string type
    .param string name
    .param int also

    .local pmc nsarray
    $P0 = compreg 'Perl6'
    nsarray = $P0.'parse_name'(name)

    if type == 'package' goto package
    if type == 'module' goto package
    if type == 'class' goto class
    if type == 'grammar' goto class
    if type == 'role' goto role
    'die'("Unsupported package declarator ", type)

  package:
    $P0 = get_hll_namespace nsarray
    .return ($P0)

  class:
    .local pmc metaclass, ns
    ns = get_hll_namespace nsarray
    if also goto is_also
    metaclass = newclass ns
    $P0 = box type
    setprop metaclass, 'pkgtype', $P0
    .return (metaclass)
  is_also:
    metaclass = get_class ns
    .return (metaclass)

  role:
    .local pmc info, metarole
    info = new 'Hash'
    $P0 = nsarray[-1]
    info['name'] = $P0
    info['namespace'] = nsarray
    metarole = new 'Role', info
    nsarray = clone nsarray
    $S0 = pop nsarray
    set_hll_global nsarray, $S0, metarole
    .return (metarole)
.end


=item !meta_compose(Class metaclass)

Compose the class.  This includes resolving any inconsistencies
and creating the protoobjects.

=cut

.sub '!meta_compose' :multi(['Class'])
    .param pmc metaclass
    .local pmc p6meta
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'

    # Parrot handles composing methods into roles, but we need to handle the
    # attribute composition ourselves.
    .local pmc roles, roles_it
    roles = inspect metaclass, 'roles'
    roles_it = iter roles
  roles_it_loop:
    unless roles_it goto roles_it_loop_end
    $P0 = shift roles_it
    '!compose_role_attributes'(metaclass, $P0)
    goto roles_it_loop
  roles_it_loop_end:

    # Create proto-object with default parent being Any or Grammar.
    $S0 = 'Any'
    $P0 = getprop 'pkgtype', metaclass
    if null $P0 goto no_pkgtype
    if $P0 != 'grammar' goto register
    $S0 = 'Grammar'
  register:
    .tailcall p6meta.'register'(metaclass, 'parent'=>$S0)
  no_pkgtype:
.end


=item !meta_compose()

Default meta composer -- does nothing.


=cut

.sub '!meta_compose' :multi()
    .param pmc metaclass
    # Currently, nothing to do.
.end


=item !meta_trait(metaclass, type, name)

Add a trait with the given C<type> and C<name> to C<metaclass>.

=cut

.sub '!meta_trait'
    .param pmc metaclass
    .param string type
    .param string name

    if type == 'trait_auxiliary:is' goto is
    if type == 'trait_auxiliary:does' goto does
    'die'("Unknown trait auxiliary ", type)

  is:
    ##  get the (parrot)class object associated with name
    $P0 = compreg 'Perl6'
    $P0 = $P0.'parse_name'(name)
    $P0 = get_hll_namespace $P0
    $P0 = get_class $P0

    ##  add it as parent to metaclass
    metaclass.'add_parent'($P0)
    .return ()

  does:
    ##  get the role to be composed
    $P0 = compreg 'Perl6'
    $P0 = $P0.'parse_name'(name)
    $S0 = pop $P0
    $P0 = get_hll_global $P0, $S0

    ##  add it to the class.
    metaclass.'add_role'($P0)
.end


=item !meta_attribute(metaclass, name, itype [, 'type'=>type] )

Add attribute C<name> to C<metaclass> with the given C<itype>
and C<type>.

=cut

.sub '!meta_attribute'
    .param pmc metaclass
    .param string name
    .param string itype        :optional
    .param int has_itype       :opt_flag
    .param pmc attr            :slurpy :named

    # twigil handling
    .local string twigil
    twigil = substr name, 1, 1
    if twigil == '.' goto twigil_public
    if twigil == '!' goto twigil_done
    substr name, 1, 0, '!'
    goto twigil_done
  twigil_public:
    substr name, 1, 1, '!'
  twigil_done:

    $P0 = metaclass.'attributes'()
    $I0 = exists $P0[name]
    if $I0 goto attr_exists
    metaclass.'add_attribute'(name)
    $P0 = metaclass.'attributes'()
  attr_exists:

    .local pmc attrhash, it
    attrhash = $P0[name]

    # Set any itype for the attribute.
    unless has_itype goto itype_done
    attrhash['itype'] = itype
  itype_done:

    # and set any other attributes that came in via the slurpy hash
    it = iter attr
  attr_loop:
    unless it goto attr_done
    $S0 = shift it
    $P0 = attr[$S0]
    attrhash[$S0] = $P0
    goto attr_loop
  attr_done:

    .const 'Sub' handles = '!handles'
    $P0 = attr['traitlist']
    if null $P0 goto traitlist_done
    it = iter $P0
  traitlist_loop:
    unless it goto traitlist_done
    .local pmc trait
    trait = shift it
    $S0 = trait[0]
    if $S0 != 'trait_verb:handles' goto traitlist_loop
    .local pmc handles_it
    $P0 = trait[1]
    $P0 = 'list'($P0)
    handles_it = iter $P0
  handles_loop:
    unless handles_it goto handles_done
    $P0 = clone handles
    $P1 = box name
    setprop $P0, 'attrname', $P1
    $P1 = shift handles_it
    setprop $P0, 'methodname', $P1
    $S1 = $P1
    metaclass.'add_method'($S1, $P0)
    goto handles_loop
  handles_done:
    goto traitlist_loop
  traitlist_done:
.end


.sub '!handles' :method
    .param pmc args            :slurpy
    .param pmc options         :slurpy :named
    .local pmc method, attribute
    $P0 = getinterp
    method = $P0['sub']
    $P1 = getprop 'attrname', method
    $S1 = $P1
    attribute = getattribute self, $S1
    $P1 = getprop 'methodname', method
    $S1 = $P1
    .tailcall attribute.$S1(args :flat, options :flat :named)
.end


=item !sub_trait(sub, type, trait, arg?)

=cut

.sub '!sub_trait'
    .param pmc block
    .param string type
    .param string trait
    .param pmc arg             :optional
    .param int has_arg         :opt_flag

    if has_arg goto have_arg
    null arg
  have_arg:

    $S0 = concat '!sub_trait_', trait
    $P0 = find_name $S0
    if null $P0 goto done
    $P0(trait, block, arg)
  done:
.end


=item !sub_trait_default(trait, block, arg)

Sets the default trait, which marks a multi candidate as the default choice
in an ambiguous multiple dispatch.

=cut

.sub '!sub_trait_default'
    .param string trait
    .param pmc block
    .param pmc arg
    $P0 = new 'Integer'
    $P0 = 1
    setprop block, 'default', $P0
.end


=item !sub_trait_export(trait, block, arg)

=cut

.sub '!sub_trait_export'
    .param string trait
    .param pmc block
    .param pmc arg

    .local string blockname
    blockname = block
    .local pmc blockns, exportns
    blockns = block.'get_namespace'()
    exportns = blockns.'make_namespace'('EXPORT')
    if null arg goto arg_done
    .local pmc it
    arg = arg.'list'()
    it = iter arg
  arg_loop:
    unless it goto arg_done
    .local pmc tag, ns
    tag = shift it
    $I0 = isa tag, ['Perl6Pair']
    unless $I0 goto arg_loop
    $S0 = tag.'key'()
    ns = exportns.'make_namespace'($S0)
    ns[blockname] = block
    goto arg_loop
  arg_done:
    ns = exportns.'make_namespace'('ALL')
    ns[blockname] = block
.end


=item !compose_role_attributes(class, role)

Helper method to compose the attributes of a role into a class.

=cut

.sub '!compose_role_attributes'
    .param pmc class
    .param pmc role

    .local pmc role_attrs, class_attrs, ra_iter
    .local string cur_attr
    role_attrs = inspect role, "attributes"
    class_attrs = inspect class, "attributes"
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
  merge:
    goto ra_iter_loop
  ra_iter_loop_end:
.end


=item !keyword_role(name)

Internal helper method to create a role.

=cut

.sub '!keyword_role'
    .param string name
    .local pmc info, role

    # Need to make sure it ends up attached to the right namespace.
    .local pmc ns
    ns = split '::', name
    name = ns[-1]
    info = new 'Hash'
    info['name'] = name
    info['namespace'] = ns

    # Create role.
    role = new 'Role', info

    # Stash in namespace.
    $I0 = elements ns
    dec $I0
    ns = $I0
    set_hll_global ns, name, role

    .return(role)
.end


=item !keyword_enum(name)

Internal helper method to create an enum class.

=cut

.sub '!keyword_enum'
    .param pmc role
    .local pmc class

    # Create an anonymous class and attach the role.
    class = new 'Class'
    "!keyword_does"(class, role)

    # Register it.
    .local pmc p6meta
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    p6meta.'register'(class, 'parent'=>'Any')

    .return(class)
.end


=item !keyword_does(class, role)

Internal helper method to implement the functionality of the does keyword.

=cut

.sub '!keyword_does'
    .param pmc class
    .param pmc role

    # Ensure that role really is a role.
    $I0 = isa role, 'Role'
    if $I0 goto role_ok
    'die'('does keyword can only be used with roles.')
  role_ok:

    # Get Parrot to compose the role for us (handles the methods).
    addrole class, role

    # Parrot doesn't handle composing the attributes; we do that here for now.
    .local pmc role_attrs, class_attrs, ra_iter
    .local string cur_attr
    role_attrs = inspect role, "attributes"
    class_attrs = inspect class, "attributes"
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
  merge:
    goto ra_iter_loop
  ra_iter_loop_end:
.end


=item !keyword_has(class, attr_name, type)

Adds an attribute with the given name to the class or role.

=cut

.sub '!keyword_has'
    .param pmc class
    .param string attr_name
    .param pmc type     :optional
    .param int got_type :opt_flag
    if got_type goto with_type
    class.'add_attribute'(attr_name)
    .return ()
  with_type:
    class.'add_attribute'(attr_name, type)
.end


=item !anon_enum(value_list)

Constructs a Mapping, based upon the values list.

=cut

.sub '!anon_enum'
    .param pmc values

    # Put the values into list context, so case of a single valued enum works.
    values = values.'list'()

    # For now, we assume integer type, unless we have a first pair that says
    # otherwise.
    .local pmc cur_val
    cur_val = new 'Int'
    cur_val = 0

    # Iterate over values and make mapping.
    .local pmc result, values_it, cur_item
    result = new 'Mapping'
    values_it = iter values
  values_loop:
    unless values_it goto values_loop_end
    cur_item = shift values_it
    $I0 = isa cur_item, 'Perl6Pair'
    if $I0 goto pair

  nonpair:
    $P0 = 'postfix:++'(cur_val)
    result[cur_item] = $P0
    goto values_loop

  pair:
    cur_val = cur_item.'value'()
    $P0 = cur_item.'key'()
    result[$P0] = cur_val
    cur_val = clone cur_val
    'postfix:++'(cur_val)
    goto values_loop

  values_loop_end:
    .return (result)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
