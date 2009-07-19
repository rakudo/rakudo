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
    .param int to_p6_multi     :named('to_p6_multi') :optional

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
    if to_p6_multi != 1 goto no_convert
    $P0 = value[0]
    '!TOPERL6MULTISUB'($P0)
    value = from[symbol]
  no_convert:
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
    anyobj = new ['Any']
    set_global '$!ANY', anyobj
  any_method_1:
    $P0 = find_method anyobj, method
    .tailcall obj.$P0()
.end


=item !dispatch_method_indirect

Does an indirect method dispatch.

=cut

.sub '!dispatch_method_indirect'
    .param pmc obj
    .param pmc methodish
    .param pmc pos_args  :slurpy
    .param pmc name_args :slurpy :named

    $P0 = get_hll_global 'Callable'
    $I0 = $P0.'ACCEPTS'(methodish)
    unless $I0 goto candidate_list
    .tailcall methodish(obj, pos_args :flat, name_args :flat :named)

  candidate_list:
    $P0 = root_new ['parrot';'P6Invocation'], methodish
    .tailcall $P0(obj, pos_args :flat, name_args :flat :named)
.end


=item !dispatch_dispatcher_parallel

Does a parallel method dispatch over an existing dispatcher. Just invokes the normal
dispatcher for each thingy we're dispatching over.

=cut

.sub '!dispatch_dispatcher_parallel'
    .param pmc invocanty
    .param string dispatcher
    .param pmc pos_args        :slurpy
    .param pmc named_args      :slurpy :named

    .local pmc it, result, disp
    disp = find_name dispatcher
    result = new ['Perl6Array']
    invocanty = invocanty.'list'()
    it = iter invocanty
  it_loop:
    unless it goto it_loop_done
    $P0 = shift it
    $P0 = disp($P0, pos_args :flat, named_args :flat :named)
    $P0 = $P0.'Scalar'()
    result.'push'($P0)
    goto it_loop
  it_loop_done:

    .return (result)
.end


=item !dispatch_method_parallel

Does a parallel method dispatch. Invokes the method for each thing in the
array of invocants.

=cut

.sub '!dispatch_method_parallel'
    .param pmc invocanty
    .param string name
    .param pmc pos_args        :slurpy
    .param pmc named_args      :slurpy :named

    .local pmc it, result
    result = new ['Perl6Array']
    invocanty = invocanty.'list'()
    it = iter invocanty
  it_loop:
    unless it goto it_loop_done
    $P0 = shift it
    $P0 = $P0.name(pos_args :flat, named_args :flat :named)
    $P0 = $P0.'Scalar'()
    result.'push'($P0)
    goto it_loop
  it_loop_done:

    .return (result)
.end


=item !VAR

Helper function for implementing the VAR and .VAR macros.

=cut

.sub '!VAR'
    .param pmc variable
    $I0 = isa variable, 'Perl6Scalar'
    unless $I0 goto nothing
    $P0 = root_new ['parrot';'MutableVAR'], variable
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
    j1 = t1.'eigenstates'()
    j2 = t1.'eigenstates'()
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

    # If it's an un-disambiguated role, dis-ambiguate.
    $I0 = isa real_type, 'Perl6Role'
    unless $I0 goto role_done
    real_type = real_type.'!select'()
  role_done:

    # Create subclass.
    .local pmc parrot_class, subset
    parrot_class = p6meta.'get_parrotclass'(refinee)
    subset = subclass parrot_class

    # Override accepts.
    .local pmc parrotclass
    .const 'Sub' $P0 = "!SUBTYPE_ACCEPTS"
    subset.'add_method'('ACCEPTS', $P0)
    .const 'Sub' $P1 = "!SUBTYPE_PROTOOVERRIDES"
    subset.'add_method'('PROTOOVERRIDES', $P1)

    # It's an abstraction.
    $P0 = get_hll_global 'Abstraction'
    $P0 = $P0.'!select'()
    subset.'add_role'($P0)

    # Register it, creating a proto-object.
    subset = p6meta.'register'(subset)

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
.sub '!SUBTYPE_PROTOOVERRIDES' :anon :method
    .return ('new', 'ACCEPTS')
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
    p6multi = root_new ['parrot';'Perl6MultiSub']
    sub_iter = iter current_thing
  iter_loop:
    unless sub_iter goto iter_loop_end
    $P0 = shift sub_iter
    push p6multi, $P0
    goto iter_loop
  iter_loop_end:

    # Nor replace the current thing with the new data structure.
    copy current_thing, p6multi
    .return()

  error:
    'die'('Sub lookup failed')
.end


=item !clone_multi_for_lexical

=cut

.sub '!clone_multi_for_lexical'
    .param pmc existing
    if null existing goto fresh
    unless existing goto fresh
    $P0 = existing.'clone'()
    .return ($P0)
  fresh:
    $P0 = root_new ['parrot';'Perl6MultiSub']
    .return ($P0)  
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
    ## Remove program argument (0) and put it in $*PROGRAM_NAME, then set up
    ## @ARGS global.
    $P0 = shift args
    set_hll_global '$PROGRAM_NAME', $P0
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


=item !ADDTOROLE

Adds a given role initializing multi-variant to a Role object, creating it
and putting it in the namespace if it doesn't already exist.

=cut

.sub '!ADDTOROLE'
    .param pmc variant

    # Get short name of role.
    .local pmc ns
    .local string short_name
    ns = variant.'get_namespace'()
    ns = ns.'get_name'()
    short_name = pop ns
    $P0 = box short_name
    setprop variant, "$!shortname", $P0
    $I0 = index short_name, '['
    if $I0 == -1 goto have_short_name
    short_name = substr short_name, 0, $I0
  have_short_name:

    # See if we have a Role object already.
    .local pmc role_obj
    role_obj = get_root_global ns, short_name
    if null role_obj goto need_role_obj
    $I0 = isa role_obj, 'NameSpace'
    unless $I0 goto have_role_obj
  need_role_obj:
    role_obj = new ['Perl6Role']
    transform_to_p6opaque role_obj
    set_root_global ns, short_name, role_obj
    $P0 = box short_name
    setattribute role_obj, "$!shortname", $P0
  have_role_obj:

    # Add this variant.
    role_obj.'!add_variant'(variant)
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
    $P0 = get_hll_global [ 'Perl6';'Compiler' ], 'parse_name'
    $P1 = null
    nsarray = $P0($P1, name)

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
    '!set_resolves_list'(metaclass)
    .return (metaclass)
  is_also:
    metaclass = get_class ns
    .return (metaclass)

  role:
    # This is a little fun. We only want to create the Parrot role and suck
    # in the methods once per role definition. We do this and it is attached to
    # the namespace. Then we attach this "master role" to a new one we create
    # per invocation, so the methods can be newclosure'd and added into it in
    # the body.
    .local pmc info, metarole
    ns = get_hll_namespace nsarray
    metarole = get_class ns
    unless null metarole goto have_role

    info = root_new ['parrot';'Hash']
    $P0 = nsarray[-1]
    info['name'] = $P0
    info['namespace'] = nsarray
    metarole = root_new ['parrot';'P6role'], info
  have_role:

    # Copy list of roles done by the metarole.
    .local pmc result, tmp, it
    result = root_new ['parrot';'P6role']
    setprop result, '$!orig_role', metarole
    tmp = metarole.'roles'()
    it = iter tmp
  roles_loop:
    unless it goto roles_loop_end
    tmp = shift it
    result.'add_role'(tmp)
    goto roles_loop
  roles_loop_end:

    .return (result)
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
    roles = getprop '@!roles', metaclass
    if null roles goto roles_it_loop_end
    roles = '!get_flattened_roles_list'(roles)
    roles_it = iter roles
  roles_it_loop:
    unless roles_it goto roles_it_loop_end
    $P0 = shift roles_it
    $I0 = does metaclass, $P0
    if $I0 goto roles_it_loop
    metaclass.'add_role'($P0)
    '!compose_role_attributes'(metaclass, $P0)
    goto roles_it_loop
  roles_it_loop_end:

    # Create proto-object with default parent being Any or Grammar, unless
    # there already is a parent.
    $P0 = metaclass.'parents'()
    $I0 = elements $P0
    if $I0 goto register_parent_set
    $S0 = 'Any'
    $P0 = getprop 'pkgtype', metaclass
    if null $P0 goto no_pkgtype
    if $P0 != 'grammar' goto register
    $S0 = 'Grammar'
  register:
    .tailcall p6meta.'register'(metaclass, 'parent'=>$S0)
  register_parent_set:
    .tailcall p6meta.'register'(metaclass)
  no_pkgtype:
.end


=item !get_flattened_roles_list

Flattens out the list of roles.

=cut

.sub '!get_flattened_roles_list'
    .param pmc unflat_list
    .local pmc flat_list, it, cur_role, nested_roles, nested_it
    flat_list = root_new ['parrot';'ResizablePMCArray']
    it = iter unflat_list
  it_loop:
    unless it goto it_loop_end
    cur_role = shift it
    $I0 = isa cur_role, 'Role'
    unless $I0 goto error_not_a_role
    push flat_list, cur_role
    nested_roles = getprop '@!roles', cur_role
    if null nested_roles goto it_loop
    nested_roles = '!get_flattened_roles_list'(nested_roles)
    nested_it = iter nested_roles
  nested_it_loop:
    unless nested_it goto it_loop
    $P0 = shift nested_it
    push flat_list, $P0
    goto nested_it_loop
  it_loop_end:
    .return (flat_list)
  error_not_a_role:
    'die'('Can not compose a non-role.')
.end


=item !meta_compose(Role)

Role meta composer -- does nothing.

=cut

.sub '!meta_compose' :multi(['Role'])
    .param pmc metaclass
    # Currently, nothing to do.
    .return (metaclass)
.end


=item !meta_compose()

Default meta composer -- does nothing.

=cut

.sub '!meta_compose' :multi()
    .param pmc metaclass
    # Currently, nothing to do.
    .return (metaclass)
.end


=item !meta_attribute(metaclass, name, itypename [, 'type'=>type] )

Add attribute C<name> to C<metaclass> with the given C<itypename>
and C<type>.

=cut

.sub '!meta_attribute'
    .param pmc metaclass
    .param string name
    .param string itypename    :optional
    .param int has_itypename   :opt_flag
    .param pmc attr            :slurpy :named

    # twigil handling (for has &!foo, we just get name as !foo)
    .local int offset
    .local string twigil
    offset = 1
    $S0 = substr name, 0, 1
    if $S0 != '!' goto offset_done
    offset = 0
  offset_done:
    twigil = substr name, offset, 1
    if twigil == '.' goto twigil_public
    if twigil == '!' goto twigil_done
    substr name, offset, 0, '!'
    goto twigil_done
  twigil_public:
    substr name, offset, 1, '!'
  twigil_done:

    $P0 = metaclass.'attributes'()
    $I0 = exists $P0[name]
    if $I0 goto attr_exists
    addattribute metaclass, name
    $P1 = getprop '@!attribute_list', metaclass
    unless null $P1 goto have_attrlist
    $P1 = root_new ['parrot';'ResizableStringArray']
    setprop metaclass, '@!attribute_list', $P1
  have_attrlist:
    push $P1, name
    $P0 = metaclass.'attributes'()
  attr_exists:

    .local pmc attrhash, it
    attrhash = $P0[name]

    # Set any itype for the attribute.
    unless has_itypename goto itype_done
    .local pmc itype
    if itypename == 'Perl6Scalar' goto itype_pmc
    itype = get_class itypename
    goto have_itype
  itype_pmc:
    $P0 = get_root_namespace ['parrot';'Perl6Scalar']
    itype = get_class $P0
  have_itype:
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

    # Anything to do with handles?
    $P0 = attr['handles']
    if null $P0 goto handles_done

    # For the handles trait verb, we may have got a name or a list of names.
    # If so, just generate methods with those names. Otherwise, need to store
    # them as a property on the metaclass, so the dispatcher can smart-match
    # against them later. Also, the % syntax is spec'd as reserved, so we give
    # an error on that for now.
    .const 'Sub' handles = '!handles'
    .local pmc handles_it
    $S0 = substr name, 0, 1
    if $S0 == '%' goto reserved_syntax_error
    $I0 = isa $P0, 'Str'
    if $I0 goto simple_handles
    $I0 = isa $P0, 'List'
    if $I0 goto simple_handles
    $I0 = isa $P0, 'Perl6Pair'
    if $I0 goto simple_handles

    .local pmc class_handles_list, handles_hash
    class_handles_list = getprop '@!handles_dispatchers', metaclass
    unless null class_handles_list goto have_class_handles_list
    class_handles_list = root_new ['parrot';'ResizablePMCArray']
    setprop metaclass, '@!handles_dispatchers', class_handles_list
  have_class_handles_list:
    handles_hash = root_new ['parrot';'Hash']
    handles_hash['attrname'] = name
    handles_hash['match_against'] = $P0
    push class_handles_list, handles_hash
    goto handles_done

  simple_handles:
    $P0 = 'list'($P0)
    handles_it = iter $P0
  handles_loop:
    .local string visible_name
    .local pmc orig_name
    unless handles_it goto handles_done
    $P0 = clone handles
    $P1 = box name
    setprop $P0, 'attrname', $P1
    $P1 = shift handles_it
    $I0 = isa $P1, 'Perl6Pair'
    if $I0 goto handles_pair
    visible_name = $P1
    orig_name = $P1
    goto naming_done
  handles_pair:
    visible_name = $P1.'key'()
    orig_name = $P1.'value'()
  naming_done:
    setprop $P0, 'methodname', orig_name
    metaclass.'add_method'(visible_name, $P0)
    goto handles_loop
  handles_done:
    .return ()
  reserved_syntax_error:
    'die'("The use of a %hash with the handles trait verb is reserved")
.end


.sub '!handles' :method
    .param pmc args            :slurpy
    .param pmc options         :slurpy :named
    .local pmc method, attribute
    .local string attrname
    $P0 = getinterp
    method = $P0['sub']
    $P1 = getprop 'attrname', method
    attrname = $P1
    attribute = getattribute self, attrname
    $P1 = getprop 'methodname', method
    $S1 = $P1
    $S0 = substr attrname, 0, 1
    if $S0 != '@' goto single_dispatch
    .local pmc it
    it = iter attribute
  it_loop:
    unless it goto it_loop_end
    $P0 = shift it
    $I0 = $P0.'can'($S1)
    unless $I0 goto it_loop
    .tailcall $P0.$S1(args :flat, options :flat :named)
  it_loop_end:
    'die'("You used handles on attribute ", attrname, ", but nothing in the array can do method ", $S1)
  single_dispatch:
    .tailcall attribute.$S1(args :flat, options :flat :named)
.end


=item !set_resolves_list(class)

Gets all the methods that the class has and adds them to the resolves list.

=cut

.sub '!set_resolves_list'
    .param pmc class
    .local pmc meths, it, res_list
    meths = class.'methods'()
    it = iter meths
    res_list = root_new ['parrot';'ResizableStringArray']
  it_loop:
    unless it goto it_loop_end
    $S0 = shift it
    $P0 = meths[$S0]
    $I0 = isa $P0, 'MultiSub'
    if $I0 goto it_loop
    push res_list, $S0
    goto it_loop
  it_loop_end:
    class.'resolve_method'(res_list)
.end


=item !compose_role_attributes(class, role)

Helper method to compose the attributes of a role into a class.

=cut

.sub '!compose_role_attributes'
    .param pmc class
    .param pmc role

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

=item !create_parametric_role

Helper method for creating parametric roles.

=cut

.sub '!create_parametric_role'
    .param pmc mr
    '!meta_compose'(mr)
    .local pmc orig_role, meths, meth_iter
    orig_role = getprop '$!orig_role', mr
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
    addmethod mr, $S0, $P1
    goto it_loop
  it_loop_end:
    .return (mr)
.end


=item !create_simple_role(name)

Internal helper method to create a role with a single parameterless variant.

=cut

.sub '!create_simple_role'
    .param string name
    .local pmc info, role, helper

    # Create Parrot-level role. Need to make sure it gets its methods from
    # the right namespace.
    .local pmc ns
    ns = split '::', name
    name = ns[-1]
    info = root_new ['parrot';'Hash']
    info['name'] = name
    info['namespace'] = ns
    role = root_new ['parrot';'P6role'], info

    # Now we need to wrap it up as a Perl6Role.
    helper = find_name '!create_simple_role_helper'
    helper = clone helper
    setprop helper, '$!metarole', role
    $P0 = new ["Signature"]
    setprop helper, '$!signature', $P0
    role = new ['Perl6Role']
    transform_to_p6opaque role

    $P0 = box name
    setattribute role, '$!shortname', $P0
    role.'!add_variant'(helper)

    # Store it in the namespace.
    ns = clone ns
    $S0 = pop ns
    set_hll_global ns, $S0, role
    .return(role)
.end
.sub '!create_simple_role_helper'
    $P0 = getinterp
    $P0 = $P0['sub']
    $P0 = getprop '$!metarole', $P0
    .return ($P0)
.end


=item !create_anon_enum(value_list)

Constructs a Mapping, based upon the values list.

=cut

.sub '!create_anon_enum'
    .param pmc values

    # Put the values into list context, so case of a single valued enum works.
    values = values.'list'()

    # For now, we assume integer type, unless we have a first pair that says
    # otherwise.
    .local pmc cur_val
    cur_val = box 0

    # Iterate over values and make mapping.
    .local pmc result, values_it, cur_item
    result = new ['Mapping']
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


=item !create_enum(name, type, value_list)

Constructs an enumeration.

=cut

.sub '!create_enum'
    .param string name
    .param pmc values

    # Use !create_anon_enum to associate all names with their underlying
    # values.
    values = '!create_anon_enum'(values)

    # Create a role for the enumeration and mark it as an enum.
    .local pmc para_role, role
    para_role = '!create_simple_role'(name)
    role = para_role.'!select'()
    $P0 = box 1
    setprop role, '$!is_enum', $P0

    # Compute short name and add attribute to the role; type is this
    # role so that you can only store other enum elements in the slut.
    .local pmc ns, outer_ns
    .local string short_name, attr_name
    $P0 = get_hll_global [ 'Perl6';'Compiler' ], 'parse_name'
    $P1 = null
    ns = $P0($P1, name)
    outer_ns = clone ns
    short_name = pop outer_ns
    attr_name = concat "$!", short_name
    '!meta_attribute'(role, attr_name, 'Perl6Scalar', 'type'=>role)
    
    # Add an l-value accessor method for the attribute.
    .local pmc attr_name_pmc, accessor
    attr_name_pmc = box attr_name
    .lex '$attr_name', attr_name_pmc
    .const 'Sub' accessor = '!create_enum_helper_accessor'
    accessor = newclosure accessor
    addmethod role, short_name, accessor

    # Next, we need methods on the role for each variant, returning
    # a true or false value depending on if the current value of the
    # enum is set to that.
    .const 'Sub' checker_create = '!create_enum_helper_checker_create'
    .local pmc it, cur_value
    it = iter values
  checker_loop:
    unless it goto checker_loop_end
    $S0 = shift it
    cur_value = values[$S0]
    $P0 = checker_create(attr_name, cur_value)
    addmethod role, $S0, $P0
    goto checker_loop
  checker_loop_end:

    # We'll make a list of the values and the .pick method on the role will
    # use that (Enum.pick then just works through punning).
    .local pmc value_list
    .local string value_name
    value_list = root_new ['parrot';'ResizablePMCArray']
    .lex '@values', value_list
    .const 'Sub' pick = '!create_enum_helper_pick'
    pick = newclosure pick
    addmethod role, 'pick', pick

    # Go over all of the values...
    it = iter values
  value_loop:
    unless it goto value_loop_end
    value_name = shift it
    cur_value = values[value_name]

    # Mix the enum role into it, so Val ~~ Enum will work, and set the value
    # field to itself plus set it readonly.
    cur_value = 'infix:but'(cur_value, role)
    $P0 = cur_value.short_name()
    copy $P0, cur_value
    $P1 = box 1
    setprop $P0, 'readonly', $P1

    # It should also do Abstraction.
    $P0 = get_hll_global 'Abstraction'
    'infix:does'(cur_value, $P0)

    # Now create and mix in another role to provide .WHAT, .perl and .name.
    $S0 = concat name, '::'
    $S0 = concat value_name
    $P0 = '!create_enum_value_role'(role, $S0, value_name)
    'infix:does'(cur_value, $P0)

    # Put it onto the list for .pick and install it in the namespace(s).
    push value_list, cur_value
    set_hll_global ns, value_name, cur_value
    set_hll_global outer_ns, value_name, cur_value

    goto value_loop
  value_loop_end:
.end
.sub '!create_enum_helper_accessor' :method :outer('!create_enum')
    $P0 = find_lex '$attr_name'
    $S0 = $P0
    $P0 = getattribute self, $S0
    .return ($P0)
.end
.sub '!create_enum_helper_checker_create'
    .param pmc attr_name
    .param pmc value
    .lex '$attr_name', attr_name
    .lex '$value', value
    .const 'Sub' $P0 = '!create_enum_helper_checker'
    $P0 = newclosure $P0
    .return ($P0)
.end
.sub '!create_enum_helper_checker' :method :outer('!create_enum_helper_checker_create')
    $P0 = find_lex '$attr_name'
    $S0 = $P0
    $P0 = getattribute self, $S0
    $P1 = find_lex '$value'
    .tailcall 'infix:eq'($P0, $P1)
.end
.sub '!create_enum_helper_pick' :method :outer('!create_enum')
    .param pmc pos_args :slurpy
    $P0 = find_lex '@values'
    $P0 = 'list'($P0 :flat)
    .tailcall $P0.'pick'(pos_args :flat)
.end
.sub '!create_enum_value_role'
    .param pmc enum_role
    .param pmc long_name
    .param pmc short_name
    .lex '$enum_role', enum_role
    .lex '$long_name', long_name
    .lex '$short_name', short_name
    $P0 = root_new ['parrot';'P6role']
    .const 'Sub' ACCEPTS = '!create_enum_value_role_ACCEPTS'
    ACCEPTS = newclosure ACCEPTS
    addmethod $P0, 'ACCEPTS', ACCEPTS
    .const 'Sub' WHAT = '!create_enum_value_role_WHAT'
    WHAT = newclosure WHAT
    addmethod $P0, 'WHAT', WHAT
    .const 'Sub' name = '!create_enum_value_role_name'
    name = newclosure name
    addmethod $P0, 'name', name
    .const 'Sub' perl = '!create_enum_value_role_perl'
    perl = newclosure perl
    addmethod $P0, 'perl', perl
    .return ($P0)
.end
.sub '!create_enum_value_role_ACCEPTS' :method :outer('!create_enum_value_role')
    .param pmc topic
    $P0 = find_lex '$enum_role'
    $I0 = does topic, $P0
    unless $I0 goto done
    $P0 = find_lex '$short_name'
    $S0 = $P0
    $I0 = topic.$S0()
  done:
    .return ($I0)
.end
.sub '!create_enum_value_role_WHAT' :method :outer('!create_enum_value_role')
    $P0 = find_lex '$enum_role'
    .return ($P0)
.end
.sub '!create_enum_value_role_name' :method :outer('!create_enum_value_role')
    $P0 = find_lex '$short_name'
    .return ($P0)
.end
.sub '!create_enum_value_role_perl' :method :outer('!create_enum_value_role')
    $P0 = find_lex '$long_name'
    .return ($P0)
.end


=item !fixup_routine_type(sub, new_type)

Reblesses a sub into a new type.

=cut

.sub '!fixup_routine_type'
    .param pmc sub
    .param string new_type_name

    # Create the correct object and rebless the sub into that class.
    .local pmc new_type
    new_type = get_hll_global new_type_name
    $P0 = new_type.'new'()
    $P0 = typeof $P0
    rebless_subclass sub, $P0
.end


=item !state_var_init

Loads any existing values of state variables for a block.

=cut

.sub '!state_var_init'
    .local pmc lexpad, state_store, names_it
    $P0 = getinterp
    lexpad = $P0['lexpad'; 1]
    $P0 = $P0['sub'; 1]
    state_store = getprop '$!state_store', $P0
    unless null state_store goto have_state_store
    state_store = root_new ['parrot';'Hash']
    setprop $P0, '$!state_store', state_store
  have_state_store:

    names_it = iter state_store
  names_loop:
    unless names_it goto names_loop_end
    $S0 = shift names_it
    $P0 = state_store[$S0]
    lexpad[$S0] = $P0
    goto names_loop
  names_loop_end:
.end


=item !state_var_inited

Takes the name of a state variable and returns true if it's been
initialized already.

=cut

.sub '!state_var_inited'
    .param string name
    $P0 = getinterp
    $P0 = $P0['sub'; 1]
    $P0 = getprop '$!state_store', $P0
    $P0 = $P0[name]
    $I0 = isnull $P0
    $I0 = not $I0
    .return ($I0)
.end


=item !MAKE_WHATEVER_CLOSURE

Creates whatever closures (*.foo => { $_.foo })

=cut

.sub '!MAKE_WHATEVER_CLOSURE'
    .param pmc whatever
    .param pmc pos_args   :slurpy
    .param pmc named_args :slurpy :named
    .local pmc name
    $P0 = getinterp
    $P0 = $P0['sub']
    name = getprop 'name', $P0
    .lex '$name', name
    .lex '$pos_args', pos_args
    .lex '$named_args', named_args
    .const 'Sub' $P0 = '!whatever_dispatch_helper'
    $P0 = newclosure $P0
    .const 'Sub' fixup = '!fixup_routine_type'
    fixup($P0, "Block")
    .return ($P0)
.end
.sub '!whatever_dispatch_helper' :outer('!MAKE_WHATEVER_CLOSURE')
    .param pmc obj
    $P0 = find_lex '$name'
    $S0 = $P0
    $P1 = find_lex '$pos_args'
    $P2 = find_lex '$named_args'
    .tailcall obj.$S0($P1 :flat, $P2 :flat :named)
.end


=item !HANDLES_HELPER

=cut

.sub '!HANDLES_DISPATCH_HELPER'
    .param pmc obj
    .param pmc pos_args   :slurpy
    .param pmc name_args  :slurpy :named
    
    # Look up attribute and method name, and look up the attribute.
    .local pmc attr
    .local string attrname, methodname
    $P0 = getinterp
    $P0 = $P0['sub']
    $P1 = getprop 'methodname', $P0
    methodname = $P1
    $P1 = getprop 'attrname', $P0
    attrname = $P1
    attr = getattribute obj, attrname

    # If it's an array, need to iterate over the set of options. Otherwise,
    # just delegate.
    $S0 = substr attrname, 0, 1
    if $S0 == '@' goto handles_on_array
    .tailcall attr.methodname(pos_args :flat, name_args :flat :named)
  handles_on_array:
    .local pmc handles_array_it
    handles_array_it = iter attr
  handles_array_it_loop:
    unless handles_array_it goto handles_array_it_loop_end
    $P0 = shift handles_array_it
    $I0 = $P0.'can'(methodname)
    unless $I0 goto handles_array_it_loop
    .tailcall $P0.methodname(pos_args :flat, name_args :flat :named)
  handles_array_it_loop_end:
    'die'("You used handles on attribute ", attrname, ", but nothing in the array can do method ", methodname)
.end


=item !make_type_fail_message

Makes a type check failure error message, so we don't have to be doing so all
over the rest of the code base.

=cut

.sub '!make_type_fail_message'
    .param string what_failed
    .param pmc got_type
    .param pmc wanted_type

    # Initial bit.
    .local string output
    output = concat what_failed, " type check failed; expected "

    # Work out what we were looking for and show that.
    $P0 = wanted_type.'WHAT'()
    $S0 = $P0.'perl'()
    output = concat $S0

    # Report what we actually got.
    output = concat ", but got "
    $P0 = got_type.'WHAT'()
    $S0 = $P0.'perl'()
    output = concat $S0

    .return (output)
.end


=item !bindability_checker

Invokes a sub in bindability checking mode. Catches any exceptions that are
thrown while trying to bind. If the bind fails, returns null. Otherwise, we
return the resume continuation so we can continue execution after the bind.

=cut

.sub '!bindability_checker'
    .param pmc orig_sub
    .param pmc pos_args
    .param pmc named_args

    # Clone sub and attach a prop to say we're just doing a bindability check.
    .local pmc sub
    sub = clone orig_sub
    .fixup_cloned_sub(orig_sub, sub)
    setprop sub, '$!bind_check_only', sub

    # Set up exception handler and invoke. We really should get an exception
    # whether it binds or not; if we don't, best we can do is hand back the
    # sub, but warn something may be very wrong.
    push_eh oh_noes
    sub(pos_args :flat, named_args :flat :named)
    pop_eh
    warn("Potential internal error: bindability check may have done more than just binding.")
    .return (sub)

  oh_noes:
    .local pmc ex
    .get_results (ex)
    if ex == '__BIND_SUCCESSFUL__' goto success
    null $P0
    .return ($P0)
  success:
    $P0 = ex["resume"]
    .return ($P0)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
