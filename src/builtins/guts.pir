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


=item !COPYPARAM

Copies a param for the is copy trait, taking account of any ObjectRef and
dereferencing it so we really do copy the underlying value.

=cut

.sub '!COPYPARAM'
    .param pmc target
    .param pmc source
    $I0 = isa source, 'ObjectRef'
    unless $I0 goto no_deref
    source = deref source
  no_deref:
    .tailcall 'infix:='(target, source)
.end


=item !DOTYPECHECK

Checks that the value and the assignee are type-compatible and does the
assignment.

=cut

.sub '!DOTYPECHECK'
    .param pmc type
    .param pmc value
    .param pmc result
    $I0 = type.'ACCEPTS'(value)
    result = $I0
.end


=item !TYPECHECKPARAM

Checks the type of a parameter.

=cut

.sub '!TYPECHECKPARAM'
    .param pmc type
    .param pmc value
    $P0 = getinterp
    $P0 = $P0['lexpad';1]
    if null $P0 goto no_match_to_copy
    $P0 = $P0['$/']
    .lex "$/", $P0
  no_match_to_copy:

    $I0 = type.'ACCEPTS'(value)
    if $I0 goto ok
    $P0 = getinterp
    $P0 = $P0['sub' ; 1]
    $S0 = $P0
    if $S0 goto have_name
    $S0 = '<anon>'
  have_name:
    'die'('Parameter type check failed in call to ', $S0)
ok:
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
    j1 = t1.'values'()
    j2 = t1.'values'()
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
    if null class goto no_class
    class.'remove_method'(name)
  no_class:

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


=item !keyword_class(name)

Internal helper method to create a class.

=cut

.sub '!keyword_class'
    .param string name   :optional
    .param int have_name :opt_flag
    .local pmc class, resolve_list, methods, it

    # Create class.
    if have_name goto named
    class = new 'Class'
    goto created
  named:
    $P0 = split '::', name
    class = newclass $P0
  created:

    # Set resolve list to include all methods of the class.
    methods = inspect class, 'methods'
    it = iter methods
    resolve_list = new 'ResizableStringArray'
  resolve_loop:
    unless it goto resolve_loop_end
    $P0 = shift it
    push resolve_list, $P0
    goto resolve_loop
  resolve_loop_end:
    class.'resolve_method'(resolve_list)

    .return(class)
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

=item !keyword_grammar(name)

Internal helper method to create a grammar.

=cut

.sub '!keyword_grammar'
    .param string name
    .local pmc grammar

    $P0 = split "::", name
    grammar = newclass $P0

    .return(grammar)
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


=item !ADD_TO_WHENCE

Adds a key/value mapping to what will become the WHENCE on a proto-object (we
don't have a proto-object to stick them on yet, so we put a property on the
class temporarily, then attach it as the WHENCE clause later).

=cut

.sub '!ADD_TO_WHENCE'
    .param pmc class
    .param pmc attr_name
    .param pmc value

    # Get hash if we have it, if not make it.
    .local pmc whence_hash
    whence_hash = getprop '%!WHENCE', class
    unless null whence_hash goto have_hash
    whence_hash = new 'Perl6Hash'
    setprop class, '%!WHENCE', whence_hash

    # Make entry.
  have_hash:
    whence_hash[attr_name] = value
.end


=item !PROTOINIT

Called after a new proto-object has been made for a new class or grammar. It
finds any WHENCE data that we may need to add.

=cut

.sub '!PROTOINIT'
    .param pmc proto

    # See if there's any attribute initializers.
    .local pmc p6meta, WHENCE
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    $P0 = p6meta.'get_parrotclass'(proto)
    WHENCE = getprop '%!WHENCE', $P0
    if null WHENCE goto no_whence

    setprop proto, '%!WHENCE', WHENCE
  no_whence:
    .return (proto)
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
