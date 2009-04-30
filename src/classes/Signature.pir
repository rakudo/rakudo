## $Id$

=head1 TITLE

Signature - Perl 6 Signature class

=head1 DESCRIPTION

This file sets up the Perl 6 C<Signature> class.

=head1 GUTS

This class will evolve over time as we understand signatures and how we will
expose there insides better. For now, a signature under the hood is just an
array of hashes, with each hash being a "descriptor" for something that is
bindable. Its keys are as follows.

* name - string holding the name of the thing we're binding to, if any
* type - the class or role type of the parameter; this references the actual
  type object rather than just naming it, and may well be parametric (but that
  will have been resolved already)
* constraints - any additional "where" refinement types on the parameter;
  will be a junction of types
* invocant - is this the invocant (as in, self for a method, not multi)
* multi_invocant - is this an invocant for the purpose of MMD
* optional - is this an optional parameter?
* slurpy - is this a slurpy parameter?

Again, this probably isn't definitive either, but it'll get us going.

=cut

.namespace ['Signature']

.sub 'onload' :anon :init :load
    load_bytecode 'PCT.pbc'
    .local pmc p6meta
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    p6meta.'new_class'('Signature', 'parent'=>'Any', 'attr'=>'@!params')
.end

=head2 Methods

=over 4

=item !add_param( $varname, *%attr )

Add the attributes given by C<%attr> as the entry for C<$var> in
the Signature.

=cut

.sub '!add_param' :method
    .param string varname
    .param pmc attr            :slurpy :named

    attr['name'] = varname

    # If no multi_invocant value, set it to 1 (meaning it is one).
    $I0 = exists attr['multi_invocant']
    if $I0 goto have_mi
    attr['multi_invocant'] = 1
  have_mi:

    # Work out any role type that the sigil implies. (Skip for slurpy, though.)
    $I0 = attr["slurpy"]
    if $I0 goto sigil_done
    .local pmc role_type
    .local string sigil
    sigil = substr varname, 0, 1
    if sigil == '$' goto sigil_done
    if sigil == '@' goto sigil_array
    if sigil == '%' goto sigil_hash
    if sigil == ':' goto sigil_done
    goto sigil_code
  sigil_array:
    role_type = get_hll_global 'Positional'
    goto sigil_done
  sigil_hash:
    role_type = get_hll_global 'Associative'
    goto sigil_done
  sigil_code:
    role_type = get_hll_global 'Callable'
    goto sigil_done
  sigil_done:

    # Get constraints list, which may have class and role types as well as
    # subset types. If we have no unique role or class type, they all become
    # constraints; otherwise, we find the unique type. Finally, we turn the
    # list of constraints into a junction.
    .local pmc cur_list, cur_list_iter, constraints, type, test_item
    constraints = new 'ResizablePMCArray'
    type = null
    cur_list = attr["type"]
    if null cur_list goto cur_list_loop_end
  have_type_attr:
    cur_list = cur_list.'eigenstates'()
    cur_list_iter = iter cur_list
  cur_list_loop:
    unless cur_list_iter goto cur_list_loop_end
    test_item = shift cur_list_iter
    $I0 = isa test_item, "Role"
    if $I0 goto is_type
    $P0 = getprop "subtype_realtype", test_item
    if null $P0 goto not_refinement
    unless null type goto all_constraints
    type = $P0
    push constraints, test_item
    goto cur_list_loop
  not_refinement:
    $I0 = isa test_item, "P6protoobject"
    if $I0 goto is_type
    push constraints, test_item
    goto cur_list_loop
  is_type:
    unless null type goto all_constraints
    type = test_item
    goto cur_list_loop
  all_constraints:
    type = null
    constraints = cur_list
  cur_list_loop_end:

    # Set parametric type, if any.
    .local pmc all_types
    all_types = new 'ResizablePMCArray'
    unless null type goto have_type
    unless null role_type goto simple_role_type
    type = get_hll_global 'Any'
    goto done_role_type
  simple_role_type:
    type = role_type
    goto done_role_type
  have_type:
    if null role_type goto done_role_type
    type = role_type.'!select'(type)
  done_role_type:
    attr["nom_type"] = type
    $I0 = elements constraints
    if $I0 == 0 goto no_constraints
    $P0 = 'infix:&'(type, constraints :flat)
    attr["type"] = $P0
    constraints = 'infix:&'(constraints :flat)
    goto set_constraints
  no_constraints:
    constraints = new 'Undef'
    attr["type"] = type
  set_constraints:
    attr["cons_type"] = constraints

    # Add to parameters list.
    .local pmc params
    params = self.'params'()
    push params, attr
.end


=item !add_implicit_self

Ensures that if there is no explicit invocant, we add one.

=cut

.sub '!add_implicit_self' :method
    .param pmc type :optional
    unless null type goto have_type
    type = get_hll_global 'Object'
  have_type:

    .local pmc params
    params = self.'params'()
    $I0 = elements params
    if $I0 == 0 goto add_implicit_self
    $P0 = params[0]
    $I0 = $P0['invocant']
    if $I0 != 1 goto add_implicit_self
    .return ()

  add_implicit_self:
    $P0 = get_root_namespace ['parrot';'Hash']
    $P0 = new $P0
    $P0['name'] = 'self'
    $P0['invocant'] = 1
    $P0['multi_invocant'] = 1
    $P0['nom_type'] = type
    unshift params, $P0
.end


=item !make_parameters_rw

Makes all parameters have readtype rw (used to implement e.g. <->).

=cut

.sub '!make_parameters_rw' :method
    .local pmc params, it, param
    params = self.'params'()
    it = iter params
  it_loop:
    unless it goto it_loop_end
    param = shift it
    $P0 = param['readtype']
    unless null $P0 goto it_loop
    param['readtype'] = 'rw'
    goto it_loop
  it_loop_end:
.end


=item params

Get the array of parameter describing hashes.

=cut

.sub 'params' :method
    $P0 = getattribute self, "@!params"
    unless null $P0 goto done
    $P0 = new 'ResizablePMCArray'
    setattribute self, "@!params", $P0
  done:
    .return ($P0)
.end

=item perl

Gets a perl representation of the signature.

=cut

.sub 'perl' :method
    .local pmc s
    s = new 'Str'
    concat s, ':('

    # Output parameters.
    .local pmc params, param_iter, cur_param
    .local int last_was_multi_inv, want_colon, first
    last_was_multi_inv = 1
    want_colon = 0
    first = 1
    params = self.'params'()
    param_iter = iter params
  param_iter_loop:
    unless param_iter goto param_iter_loop_end
    cur_param = shift param_iter

    # If it's the first time, no separator.
    if first goto first_time
    if want_colon goto emit_colon
    $P0 = cur_param["multi_invocant"]
    if $P0 goto emit_comma
    unless last_was_multi_inv goto emit_comma
    concat s, ';; '
    last_was_multi_inv = 0
    goto separator_done
  emit_comma:
    concat s, ', '
    goto separator_done
  emit_colon:
    concat s, ': '
    goto separator_done
  first_time:
    first = 0
  separator_done:

    # First any nominal type.
    $P0 = cur_param["nom_type"]
    if null $P0 goto any_type
    $I0 = isa $P0, 'Role'
    unless $I0 goto type_as_is
    $S0 = cur_param["name"]
    $S0 = substr $S0, 0, 1
    if $S0 == '$' goto type_as_is
    $S1 = $P0.'perl'()
    $I0 = index $S1, '['
    inc $I0
    $I1 = length $S1
    $I1 -= $I0
    dec $I1
    $S1 = substr $S1, $I0, $I1
    concat s, $S1
    goto type_done
  type_as_is:
    $P0 = $P0.'perl'()
    if $P0 == 'Positional' goto no_type
    if $P0 == 'Associative' goto no_type
    if $P0 == 'Callable' goto no_type
    concat s, $P0
    goto type_done
  any_type:
    concat s, "Any"
  type_done:
    concat s, " "
  no_type:

    # If it's slurpy, the *.
    $P0 = cur_param["slurpy"]
    if null $P0 goto slurpy_done
    unless $P0 goto slurpy_done
    concat s, '*'
  slurpy_done:

    # Now the name.
    $P0 = cur_param["name"]
    concat s, $P0

    # If it's optional, the ?.
    $P0 = cur_param["optional"]
    if null $P0 goto optional_done
    unless $P0 goto optional_done
    concat s, '?'
  optional_done:

    # Now any constraints.
    $P0 = cur_param["cons_type"]
    if null $P0 goto constraints_done
    unless $P0 goto constraints_done
    concat s, " where "
    $P0 = $P0.'perl'()
    concat s, $P0
  constraints_done:

    goto param_iter_loop
  param_iter_loop_end:

    # If we just had an invocant, need the colon.
    unless want_colon goto no_trailing_colon
    concat s, ':'
  no_trailing_colon:

    # XXX TODO: Return type, once we support those.

    # Done.
    concat s, ')'
    .return (s)
.end

=item !SIGNATURE_BIND

Analyze the signature of the caller, (re)binding the caller's
lexicals as needed and performing type checks.

=cut

.namespace []
.sub '!SIGNATURE_BIND'
    .local pmc callersub, callerlex, callersig
    $P0 = getinterp
    callersub = $P0['sub';1]
    callerlex = $P0['lexpad';1]
    getprop callersig, '$!signature', callersub
    if null callersig goto end
    .local pmc it
    $P0 = callersig.'params'()
    if null $P0 goto end
    it = iter $P0
  param_loop:
    unless it goto param_done
    .local pmc param
    param = shift it
    .local string name, sigil
    name = param['name']
    if name == 'self' goto param_loop
    sigil = substr name, 0, 1
    .local pmc type, optional, orig, var
    type = param['type']
    optional = param['optional']
    orig = callerlex[name]
    if sigil == '@' goto param_array
    if sigil == '%' goto param_hash
    var = '!CALLMETHOD'('Scalar', orig)
    ##  typecheck the argument unless it's undef (for optional parameter)
    if null optional goto not_optional
    $I0 = defined orig
    unless $I0 goto param_val_done 
  not_optional:
    if null type goto param_val_done
    .lex '$/', $P99
    $P0 = type.'ACCEPTS'(var)
    unless $P0 goto err_param_type
    goto param_val_done
  param_array:
    $P0 = type.'ACCEPTS'(orig)
    unless $P0 goto err_param_type_non_scalar
    var = '!DEREF'(orig)
    var = '!CALLMETHOD'('Array', var)
    goto param_val_done
  param_hash:
    $P0 = type.'ACCEPTS'(orig)
    unless $P0 goto err_param_type_non_scalar
    var = '!DEREF'(orig)
    var = '!CALLMETHOD'('Hash', var)
    goto param_val_done
  param_val_done:
    ## handle readonly/copy traits
    $S0 = param['readtype']
    if $S0 == 'rw' goto param_readtype_done
    if $S0 == 'copy' goto param_readtype_copy
    ne_addr orig, var, param_readtype_var
    var = new 'ObjectRef', var
  param_readtype_var:
    $P0 = get_hll_global ['Bool'], 'True'
    setprop var, 'readonly', $P0
    goto param_readtype_done
  param_readtype_copy:
    if sigil == '@' goto param_readtype_copy_array
    if sigil == '%' goto param_readtype_copy_hash
    var = clone var
    goto param_readtype_done
  param_readtype_copy_array:
    $P0 = new 'Perl6Array'
    'infix:='($P0, var)
    var = $P0
    goto param_readtype_done
  param_readtype_copy_hash:
    $P0 = new 'Perl6Hash'
    'infix:='($P0, var)
    var = $P0
  param_readtype_done:
    ## set any type properties
    setprop var, 'type', type
    ## place the updated variable back into lex
    callerlex[name] = var
    goto param_loop

  param_done:
  end:
    .return ()

  err_param_type_non_scalar:
    set var, orig
  err_param_type:
    # Is it a junctional parameter?
    $I0 = isa var, 'Junction'
    unless $I0 goto not_junctional
    $P0 = '!DISPATCH_JUNCTION_SINGLE'(callersub, callerlex, callersig)
    'return'($P0)
  not_junctional:
    .local string errmsg
    errmsg = 'Parameter type check failed; expected something matching '
    $S0 = type.'perl'()
    concat errmsg, $S0
    concat errmsg, ' but got something of type '
    $S0 = orig.'WHAT'()
    concat errmsg, $S0
    .local string callername
    callername = callersub
    if callername goto have_callername
    callername = '<anon>'
  have_callername:
    'die'(errmsg, ' for ', name, ' in call to ', callername)
.end


=back

=cut


# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
