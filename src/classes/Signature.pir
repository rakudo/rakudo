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

    # Get constraints list, which may have class and role types as well as
    # subset types. If we have no unique role or class type, they all become
    # constraints; otherwise, we find the unique type. Finally, we turn the
    # list of constraints into a junction.
    .local pmc cur_list, cur_list_iter, constraints, type, test_item
    constraints = 'list'()
    type = null
    cur_list = attr["type"]
    unless null cur_list goto have_type_attr
    $P0 = get_hll_global 'Any'
    cur_list = 'all'($P0)
    attr["type"] = cur_list
  have_type_attr:
    cur_list = cur_list.'!eigenstates'()
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
    unless null type goto have_type
    type = get_hll_global 'Any'
  have_type:
    attr["nom_type"] = type
    $I0 = elements constraints
    if $I0 == 0 goto no_constraints
    constraints = 'all'(constraints)
    goto set_constraints
  no_constraints:
    constraints = null
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
    .local pmc params
    params = self.'params'()
    $I0 = elements params
    if $I0 == 0 goto add_implicit_self
    $P0 = params[0]
    $I0 = $P0['invocant']
    if $I0 != 1 goto add_implicit_self
    .return ()

  add_implicit_self:
    $P0 = new 'Hash'
    $P0['name'] = 'self'
    $P0['invocant'] = 1
    $P0['multi_invocant'] = 1
    # XXX Need to get type of class/role/grammar method is in.
    $P1 = get_hll_global 'Object'
    $P0['nom_type'] = $P1
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
    $P0 = $P0.'perl'()
    concat s, $P0
    goto type_done
  any_type:
    concat s, "Any"
  type_done:
    concat s, " "

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
    if sigil != '$' goto param_sub
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
    $I0 = does orig, 'Positional'
    if $I0 goto param_array_1
    $I0 = does orig, 'array'
    unless $I0 goto err_array
  param_array_1:
    var = '!DEREF'(orig)
    var = '!CALLMETHOD'('Array', var)
    goto param_val_done
  param_hash:
    $I0 = does orig, 'Associative'
    if $I0 goto param_hash_1
    $I0 = does orig, 'hash'
    unless $I0 goto err_hash
  param_hash_1:
    var = '!DEREF'(orig)
    var = '!CALLMETHOD'('Hash', var)
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
    var = clone var
  param_readtype_done:
    ## set any type properties
    setprop var, 'type', type
    ## place the updated variable back into lex
    callerlex[name] = var
    goto param_loop
  param_sub:
    $I0 = isa orig, 'Sub'
    unless $I0 goto err_sub
    if $I0 goto param_loop

  param_done:
  end:
    .return ()

  err_param_type:
    # Is it a junctional parameter?
    $I0 = isa var, 'Junction'
    unless $I0 goto not_junctional
    $P0 = '!DISPATCH_JUNCTION_SINGLE'(callersub, callerlex, callersig)
    'return'($P0)
  not_junctional:
    .local string errmsg
    errmsg = 'Parameter type check failed'
    goto err_throw
  err_array:
    errmsg = 'Non-Positional argument'
    goto err_throw
  err_hash:
    errmsg = 'Non-Associative argument'
    goto err_throw
  err_sub:
    errmsg = 'Non-Callable argument'
  err_throw:
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
