## $Id$

=head1 TITLE

Signature - Perl 6 Signature class

=head1 DESCRIPTION

This file sets up the high level Perl 6 C<Signature> class. It wraps around a
P6LowLevelSig and provides higher level access to it.

=cut

.namespace ['Signature']

.sub 'onload' :anon :init :load
    load_bytecode 'PCT.pbc'
    .local pmc p6meta
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    p6meta.'new_class'('Signature', 'parent'=>'Any', 'attr'=>'$!ll_sig')
.end


=head2 Methods

=over 4

=item params

Returns a C<List> of C<Parameter> descriptors.

=cut

.sub 'params' :method
    # Create result.
    .local pmc result
    result = new 'ResizablePMCArray'

    # Grab low level signature we're wrapping.
    .local pmc signature
    signature = getattribute self, '$!ll_sig'
    signature = descalarref signature

    # And Parameter proto.
    .local pmc parameter
    parameter = get_hll_global 'Parameter'

    # Loop over parameters.
    .local int cur_param, count
    count = get_signature_size signature
    cur_param = -1
  param_loop:
    inc cur_param
    unless cur_param < count goto param_done

    # Get all curent parameter info.
    .local pmc nom_type, cons_type, names, type_captures
    .local int flags, optional, invocant, multi_invocant, slurpy, rw, ref, copy, named
    .local string name
    get_signature_elem signature, cur_param, name, flags, nom_type, cons_type, names, type_captures
    optional       = flags & SIG_ELEM_IS_OPTIONAL
    invocant       = flags & SIG_ELEM_INVOCANT
    multi_invocant = flags & SIG_ELEM_MULTI_INVOCANT
    slurpy         = flags & SIG_ELEM_SLURPY
    rw             = flags & SIG_ELEM_IS_RW
    ref            = flags & SIG_ELEM_IS_REF
    copy           = flags & SIG_ELEM_IS_COPY

    # Make sure constraints is non-null.
    unless null cons_type goto cons_done
    cons_type = 'undef'()
  cons_done:

    # Any names?
    named = 0
    if null names goto no_names
    named = 1
    names = 'list'(names :flat)
    goto names_done
  no_names:
    names = 'list'()
    $I0 = flags & SIG_ELEM_SLURPY_NAMED
    unless $I0 goto names_done
    named = 1
  names_done:

    # Any type captures?
    if null type_captures goto no_type_captures
    type_captures = 'list'(type_captures :flat)
    goto type_captures_done
  no_type_captures:
    type_captures = 'list'()
  type_captures_done:

    # Create parameter instance. XXX Missing $.default, $.signature
    $P0 = parameter.'new'('name'=>name, 'type'=>nom_type, 'constraint'=>cons_type, 'optional'=>optional, 'slurpy'=>slurpy, 'invocant'=>invocant, 'multi_invocant'=>multi_invocant, 'rw'=>rw, 'ref'=>ref, 'copy'=>copy, 'named'=>named, 'named_names'=>names, 'type_captures'=>type_captures)
    push result, $P0
    goto param_loop
  param_done:

    # Turn into a List.
    .tailcall 'list'(result :flat)
.end


=item !SIGNATURE_BIND

Analyze the signature of the caller, (re)binding the caller's
lexicals as needed and performing type checks.

XXX Note that this will be going away in the near future as part
of the overall signature binding refactor.

=cut

.namespace []
.sub '!SIGNATURE_BIND'
    # Get hold of caller's info and sig.
    .local pmc callersub, callerlex, callersig
    $P0 = getinterp
    callersub = $P0['sub';1]
    callerlex = $P0['lexpad';1]
    getprop callersig, '$!signature', callersub
    if null callersig goto end

    # Loop over parameters.
    .local int cur_param, count
    count = get_signature_size callersig
    cur_param = -1
  param_loop:
    inc cur_param
    unless cur_param < count goto param_done
    
    # Get all curent parameter info.
    .local pmc nom_type, cons_type
    .local int flags, optional
    .local string name
    get_signature_elem callersig, cur_param, name, flags, nom_type, cons_type, $P0, $P1
    optional = flags & SIG_ELEM_IS_OPTIONAL

    # Skip invocant.
    if name == 'self' goto param_loop
    
    # Get hold of some info we'll need about the value.
    .local pmc orig, var
    .local string sigil
    sigil = substr name, 0, 1
    orig = callerlex[name]

    # Go by sigil...
    if sigil == '@' goto param_array
    if sigil == '%' goto param_hash
    
    # Scalar.
    var = '!CALLMETHOD'('Scalar', orig)
    ##  typecheck the argument unless it's undef (for optional parameter)
    unless optional goto not_optional
    $I0 = defined orig
    unless $I0 goto param_val_done
  not_optional:
    .lex '$/', $P99
    $P0 = nom_type.'ACCEPTS'(var)
    unless $P0 goto err_param_type
    if null cons_type goto param_val_done
    $P0 = cons_type.'ACCEPTS'(var)
    unless $P0 goto err_param_type
    goto param_val_done

    # Array.
  param_array:
    $P0 = nom_type.'ACCEPTS'(orig)
    unless $P0 goto err_param_type_non_scalar
    if null cons_type goto param_array_types_done
    $P0 = cons_type.'ACCEPTS'(orig)
    unless $P0 goto err_param_type_non_scalar
  param_array_types_done:
    var = descalarref orig
    var = '!CALLMETHOD'('Array', var)
    goto param_val_done

    # Hash.
  param_hash:
    $P0 = nom_type.'ACCEPTS'(orig)
    unless $P0 goto err_param_type_non_scalar
    if null cons_type goto param_hash_types_done
    $P0 = cons_type.'ACCEPTS'(orig)
    unless $P0 goto err_param_type_non_scalar
  param_hash_types_done:
    var = descalarref orig
    var = '!CALLMETHOD'('Hash', var)
    goto param_val_done
  param_val_done:

    ## handle readonly/copy traits
    $I0 = flags & SIG_ELEM_IS_RW
    if $I0 goto param_readtype_done
    $I0 = flags & SIG_ELEM_IS_COPY
    if $I0 goto param_readtype_copy
    ne_addr orig, var, param_readtype_var
    var = root_new ['parrot';'ObjectRef'], var
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
    $P0 = new ['Perl6Array']
    'infix:='($P0, var)
    var = $P0
    goto param_readtype_done
  param_readtype_copy_hash:
    $P0 = new ['Perl6Hash']
    'infix:='($P0, var)
    var = $P0
  param_readtype_done:

    ## set any type properties
    setprop var, 'type', nom_type

    ## place the updated variable back into lex
    callerlex[name] = var
    goto param_loop

  param_done:
  end:

    # In theory we're done now, however we may be doing only a bindability check
    # for the purposes of MMD. In that case, throw a resumable exception here.
    $P0 = getprop '$!bind_check_only', callersub
    if null $P0 goto done
    die '__BIND_SUCCESSFUL__' # XXX A little fragile...think of something better
  done:
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
    .local string errmsg, callername
    errmsg = '!make_type_fail_message'('Parameter', orig, nom_type)
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
