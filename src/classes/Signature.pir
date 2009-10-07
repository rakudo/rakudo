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

=item perl

Gets a perl representation of the signature.

=cut

.sub 'perl' :method
    .local pmc s
    s = new ['Str']
    concat s, ':('

    # Various bits of state we'll want.
    .local int last_was_multi_inv, want_colon, first
    last_was_multi_inv = 1
    want_colon = 0
    first = 1

    # Grab low level signature we're wrapping.
    .local pmc signature
    signature = getattribute self, '$!ll_sig'

    # Loop over parameters.
    .local int cur_param, count
    count = get_signature_size signature
    cur_param = -1
  param_loop:
    inc cur_param
    unless cur_param < count goto param_done

    # Get all curent parameter info.
    .local pmc nom_type, cons_type, names
    .local int flags, optional, multi_invocant, slurpy
    .local string name
    get_signature_elem signature, cur_param, name, flags, nom_type, cons_type, names, $P1
    optional       = flags & SIG_ELEM_IS_OPTIONAL
    multi_invocant = flags & SIG_ELEM_MULTI_INVOCANT
    slurpy         = flags & SIG_ELEM_SLURPY

    # If it's the first time, no separator.
    if first goto first_time
    if want_colon goto emit_colon
    if multi_invocant goto emit_comma
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
    if null nom_type goto any_type
    $I0 = isa nom_type, 'Role'
    unless $I0 goto type_as_is
    $S0 = substr name, 0, 1
    if $S0 == '$' goto type_as_is
    $S1 = nom_type.'perl'()
    $I0 = index $S1, '['
    inc $I0
    $I1 = length $S1
    $I1 -= $I0
    dec $I1
    $S1 = substr $S1, $I0, $I1
    concat s, $S1
    goto type_done
  type_as_is:
    $P0 = nom_type.'perl'()
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
    unless slurpy goto slurpy_done
    concat s, '*'
    goto named_done
  slurpy_done:

    # If it's named, the :. XXX Handle different naming/multiple names.
    if null names goto named_done
    concat s, ':'
  named_done:

    # Now the name.
    concat s, name

    # If it's optional, the ?. XXX Fix named case for non-optional.
    unless optional goto optional_done
    concat s, '?'
  optional_done:

    # Now any constraints.
    if null cons_type goto constraints_done
    unless cons_type goto constraints_done
    concat s, " where "
    $P0 = cons_type.'perl'()
    concat s, $P0
  constraints_done:

    goto param_loop
  param_done:

    # If we just had an invocant, need the colon.
    unless want_colon goto no_trailing_colon
    concat s, ':'
  no_trailing_colon:

    # Done.
    concat s, ')'
    .return (s)
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
