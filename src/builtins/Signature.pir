## $Id$

=head1 TITLE

Signature - Perl 6 Signature class

=head1 DESCRIPTION

This file sets up the high level Perl 6 C<Signature> class. It wraps around a
P6LowLevelSig and provides higher level access to it.

=cut

.namespace ['Signature']

.sub 'onload' :anon :init :load
    .local pmc p6meta
    p6meta = get_hll_global ['Mu'], '$!P6META'
    p6meta.'new_class'('Signature', 'parent'=>'Any', 'attr'=>'$!ll_sig $!param_cache $!try_bind_sub $!bind_target')
.end


=head2 Methods

=over 4

=item params

Returns a C<List> of C<Parameter> descriptors.

=cut

.sub 'params' :method
    # Did we compute this before?
    .local pmc result
    result = getattribute self, '$!param_cache'
    if result == 'Mu()' goto compute_result
    .return (result)
  compute_result:

    # Create result.
    result = new ['Parcel']

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
    .local pmc nom_type, cons_type, names, type_captures, default, sub_sig
    .local int flags, optional, invocant, multi_invocant, slurpy, rw, parcel, capture, copy, named
    .local string name, coerce_to
    get_signature_elem signature, cur_param, name, flags, nom_type, cons_type, names, type_captures, default, sub_sig, coerce_to
    optional       = flags & SIG_ELEM_IS_OPTIONAL
    invocant       = flags & SIG_ELEM_INVOCANT
    multi_invocant = flags & SIG_ELEM_MULTI_INVOCANT
    slurpy         = flags & SIG_ELEM_SLURPY
    rw             = flags & SIG_ELEM_IS_RW
    copy           = flags & SIG_ELEM_IS_COPY
    parcel         = flags & SIG_ELEM_IS_PARCEL
    capture        = flags & SIG_ELEM_IS_CAPTURE

    # Make sure constraints is non-null.
    unless null cons_type goto have_cons
    cons_type = get_hll_global ['Bool'], 'True'
    goto cons_done
  have_cons:
    cons_type = '&list'(cons_type :flat)
  cons_done:

    # Any names?
    named = 0
    if null names goto no_names
    named = 1
    names = '&list'(names :flat)
    goto names_done
  no_names:
    names = '&list'()
    $I0 = flags & SIG_ELEM_SLURPY_NAMED
    unless $I0 goto names_done
    named = 1
  names_done:

    # Any type captures?
    if null type_captures goto no_type_captures
    type_captures = '&list'(type_captures :flat)
    goto type_captures_done
  no_type_captures:
    type_captures = '&list'()
  type_captures_done:

    # Make sure default and sub-signature are non-null.
    unless null default goto default_done
    default = '!FAIL'()
  default_done:
    unless null sub_sig goto sub_sig_done
    sub_sig = '!FAIL'()
  sub_sig_done:

    # Create parameter instance.
    $P0 = parameter.'new'('name'=>name, 'type'=>nom_type, 'constraints'=>cons_type, 'optional'=>optional, 'slurpy'=>slurpy, 'invocant'=>invocant, 'multi_invocant'=>multi_invocant, 'rw'=>rw, 'parcel'=>parcel, 'capture'=>capture, 'copy'=>copy, 'named'=>named, 'named_names'=>names, 'type_captures'=>type_captures, 'default'=>default, 'signature'=>sub_sig)
    push result, $P0
    goto param_loop
  param_done:

    # Cache and return.
    setattribute self, '$!param_cache', result
    .return (result)
.end


=item !BIND

Binds the signature into the given bind target.

=cut

.sub '!BIND' :method
    .param pmc capture
    .param pmc adverbs :named :slurpy

    # Get hold of the bind testing sub.
    $P0 = getattribute self, '$!try_bind_sub'
    $I0 = defined $P0
    if $I0 goto have_try_bind_sub
    $P0 = self.'!make_try_bind_sub'()
  have_try_bind_sub:

    # Attempt to bind and get back a hash of the bound variables.
    .local pmc bound
    bound = $P0(capture)

    # Update the target.
    .local pmc target, bound_it
    target = getattribute self, '$!bind_target'
    $I0 = defined target
    unless $I0 goto done
    bound_it = iter bound
  bound_it_loop:
    unless bound_it goto bound_it_loop_end
    $S0 = shift bound_it
    $P0 = bound[$S0]
    target[$S0] = $P0
    goto bound_it_loop
  bound_it_loop_end:

  done:
    .return (bound)
.end


=item !make_try_bind_sub

This is terrifying. To try binding a signature, we want to have a sub so we
have a proper lex pad to bind against, and constraints will work. However,
it can't be the actual sub the signature is attached to, and we won't always
need it, So, we'll "just" manufacture one on demand.

=cut

.sub '!make_try_bind_sub' :method
    .local pmc params, param_it
    .local string pir

    # Opening.
    pir = <<'PIR'
.sub ''
    .param pmc capture
PIR

    # Emit bound variables.
    $I0 = 1
    params = self.'params'()
    param_it = iter params
  it_loop:
    unless param_it goto it_loop_end
    $P0 = shift param_it
    $S0 = $P0.'name'()
    if null $S0 goto it_loop
    if $S0 == '' goto it_loop
    concat pir, '    $P'
    $S1 = $I0
    concat pir, $S1
    concat pir, " = new ['Perl6Scalar']\n    .lex '"
    concat pir, $S0
    concat pir, "', $P"
    concat pir, $S1
    concat pir, "\n"
    inc $I0
    goto it_loop
  it_loop_end:

    # Ending.
    pir = concat <<'PIR'
    bind_signature capture
    $P0 = getinterp
    $P0 = $P0['lexpad']
    .return ($P0)
.end
PIR
    
    # Compile and return.
    $P0 = compreg 'PIR'
    $P0 = $P0(pir)
    $P0 = $P0[0]
    $P1 = getattribute self, '$!ll_sig'
    $P1 = descalarref $P1
    setprop $P0, '$!signature', $P1
    .return ($P0)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
