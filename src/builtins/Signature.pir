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
    p6meta.'new_class'('Signature', 'parent'=>'Any', 'attr'=>'$!ll_sig $!param_cache')
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
    .local string name
    get_signature_elem signature, cur_param, name, flags, nom_type, cons_type, names, type_captures, default, sub_sig
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

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
