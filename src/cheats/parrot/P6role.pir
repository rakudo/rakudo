## $Id$

=head1 NAME

src/parrot/P6role.pir - methods for the our P6role class

=head2 Methods on P6role

We also add some methods to P6role.

=item !pun

Puns the role to a class and returns that class.

=cut

.namespace ["P6role"]
.sub '!pun' :method
    self = descalarref self

    # See if we have already created a punned class; use it if so.
    .local pmc pun
    pun = getprop '$!pun', self
    if null pun goto make_pun
    .return (pun)
  make_pun:

    # Otherwise, need to create a punned class.
    .local pmc ClassHOW, meta, proto
    ClassHOW = get_root_global ['perl6'], 'ClassHOW'
    meta = ClassHOW.'new'() # XXX Name?
    ClassHOW.'add_composable'(meta, self)
    proto = ClassHOW.'compose'(meta)
    
    # Stash it away, and return it.
    setprop self, '$!pun', proto
    .return (proto)
.end


=item ACCEPTS

=cut

.sub 'ACCEPTS' :method
    .param pmc topic

    # First, check if this role is directly done by the topic.
    $I0 = does topic, self
    if $I0 goto done

    # Otherwise, need to consider subtypes in the parameters.
    .local pmc all_variants, it, want_rf, our_types, cur_variant
    self = descalarref self
    $P0 = getprop '$!owner', self
    all_variants = getattribute $P0, '$!created'
    want_rf = getprop '$!orig_role', self
    our_types = getprop '@!type_args', self
    it = iter all_variants
  it_loop:
    unless it goto it_loop_end
    cur_variant = shift it

    # We can exclude a variant if it wasn't from the same role factory.
    $P0 = cur_variant['role']
    $P1 = getprop '$!orig_role', $P0
    eq_addr $P1, want_rf, same_variant
    goto it_loop
  same_variant:

    # Also we can exclude it if our topic doens't do it.
    $I0 = does topic, $P0
    unless $I0 goto it_loop

    # If it's from the same variant, check all types of the role we're
    # considering here are broader-or-equal types.
    .local pmc check_types
    check_types = cur_variant['pos_args']
    $I0 = elements check_types
    $I1 = elements our_types
    if $I0 != $I1 goto it_loop
    $I0 = 0
  type_loop:
    if $I0 >= $I1 goto type_loop_end
    $P0 = our_types[$I0]
    $P1 = check_types[$I0]
    $I2 = $P0.'ACCEPTS'($P1)
    unless $I2 goto it_loop
    inc $I0
    goto type_loop
  type_loop_end:

    # If we get here, we found a role that through the subtypes of its
    # parameters is applicable.
    $I0 = 1
    goto done
  it_loop_end:

    # If we get here, no applicable roles.
    $I0 = 0
  done:
    .return ($I0)
.end
.sub 'REJECTS' :method
    .param pmc topic
    $P0 = self.'ACCEPTS'(topic)
    $I0 = isfalse $P0
    .return ($I0)
.end


=item perl

=cut

.sub 'perl' :method
    .local pmc args, it
    self = descalarref self
    $P0 = getprop '$!owner', self
    $P0 = getattribute $P0, '$!shortname'
    $S0 = $P0
    $S0 = concat $S0, '['
    args = getprop '@!type_args', self
    it = iter args
  it_loop:
    unless it goto it_loop_end
    $P0 = shift it
    $S1 = $P0.'perl'()
    $S0 = concat $S1
    goto it_loop
  it_loop_end:
    $S0 = concat ']'
    .return ($S0)
.end


=item HOW

=cut

.sub 'HOW' :method
    self = descalarref self
    $P0 = getprop 'metaclass', self
    .return ($P0)
.end


=item WHICH

=cut

.sub 'WHICH' :method
    $I0 = get_addr self
    .return ($I0)
.end


=item WHAT

=cut

.sub 'WHAT' :method
    self = descalarref self
    .return (self)
.end


=item Str (vtable get_string)

=cut

.sub 'Str' :method :vtable('get_string')
    self = descalarref self
    $P0 = getprop '$!owner', self
    $S0 = $P0
    .return ($S0)
.end


=item postcircumfix:<[ ]>

=cut

.sub 'postcircumfix:<[ ]>' :method
    .return (self)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
