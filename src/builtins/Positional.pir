## $Id$

=head1 NAME

src/classes/Positional.pir - Positional Role

=head1 DESCRIPTION

=cut

.namespace ['Positional[::T]']

.sub '' :load :init
    # Create a parametric role with 1 possible candidate.
    .local pmc role
    .const 'Sub' $P0 = '_positional_role_body'
    role = new ['Perl6Role']
    $P1 = box 'Positional'
    setattribute role, '$!shortname', $P1
    role.'!add_variant'($P0)
    set_hll_global 'Positional', role
.end


# This defines the body of the role, which is run per type the role is
# parameterized with.
.sub '' :anon :subid('_positional_role_body')
    .param pmc type :optional

    # Need to capture the methods that belong in this role.
    .const 'Sub' $P0 = 'Positional::postcircumfix:[Int]'
    capture_lex $P0
    .const 'Sub' $P1 = 'Positional::of'
    capture_lex $P1

    # Capture type.
    if null type goto no_type
    type = type.'WHAT'()
    goto type_done
  no_type:
    type = get_hll_global 'Mu'
  type_done:
    .lex 'T', type

    # Create role.
    .tailcall '!create_parametric_role'("Positional[::T]")
.end
.sub '' :load :init
    .local pmc block, signature
    .const 'Sub' $P0 = '_positional_role_body'
    block = $P0
    signature = allocate_signature 1
    setprop block, "$!signature", signature
    null $P1
    set_signature_elem signature, 0, "T", SIG_ELEM_IS_OPTIONAL, $P1, $P1, $P1, $P1, $P1, $P1
.end


=head2 Operators

=over

=item postcircumfix:<[ ]>

=cut

.sub 'postcircumfix:<[ ]>' :method :multi(_, 'Integer') :outer('_positional_role_body') :subid('Positional::postcircumfix:[Int]')
    .param int index
    .param pmc options         :slurpy :named
    unless index < 0 goto index_ok
    die "Negative indexes to .[] not allowed in Perl 6"
  index_ok:
    .local pmc result
    result = self[index]
    unless null result goto have_result
    result = new ['Perl6Scalar']
  have_result:
    .return (result)
.end

.sub 'postcircumfix:<[ ]>' :method :multi(_, 'Sub')
    .param pmc arg
    .param pmc options         :slurpy :named
    $I0 = elements self
    $P0 = arg($I0)
    .tailcall '!postcircumfix:<[ ]>'(self, $P0, options :named :flat)
.end

.sub 'postcircumfix:<[ ]>' :method :multi(_, 'Whatever')
    .param pmc arg
    .param pmc options         :slurpy :named
    .tailcall 'list'(self)
.end

.sub 'postcircumfix:<[ ]>' :method :multi(_)
    .param pmc options         :slurpy :named
    .tailcall self.'list'()
.end

.sub 'postcircumfix:<[ ]>' :method :multi(_, _)
    .param pmc args            :slurpy
    .param pmc options         :slurpy :named
    .local pmc result
    args = '&eager'(args)
    $I0 = elements args
    if $I0 == 1 goto arg_slice
    result = new ['Parcel']
  args_loop:
    unless args goto args_done
    $P0 = shift args
    $P0 = '!postcircumfix:<[ ]>'(self, $P0, options :named :flat)
    push result, $P0
    goto args_loop
  args_done:
    .return (result)
  arg_slice:
    $P0 = args[0]
    .const 'Sub' $P1 = 'Positional::postcircumfix:[Int]'
    .tailcall self.$P1($P0, options :named :flat)
.end

.sub '' :load :init
    .local pmc block, signature
    .const 'Sub' block1 = 'Positional::postcircumfix:[Int]'
    signature = allocate_signature 0
    setprop block1, "$!signature", signature
.end


=item of

Returns the type constraining what may be stored.

=cut

.sub 'of' :method :outer('_positional_role_body') :subid('Positional::of')
    $P0 = find_lex 'T'
    .return ($P0)
.end
.sub '' :load :init
    .local pmc block, signature
    .const 'Sub' block = 'Positional::of'
    signature = allocate_signature 0
    setprop block, "$!signature", signature
.end


=item !postcircumfix:<[ ]>

Because foreign (non-Rakudo) Parrot objects generally won't
understand the "postcircumfix:<[ ]>" method, we generate
postcircumfix as a private call to this function, and this
function then delegates to the appropriate method.  For PMCs
that don't have a postcircumfix:<[ ]> method, we directly
use the one in Positional.

=cut

.namespace []
.sub '!postcircumfix:<[ ]>'
    .param pmc invocant
    .param pmc args            :slurpy
    $I0 = can invocant, 'postcircumfix:<[ ]>'
    if $I0 goto object_method
    $I0 = isa invocant, 'Mu'
    if $I0 goto object_method
  foreign:
    # XXX not a good idea, this relies on the method being in the namespace
    $P0 = get_hll_global ['Positional[::T]'], 'postcircumfix:<[ ]>'
    .tailcall invocant.$P0(args :flat)
  object_method:
    .tailcall invocant.'postcircumfix:<[ ]>'(args :flat)
.end


=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:

