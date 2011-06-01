## $Id$

=head1 NAME

src/classes/Positional.pir - Positional Role

=head1 DESCRIPTION

=cut

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
    .param pmc args            :optional
    .param int has_args        :opt_flag

    $I0 = can invocant, 'postcircumfix:<[ ]>'
    if $I0 goto object_method
    $I0 = isa invocant, 'Mu'
    if $I0 goto object_method
  foreign:
    $P0 = get_hll_global ['Positional[::T]'], 'postcircumfix:<[ ]>'
    unless has_args goto foreign_zen
    $P1 = invocant.$P0(args)
    .return ($P1)
  foreign_zen:
    $P1 = invocant.$P0()
    .return ($P1)
  object_method:
    unless has_args goto object_zen
    $P1 = invocant.'postcircumfix:<[ ]>'(args)
    .return ($P1)
  object_zen:
    $P1 = invocant.'postcircumfix:<[ ]>'()
    .return ($P1)
.end


=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:

