## $Id$

=head1 NAME

src/classes/Associative.pir - Associative Role

=head1 DESCRIPTION

Actually these days, Associative is defined in Perl 6 and this is
just a postcircumfix:<{ }> for non-Perl 6 types mapper.

=cut

=item !postcircumfix:<{ }>

Because foreign (non-Rakudo) Parrot objects generally won't
understand the "postcircumfix:<{ }>" method, we generate
postcircumfix as a private call to this function, and this
function then delegates to the appropriate method.  In the
case we want a single value, then it just does the lookup;
otherwise, we rely on the method dispatches for the complex
cases looping back to here to get the one value.

=cut

.namespace []
.sub '!postcircumfix:<{ }>'
    .param pmc invocant
    .param pmc args            :slurpy
    $I0 = can invocant, 'postcircumfix:<{ }>'
    if $I0 goto object_method
    $I0 = isa invocant, 'Mu'
    if $I0 goto object_method
  foreign:
    $I0 = elements args
    if $I0 != 1 goto delegate
    $P0 = args[0]
    $P0 = invocant[$P0]
    unless null $P0 goto done
    $P0 = new ['Perl6Scalar']
  done:
    .return ($P0)
  delegate:
    # XXX relies on the method being in the namespace -- perhaps
    # should use method lookup instead
    $P0 = get_hll_global ['Associative[::T]'], 'postcircumfix:<{ }>'
    .tailcall invocant.$P0(args :flat)
  object_method:
    .tailcall invocant.'postcircumfix:<{ }>'(args :flat)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:

