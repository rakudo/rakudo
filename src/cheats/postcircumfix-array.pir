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
    $I0 = isa invocant, 'Perl6Object'
    if $I0 goto object_method
  foreign:
    die "Can't postcircumfix:<[ ]> foreign objects yet."
  object_method:
    .tailcall invocant.'postcircumfix:<[ ]>'(args :flat)
.end

