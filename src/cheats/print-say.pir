## $Id$

=head1 NAME

src/s1_setting.pir -  A few built-ins specific to Stage 1

=head1 DESCRIPTION

This file contains various subs and/or methods that we want to make
available to the Stage 1 compiler. Anything in this file is not included
in the final Rakudo PBC, just in the PBC for perl6_s1.pbc.

=cut

.namespace []

.sub '&say'
    .param pmc args :slurpy
    $P0 = args.'list'()
    $S0 = join '', $P0
    say $S0
    .return (1)
.end

.sub '&print'
    .param pmc args :slurpy
    $P0 = args.'list'()
    $S0 = join '', $P0
    print $S0
    .return (1)
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:

