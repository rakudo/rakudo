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
    $S0 = join '', args
    say $S0
    .return (1)
.end

.sub '&print'
    .param pmc args :slurpy
    $S0 = join '', args
    print $S0
    .return (1)
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:

