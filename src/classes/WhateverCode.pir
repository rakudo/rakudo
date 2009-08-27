## $Id$

=head1 TITLE

WhateverCode - Blocks that delay evaluation of whatever results

=head1 DESCRIPTION

This file sets up the Perl 6 C<WhateverCode> class, the class for 
C<Whatever> operations.

=cut

.namespace ['WhateverCode']

.sub 'onload' :anon :load :init
    .local pmc p6meta
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    p6meta.'new_class'('WhateverCode', 'parent'=>'Code')
.end


.namespace []

.sub 'WhateverCodeX'
    .param string opname
    .param pmc a
    .param pmc b
    .tailcall '!whatever_helper'(opname, a, b)
.end

=over 4

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
