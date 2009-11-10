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
.sub 'WhateverCodeX' :anon
    .param string opname
    .param pmc left
    .param pmc right       :optional
    .local pmc opfunc
    opfunc = find_name opname
    .lex '$opfunc', opfunc
    .lex '$left', left
    .lex '$right', right
    .const 'Sub' $P0 = '!whatever_closure'
    $P1 = newclosure $P0
    '!fixup_routine_type'($P1, 'WhateverCode')
    .return ($P1)
.end
.sub '!whatever_closure' :anon :outer('WhateverCodeX')
    .param pmc arg
    .local pmc opfunc, left, right
    opfunc = find_lex '$opfunc'
    left   = find_lex '$left'
    right  = find_lex '$right'
    left   = '!whatever_eval'(left, arg)
    if null right goto unary
    right  = '!whatever_eval'(right, arg)
    .tailcall opfunc(left, right)
  unary:
    .tailcall opfunc(left)
.end
.sub '!whatever_eval' :multi(_)
    .param pmc whatever
    .param pmc arg
    .return (whatever)
.end
.sub '!whatever_eval' :multi('Whatever')
    .param pmc whatever
    .param pmc arg
    .return (arg)
.end
.sub '!whatever_eval' :multi('WhateverCode')
    .param pmc whatever
    .param pmc arg
    .tailcall whatever(arg)
.end

=over 4

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
