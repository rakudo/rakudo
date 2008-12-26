## $Id$

=head1 NAME

src/builtins/cmp.pir - Perl6 comparison builtins

=head1 Functions

=over 4

=cut

.namespace []

.sub 'infix:==' :multi(_,_)
    .param num a
    .param num b
    $I0 = iseq a, b
    .tailcall 'prefix:?'($I0)
.end


.sub 'infix:!==' :multi(_,_)
    .param num a
    .param num b
    $I0 = isne a, b
    .tailcall 'prefix:?'($I0)
.end

# Shortcut for infix:!==, so same code
.sub 'infix:!=' :multi(_,_)
    .param num a
    .param num b
    $I0 = isne a, b
    .tailcall 'prefix:?'($I0)
.end


.sub 'infix:<' :multi(_,_)
    .param num a
    .param num b
    $I0 = islt a, b
    .tailcall 'prefix:?'($I0)
.end


.sub 'infix:<=' :multi(_,_)
    .param num a
    .param num b
    $I0 = isle a, b
    .tailcall 'prefix:?'($I0)
.end


.sub 'infix:>' :multi(_,_)
    .param num a
    .param num b
    $I0 = isgt a, b
    .tailcall 'prefix:?'($I0)
.end


.sub 'infix:>=' :multi(_,_)
    .param num a
    .param num b
    $I0 = isge a, b
    .tailcall 'prefix:?'($I0)
.end


.sub 'infix:<=>' :multi(_,_)
    .param pmc a
    .param pmc b
    $I0 = cmp_num a, b
    if $I0 < 0 goto increase
    if $I0 > 0 goto decrease
    $P0 = get_hll_global ['Order'], 'Same'
    .return ($P0)
  increase:
    $P0 = get_hll_global ['Order'], 'Increase'
    .return ($P0)
  decrease:
    $P0 = get_hll_global ['Order'], 'Decrease'
    .return ($P0)
.end


.sub 'infix:eq' :multi(_,_)
    .param string a
    .param string b
    $I0 = iseq a, b
    .tailcall 'prefix:?'($I0)
.end

.sub 'infix:!eq' :multi(_,_)
    .param string a
    .param string b
    $I0 = isne a, b
    .tailcall 'prefix:?'($I0)
.end


.sub 'infix:ne' :multi(_,_)
    .param string a
    .param string b
    $I0 = isne a, b
    .tailcall 'prefix:?'($I0)
.end


.sub 'infix:lt' :multi(_,_)
    .param string a
    .param string b
    $I0 = islt a, b
    .tailcall 'prefix:?'($I0)
.end


.sub 'infix:le' :multi(_,_)
    .param string a
    .param string b
    $I0 = isle a, b
    .tailcall 'prefix:?'($I0)
.end


.sub 'infix:gt' :multi(_,_)
    .param string a
    .param string b
    $I0 = isgt a, b
    .tailcall 'prefix:?'($I0)
.end


.sub 'infix:ge' :multi(_,_)
    .param string a
    .param string b
    $I0 = isge a, b
    .tailcall 'prefix:?'($I0)
.end


.sub 'infix:cmp' :multi(_,_)
    .param pmc a
    .param pmc b
    $I0 = cmp a, b
    ##  Don't use a tailcall here due to RT#56448
    $P0 = 'infix:<=>'($I0, 0)
    .return ($P0)
.end


.sub 'infix:leg' :multi(_,_)
    .param string a
    .param string b
    $I0 = cmp a, b
    ##  Don't use a tailcall here due to RT#56448
    $P0 = 'infix:<=>'($I0, 0)
    .return ($P0)
.end


.sub 'infix:===' :multi(_,_)
    .param pmc a
    .param pmc b
    $I0 = '!SAMETYPE_EXACT'(a, b)
    unless $I0 goto false
    $P0 = a.'WHICH'()
    $P1 = b.'WHICH'()
    .tailcall 'infix:==='($P0, $P1)
  false:
    $P0 = get_hll_global [ 'Bool' ], 'False'
    .return ($P0)
.end


.sub 'infix:!===' :multi(_,_)
    .param pmc a
    .param pmc b
    $P0 = 'infix:==='(a, b)
    .tailcall 'prefix:!'($P0)
.end


.sub 'infix:=:=' :multi(_,_)
    .param pmc a
    .param pmc b
    $I0 = issame a, b
    .return ($I0)
.end


.sub 'infix:!=:=' :multi(_,_)
    .param pmc a
    .param pmc b
    $P0 = 'infix:=:='(a, b)
    .tailcall 'prefix:!'($P0)
.end


=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
