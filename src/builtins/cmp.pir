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
    .return 'prefix:?'($I0)
.end


.sub 'infix:!==' :multi(_,_)
    .param num a
    .param num b
    $I0 = isne a, b
    .return 'prefix:?'($I0)
.end

# Shortcut for infix:!==, so same code
.sub 'infix:!=' :multi(_,_)
    .param num a
    .param num b
    $I0 = isne a, b
    .return 'prefix:?'($I0)
.end


.sub 'infix:<' :multi(_,_)
    .param num a
    .param num b
    $I0 = islt a, b
    .return 'prefix:?'($I0)
.end


.sub 'infix:<=' :multi(_,_)
    .param num a
    .param num b
    $I0 = isle a, b
    .return 'prefix:?'($I0)
.end


.sub 'infix:>' :multi(_,_)
    .param num a
    .param num b
    $I0 = isgt a, b
    .return 'prefix:?'($I0)
.end


.sub 'infix:>=' :multi(_,_)
    .param num a
    .param num b
    $I0 = isge a, b
    .return 'prefix:?'($I0)
.end


.sub 'infix:<=>'
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
    .return 'prefix:?'($I0)
.end

.sub 'infix:!eq' :multi(_,_)
    .param string a
    .param string b
    $I0 = isne a, b
    .return 'prefix:?'($I0)
.end


.sub 'infix:ne' :multi(_,_)
    .param string a
    .param string b
    $I0 = isne a, b
    .return 'prefix:?'($I0)
.end


.sub 'infix:lt' :multi(_,_)
    .param string a
    .param string b
    $I0 = islt a, b
    .return 'prefix:?'($I0)
.end


.sub 'infix:le' :multi(_,_)
    .param string a
    .param string b
    $I0 = isle a, b
    .return 'prefix:?'($I0)
.end


.sub 'infix:gt' :multi(_,_)
    .param string a
    .param string b
    $I0 = isgt a, b
    .return 'prefix:?'($I0)
.end


.sub 'infix:ge' :multi(_,_)
    .param string a
    .param string b
    $I0 = isge a, b
    .return 'prefix:?'($I0)
.end


.sub 'infix:cmp'
    .param pmc a
    .param pmc b
    $I0 = cmp a, b
    .return 'infix:<=>'($I0, 0)
.end


.sub 'infix:leg'
    .param string a
    .param string b
    $I0 = cmp a, b
    .return 'infix:<=>'($I0, 0)
.end


## TODO: infix:=:= infix:===


=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
