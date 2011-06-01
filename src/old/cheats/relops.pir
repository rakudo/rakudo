## $Id$

=head1 NAME

src/cheats/cmp.pir - Perl6 comparison builtins

=head1 Functions

=over 4

=cut

.namespace []

.sub '&infix:<===>' :multi(_,_)
    .param pmc a
    .param pmc b
    $I0 = '!have_exact_same_type'(a, b)
    unless $I0 goto false
    $P0 = a.'WHICH'()
    $P1 = b.'WHICH'()
    .tailcall '&infix:<===>'($P0, $P1)
  false:
    $P0 = get_hll_global [ 'Bool' ], 'False'
    .return ($P0)
.end


.sub '&infix:<!===>' :multi(_,_)
    .param pmc a
    .param pmc b
    $P0 = '&infix:<===>'(a, b)
    $P1 = not $P0
    .return ($P1)
    # .tailcall '&prefix:!'($P0)
.end


.sub '&infix:<=:=>' :multi(_,_)
    .param pmc a
    .param pmc b
    $I0 = issame a, b
    unless $I0 goto false
    $P0 = get_hll_global [ 'Bool' ], 'True'
    goto done
  false:
    $P0 = get_hll_global [ 'Bool' ], 'False'
  done:
    .return ($P0)
.end


.sub '&infix:<!=:=>' :multi(_,_)
    .param pmc a
    .param pmc b
    $P0 = '&infix:<=:=>'(a, b)
    .tailcall '&prefix:!'($P0)
.end


=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
