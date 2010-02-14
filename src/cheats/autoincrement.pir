=head1 NAME

src/cheats/autoincrement.pir - autoincrement and autodecrement operators

=head2 Functions

=over 4

=cut

.namespace []

## autoincrement
.sub '&prefix:<++>' :multi(_) :subid('!prefix:++')
    .param pmc a
    $I0 = defined a
    unless $I0 goto inc_undef
    $P1 = a.'succ'()
    .tailcall '&infix:<=>'(a, $P1)
  inc_undef:
    .tailcall '&infix:<=>'(a, 1)
.end

.sub '&postfix:<++>' :multi(_) :subid('!postfix:++')
    .param pmc a
    $P0 = a.'clone'()
    .const 'Sub' $P1 = '!prefix:++'
    $P1(a)
    .return ($P0)
.end

.sub '&prefix:<-->' :multi(_) :subid('!prefix:--')
    .param pmc a
    $I0 = defined a
    unless $I0 goto dec_undef
    $P1 = a.'pred'()
    .tailcall '&infix:<=>'(a, $P1)
  dec_undef:
    .tailcall '&infix:<=>'(a, -1)
.end

.sub '&postfix:<-->' :multi(_)
    .param pmc a
    $P0 = a.'clone'()
    .const 'Sub' $P1 = '!prefix:--'
    $P1(a)
    .return ($P0)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
