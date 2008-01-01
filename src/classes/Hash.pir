## $Id$

=head1 NAME

src/classes/Hash.pir - Perl 6 Hash class

=head1 Methods

=over 4

=cut

.namespace ['Hash']

.sub 'onload' :anon :load :init
    $P0 = subclass 'Hash', 'Perl6Hash'
    $P1 = get_hll_global ['Perl6Object'], 'make_proto'
    $P1($P0, 'Hash')
.end


.sub 'get_string' :vtable :method
    $S0 = ''
    .local pmc iter
    iter = new 'Iterator', self
  loop:
    unless iter goto end
    $S1 = shift iter
    $S2 = iter[$S1]
    $S0 = concat $S0, $S1
    concat $S0, "\t"
    concat $S0, $S2
    concat $S0, "\n"
    goto loop
  end:
    .return ($S0)
.end


## FIXME:  Parrot currently requires us to write our own "clone" method.
.sub 'clone' :vtable :method
    $P0 = new 'Perl6Hash'
    .local pmc iter
    iter = new 'Iterator', self
  loop:
    unless iter goto end
    $P1 = shift iter
    $P2 = iter[$P1]
    $P0[$P1] = $P2
    goto loop
  end:
    .return ($P0)
.end


=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
