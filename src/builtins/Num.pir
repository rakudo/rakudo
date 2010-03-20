## $Id$

=head1 TITLE

Num - Perl 6 numbers

=head2 Methods

=over 4

=cut

.namespace [ 'Num' ]

.sub 'onload' :anon :init :load
    .local pmc p6meta, numproto
    p6meta = get_hll_global ['Mu'], '$!P6META'
    numproto = p6meta.'new_class'('Num', 'parent'=>'parrot;Float Any')

    # Override the proto's ACCEPT method so we also accept Ints.
    .const 'Sub' $P0 = "Num::ACCEPTS"
    $P1 = typeof numproto
    $P1.'add_method'('ACCEPTS', $P0)

    # Map Parrot Float to Rakudo Num
    $P0 = getinterp
    $P1 = get_class ['Float']
    $P2 = get_class ['Num']
    $P0.'hll_map'($P1, $P2)
.end


.sub 'Num::ACCEPTS' :anon :method
    .param pmc topic

    ##  first, try our superclass .ACCEPTS
    $P0 = get_hll_global 'Any'
    $P1 = find_method $P0, 'ACCEPTS'
    $I0 = self.$P1(topic)
    unless $I0 goto try_int
    .return ($I0)

  try_int:
    $P0 = get_hll_global 'Int'
    $I0 = $P0.'ACCEPTS'(topic)
    .return ($I0)
.end

=item succ and pred

Increment and Decrement Methods

=cut

.sub 'pred' :method
    $N0 = self
    dec $N0
    .return ($N0)
.end

.sub 'succ' :method
    $N0 = self
    inc $N0
    .return ($N0)
.end


=item WHICH()

Returns the identify value.

=cut

.sub 'WHICH' :method
    $N0 = self
    .return ($N0)
.end

=back

=head2 Operators

=over 4

=item &infix:<===>

Overridden for Num.

=cut

.namespace []
.sub '&infix:<===>' :multi(Float,Float)
    .param num a
    .param num b
    $I0 = iseq a, b
    .tailcall '&prefix:<?>'($I0)
.end

=back

=head2 Private methods

=over 4

=item !FETCH()

Value type, so return self.

=cut

.sub '!FETCH' :method
    .return (self)
.end


=back

=cut

=item radcalc

=cut

.namespace []

.sub '&radcalc'
    .param int radix
    .param string intpart
    .param string fracpart     :optional
    .param int    has_fracpart :opt_flag
    .param num    base         :optional
    .param int    has_base     :opt_flag
    .param num    exp          :optional
    .param int    has_exp      :opt_flag
    .local num    result, fracdivisor, magnitude
    .local pmc    it

    if radix <= 1 goto err_range
    if radix > 36 goto err_range

    result       = 0.0
    fracdivisor = 1.0

    $P0 = split '', intpart
    it = iter $P0

  lp1: # Accumulate over decimal part
    unless it goto ex1
    $S0 = shift it
    $S0 = downcase $S0
    if $S0 == "_" goto lp1
    $I0 = index "0123456789abcdefghijklmnopqrstuvwxyz", $S0
    if $I0 == -1 goto err_char
    $N0 = $I0
    result *= radix
    result += $N0
    goto lp1

  ex1:
    unless has_fracpart goto nofracpart
    $I0 = length fracpart
    unless $I0 goto nofracpart
    $P0 = split '', fracpart
    $P99 = shift $P0                             # remove the radix point

  lp2: # Accumulate over fractional part, keep length
    unless it goto ex2
    $S0 = shift it
    $S0 = downcase $S0
    if $S0 == "_" goto lp2
    $I0 = index "0123456789abcdefghijklmnopqrstuvwxyz", $S0
    if $I0 == -1 goto err_char
    $N0 = $I0

    result *= radix
    result += $N0
    fracdivisor *= radix
    goto lp2

  ex2:
    result /= fracdivisor

  nofracpart:
    unless has_base goto ret
    magnitude = base ** exp
    result *= magnitude
  ret:
    .return (result)

  err_range:
    die "radix out of range (2-36)"
  err_char:
    $S0 = concat "unrecognized character: ", $S0
    die $S0
.end


# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
