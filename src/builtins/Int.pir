## $Id$

=head1 TITLE

Int - Perl 6 integers

=head2 Methods

=over 4

=cut

.namespace [ 'Int' ]

.sub 'onload' :anon :init :load
    .local pmc p6meta, intproto
    p6meta = get_hll_global ['Mu'], '$!P6META'
    intproto = p6meta.'new_class'('Int', 'parent'=>'parrot;Integer Any')
.end

=item ACCEPTS()

=cut

.sub 'ACCEPTS' :method
    .param num topic
    .tailcall '&infix:<==>'(topic, self)
.end

=item perl()

Returns a Perl representation of the Int.

=cut

.sub 'perl' :method
    $S0 = self
    .return($S0)
.end


=item succ and pred

Increment and Decrement Methods

=cut

.sub 'pred' :method
    $N0 = self
    dec $N0
    .tailcall '!upgrade_to_num_if_needed'($N0)
.end

.sub 'succ' :method
    $N0 = self
    inc $N0
    .tailcall '!upgrade_to_num_if_needed'($N0)
.end


=item WHICH()

Returns the identify value.

=cut

.sub 'WHICH' :method
    $I0 = self
    .return ($I0)
.end

=back

=head2 Operators

=over 4

=item &infix:<===>

Overridden for Int.

=cut

.namespace []
.sub '&infix:<===>' :multi(Integer,Integer)
    .param int a
    .param int b
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

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
