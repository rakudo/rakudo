## $Id$

=head1 NAME

src/builtins/num.pir -  C<Num>-like builtin functions and methods

=head1 DESCRIPTION

This file implements the methods and functions of C<Any> that
are most closely associated with the C<Num> class or role.
We place them here instead of L<src/classes/Any.pir> to keep
the size of that file down and to emphasize that they're
builtins.

=head2 Methods

=cut

.namespace ['Any']
.sub 'onload' :anon :init :load
    $P0 = get_hll_namespace ['Any']
    '!EXPORT'('abs', 'from'=>$P0)
.end


=item abs()

=cut

.namespace ['Any']
.sub 'abs' :method :multi(_)
    $N0 = self
    $N1 = abs $N0
    .return ($N1)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
