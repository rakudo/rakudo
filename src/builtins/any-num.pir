## $Id$

=head1 NAME

src/builtins/any_num.pir -  C<Num>-like functions and methods for C<Any>

=head1 DESCRIPTION

This file implements the methods and functions of C<Any> that
are most closely associated with the C<Num> class or role.
We place them here instead of F<src/classes/Any.pir> to keep
the size of that file down and to emphasize their generic,
"built-in" nature.

=head2 Methods

=over 4

=cut

.namespace []
.loadlib 'math_ops'
.sub 'onload' :anon :init :load
    $P0 = get_hll_namespace ['Any']
    '!EXPORT'('abs,int,polar,sqrt,truncate', 'from'=>$P0)

    ##  pre-seed a random number generator
    'srand'()
.end


=item abs()

=cut

.namespace ['Any']
.sub 'abs' :method :multi(_)
    $N0 = self
    $N1 = abs $N0
    .return ($N1)
.end

.namespace ['Any']
.sub 'int' :method :multi(_)
    "die"("the int() sub and .int method have been replaced by the .Int method")
.end

.namespace ['Any']
.sub 'Int' :method :multi(_)
    .tailcall self.'truncate'()
.end


=item polar

=cut

.namespace ['Any']
.sub 'polar' :method :multi(_)
    $N0 = self
    .tailcall 'list'($N0, 0)
.end


=item sqrt()

=cut

.namespace ['Any']
.sub 'sqrt' :method :multi(_)
    $N0 = self
    $N1 = sqrt $N0
    .return ($N1)
.end


=item srand()

=cut

.namespace []
.sub 'srand'
    .param num seed            :optional
    .param int has_seed        :opt_flag
    if has_seed goto have_seed
    seed = time
  have_seed:
    srand seed
    .return ()
.end

.namespace ['Any']
.sub 'srand' :method
    $N0 = self
    srand $N0
    .return ()
.end


=item truncate()

=item int

=cut

.namespace ['Any']
.sub 'truncate' :method :multi(_)
    $N0 = self
    if $N0 == 0 goto done
    if $N0 < 0 goto num_ceil
    floor $N0
    goto done
  num_ceil:
    ceil $N0
  done:
    $I0 = $N0
    .return ($I0)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
