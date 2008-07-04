## $Id$

=head1 NAME

src/builtins/any_num.pir -  C<Num>-like functions and methods for C<Any>

=head1 DESCRIPTION

This file implements the methods and functions of C<Any> that
are most closely associated with the C<Num> class or role.
We place them here instead of L<src/classes/Any.pir> to keep
the size of that file down and to emphasize their generic,
"built-in" nature.

=head2 Methods

=cut

.namespace []
.sub 'onload' :anon :init :load
    $P0 = get_hll_namespace ['Any']
    '!EXPORT'('abs cis sqrt truncate int unpolar', 'from'=>$P0)

    ##  pre-seed a random number generator
    $P0 = new 'Random'
    set_hll_global ['Any'], '$!random', $P0
    srand()
.end


=item abs()

=cut

.namespace ['Any']
.sub 'abs' :method :multi(_)
    $N0 = self
    $N1 = abs $N0
    .return ($N1)
.end


=item cis($angle)

=cut

.namespace ['Any']
.sub 'cis' :method
    .return 'unpolar'(1.0, self)
.end


=item rand()

=cut

.namespace []
.sub 'rand'
    .param pmc x               :slurpy
    ## 0-argument test, RT#56366
    unless x goto no_args
    die "too many arguments passed - 0 params expected"
  no_args:
    $P0 = get_hll_global ['Any'], '$!random'
    $N0 = $P0
    .return ($N0)
.end

.namespace ['Any']
.sub 'rand' :method
    $N0 = self
    $P0 = get_hll_global ['Any'], '$!random'
    $N1 = $P0
    $N0 *= $N1
    .return ($N0)
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
    $P0 = get_hll_global ['Any'], '$!random'
    $I0 = seed
    $P0 = $I0
    .return ()
.end

.namespace ['Any']
.sub 'srand' :method
    $N0 = self
    $I0 = $N0
    $P0 = get_hll_global ['Any'], '$!random'
    $P0 = $I0
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

.sub 'int' :method :multi(_)
    .return self.'truncate'()
.end

=item unpolar($angle)

=cut

.sub 'unpolar' :method
    .param num angle
    .local num mag
    .local pmc result
    mag = self
    result = new 'Complex'
    $N0 = cos angle
    $N0 *= mag
    result[0] = $N0
    $N0 = sin angle
    $N0 *= mag
    result[1] = $N0
    .return (result)
.end


=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
