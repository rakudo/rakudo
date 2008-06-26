## $Id$

=head1 TITLE

Any - Perl 6 Any class

=head1 DESCRIPTION

This file implements the Any class.

=over 4

=cut

.namespace [ 'Any' ]

.sub 'onload' :anon :init :load
    .local pmc p6meta
    p6meta = get_hll_global ['Perl6Object'], '$!P6META'
    p6meta.'new_class'('Any', 'parent'=>'Perl6Object')

    ##  pre-seed a random number generator
    $P0 = new 'Random'
    set_hll_global ['Any'], '$!random', $P0
    'srand'()
.end

.sub 'isa' :method
    .param pmc x
    $P0 = self.'HOW'()
    .return $P0.'isa'(x)
.end

.sub 'can' :method
    .param pmc x
    $P0 = self.'HOW'()
    .return $P0.'can'(x)
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

.namespace ['Num']
.sub 'srand' :method
    $I0 = self
    $P0 = get_hll_global ['Any'], '$!random'
    $P0 = $I0
    .return ()
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
