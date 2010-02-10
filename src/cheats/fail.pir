=item !FAIL(args)

Return a Failure object using C<args> as the failure message.

=cut

.namespace []
.sub '!FAIL'
    .param pmc args            :slurpy
    $P0 = new ['Exception']
    $S0 = join '', args
    $P1 = box $S0
    setattribute $P0, 'message', $P1
    $P1 = get_hll_global 'Exception'
    $P0 = $P1.'new'($P0)
    $P1 = get_hll_global 'Failure'
    $P1 = $P1.'new'($P0)
    .return ($P1)
.end
