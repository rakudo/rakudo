# workaround Parrot role quirk, which we won't need when we stop using
# the Parrot role PMC.

.namespace []

=item !set_resolves_list(parrot_class)

Gets all the methods that the class has and adds them to the resolves list.

=cut

.sub '!set_resolves_list'
    .param pmc class
    .local pmc meths, it, res_list
    meths = class.'methods'()
    it = iter meths
    res_list = root_new ['parrot';'ResizableStringArray']
  it_loop:
    unless it goto it_loop_end
    $S0 = shift it
    $P0 = meths[$S0]
    $I0 = isa $P0, 'MultiSub'
    if $I0 goto it_loop
    push res_list, $S0
    goto it_loop
  it_loop_end:
    class.'resolve_method'(res_list)
.end