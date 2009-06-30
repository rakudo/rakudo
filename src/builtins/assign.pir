## $Id$

=head1 NAME

src/builtins/assign.pir - assignments

=head1 Functions

=over 4

=cut

.namespace []
.sub 'infix:=' :multi(_,_)
    .param pmc cont
    .param pmc source

    .local pmc ro, type
    getprop ro, 'readonly', cont
    if null ro goto ro_ok
    unless ro goto ro_ok
    'die'('Cannot assign to readonly variable.')
  ro_ok:

    $I0 = isa cont, 'Perl6Scalar'
    if $I0 goto obj_store
    $I0 = can cont, '!STORE'
    unless $I0 goto obj_store
    .tailcall cont.'!STORE'(source)

  obj_store:
    .const 'Sub' STORE = 'Object::!STORE'
    .tailcall STORE(cont, source)
.end


.sub '!REDUCEMETAOP'
    .param string opname
    .param pmc identity
    .param pmc args                # already :slurpy array by caller

    args.'!flatten'()
    if args goto reduce
    if identity == 'fail' goto fail
    if identity == 'list' goto list
    .return (identity)

  fail:
    .tailcall '!FAIL'()
  list:
    .tailcall 'list'()

  reduce:
    opname = concat 'infix:', opname
    .local pmc opfunc
    opfunc = find_name opname
    .local pmc result
    result = shift args
  reduce_loop:
    unless args goto reduce_done
    $P0 = shift args
    result = opfunc(result, $P0)
    goto reduce_loop
  reduce_done:
    .return (result)
.end


.sub '!REDUCEMETAOPCHAIN'
    .param string opname
    .param string identity
    .param pmc args                # already :slurpy array by caller

    .local int want_true
    want_true = identity == 'True'

    args.'!flatten'()
    $I0 = elements args
    if $I0 > 1 goto reduce
    if want_true goto true
  false:
    $P0 = get_hll_global [ 'Bool' ], 'False'
    .return ($P0)
  true:
    $P0 = get_hll_global [ 'Bool' ], 'True'
    .return ($P0)

  reduce:
    opname = concat 'infix:', opname
    .local pmc opfunc
    opfunc = find_name opname
    .local pmc a, b
    b = shift args
  reduce_loop:
    unless args goto reduce_done
    a = b
    b = shift args
    $I0 = opfunc(a, b)
    unless $I0 goto false
    goto reduce_loop
  reduce_done:
    goto true
.end


.sub '!ASSIGNMETAOP'
    .param string opname
    .param pmc a
    .param pmc b

    $I0 = defined a
    if $I0 goto have_a
    $S0 = concat 'prefix:[', opname
    concat $S0, ']'
    $P1 = find_name $S0
    $P0 = $P1()
    'infix:='(a, $P0)
  have_a:

    opname = concat 'infix:', opname
    $P1 = find_name opname
    $P0 = $P1(a, b)
    'infix:='(a, $P0)
    .return (a)
.end


.sub '!HYPEROP'
    .param string opname
    .param pmc a
    .param pmc b
    .param int dwim_lhs
    .param int dwim_rhs

    # If we have a hash, go to the hyper op for hashes implementation.
    $P0 = get_hll_global 'Associative'
    $I0 = $P0.'ACCEPTS'(a)
    unless $I0 goto not_hash
    $I0 = $P0.'ACCEPTS'(b)
    unless $I0 goto not_hash
    .tailcall '!HYPEROPHASH'(opname, a, b, dwim_lhs, dwim_rhs)
  not_hash:

    # Make sure they're both lists.
    a = a.'list'()
    b = b.'list'()

    # Ensure lengths are the same.
    .local int elems_a, elems_b
    elems_a = a.'elems'()
    elems_b = b.'elems'()
    if elems_a < elems_b goto extend_lhs
    if elems_b < elems_a goto extend_rhs
    goto go_hyper

    # Extend LHS if needed.
    .local pmc extend_with
  extend_lhs:
    unless dwim_lhs goto incompatible
    if elems_a > 0 goto have_elems_a
    extend_with = '!FAIL'()
    a = 'infix:xx'(extend_with, elems_b)
    goto go_hyper
  have_elems_a:
    extend_with = a[-1]
    $I0 = elems_b - elems_a
    extend_with = 'infix:xx'(extend_with, $I0)
    a = 'list'(a, extend_with)
    goto go_hyper

    # Extend RHS if needed.
  extend_rhs:
    unless dwim_rhs goto incompatible
    if elems_b > 0 goto have_elems_b
    extend_with = '!FAIL'()
    b = 'infix:xx'(extend_with, elems_a)
    goto go_hyper
  have_elems_b:
    extend_with = b[-1]
    $I0 = elems_a - elems_b
    extend_with = 'infix:xx'(extend_with, $I0)
    b = 'list'(b, extend_with)
    goto go_hyper

    # Create result list and get iterators over the two.
  go_hyper:
    .local pmc result, it_a, it_b
    result = new ['Perl6Array']
    it_a = iter a
    it_b = iter b

    # Go over them and do the op, recursing if we see a nested array.
    .local pmc opfunc, cur_a, cur_b
    .local int array_a, array_b
    $S0 = concat 'infix:', opname
    opfunc = find_name $S0
  loop:
    unless it_a goto loop_end
    cur_a = shift it_a
    cur_b = shift it_b
    array_a = isa cur_a, 'Perl6Array'
    array_b = isa cur_b, 'Perl6Array'
    if array_a goto nested_array_lhs
    if array_b goto nested_array_rhs
    $P0 = opfunc(cur_a, cur_b)
    push result, $P0
    goto loop

    # Handle nested arrays.
  nested_array_lhs:
    if array_b goto recurse
    unless dwim_rhs goto incompatible
    cur_b = 'list'(cur_b)
    goto recurse
  nested_array_rhs:
    if array_a goto recurse
    unless dwim_lhs goto incompatible
    cur_a = 'list'(cur_a)
  recurse:
    $P0 = '!HYPEROP'(opname, cur_a, cur_b, dwim_lhs, dwim_rhs)
    $P0 = root_new ['parrot';'Perl6Scalar'], $P0
    push result, $P0
    goto loop

  loop_end:
    .return (result)

  incompatible:
    'die'("Non-dwimmy hyperoperator cannot be used on arrays of different sizes or dimensions.")
.end


.sub '!HYPEROPHASH'
    .param string opname
    .param pmc a
    .param pmc b
    .param int dwim_lhs
    .param int dwim_rhs

    # First, work out applicable keys.
    .local pmc keys_applicable, it
    keys_applicable = root_new ['parrot';'ResizablePMCArray']
    $I0 = dwim_lhs * dwim_rhs
    if $I0 goto intersection
    $I0 = dwim_lhs + dwim_rhs
    unless $I0 goto union
    if dwim_rhs goto keys_a
    keys_applicable = b.'keys'()
    goto have_applicable_keys
  keys_a:
    keys_applicable = a.'keys'()
    goto have_applicable_keys

  intersection:
    it = iter a
  intersection_it_loop:
    unless it goto intersection_it_loop_end
    $P0 = shift it
    $I0 = b.'exists'($P0)
    unless $I0 goto intersection_it_loop
    push keys_applicable, $P0
    goto intersection_it_loop
  intersection_it_loop_end:
    goto have_applicable_keys

  union:
    it = iter a
  union_it_loop_a:
    unless it goto union_it_loop_a_end
    $P0 = shift it
    push keys_applicable, $P0
    goto union_it_loop_a
  union_it_loop_a_end:
    it = iter b
  union_it_loop_b:
    unless it goto union_it_loop_b_end
    $P0 = shift it
    $I0 = a.'exists'($P0)
    if $I0 goto union_it_loop_b
    push keys_applicable, $P0
    goto union_it_loop_b
  union_it_loop_b_end:
    goto have_applicable_keys

  have_applicable_keys:
    .local pmc opfunc, result
    $S0 = concat 'infix:', opname
    opfunc = find_name $S0
    result = new ['Perl6Hash']
    it = iter keys_applicable
  it_loop:
    unless it goto it_loop_end
    $P0 = shift it
    # XXX Would be nice to do:
    # $P1 = a.'postcircumfix:{ }'($P0)
    # $P2 = b.'postcircumfix:{ }'($P0)
    # But we can't until the auto-vivification-on-read bug is fixed.
    $P1 = a[$P0]
    unless null $P1 goto got_first
    $P1 = 'undef'()
  got_first:
    $P2 = b[$P0]
    unless null $P2 goto got_second
    $P2 = 'undef'()
  got_second:
    $P3 = opfunc($P1, $P2)
    result[$P0] = $P3
    goto it_loop
  it_loop_end:

    .return (result)
.end


.sub '!CROSSMETAOP'
    .param string opname
    .param string identity
    .param int chain
    .param pmc args :slurpy

    # Use the X operator to get all permutation lists.
    .local pmc lists
    lists = 'infix:X'(args :flat)

    # Go over the lists and combine them with reduce meta-op.
    .local pmc result, it, combinder
    if chain goto chain_reduce
    combinder = find_name '!REDUCEMETAOP'
    goto combinder_done
  chain_reduce:
    combinder = find_name '!REDUCEMETAOPCHAIN'
  combinder_done:
    result = 'list'()
    it = iter lists
  it_loop:
    unless it goto it_loop_end
    $P0 = shift it
    $P0 = combinder(opname, identity, $P0)
    push result, $P0
    goto it_loop
  it_loop_end:

    .return (result)
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
