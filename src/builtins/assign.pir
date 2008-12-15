## $Id$

=head1 NAME

src/builtins/inplace.pir - Inplace assignments

=head1 Functions

=over 4

=cut


.namespace []
.sub 'infix:=' :multi(_,_)
    .param pmc cont
    .param pmc source

    $I0 = isa source, 'ObjectRef'
    if $I0 goto have_source
    $I0 = can source, 'Scalar'
    if $I0 goto can_scalar
    ##  source comes from outside Rakudo's type system
    $I0 = does source, 'scalar'
    if $I0 goto have_source
    source = new 'ObjectRef', source
    goto have_source
  can_scalar:
    source = source.'Scalar'()
  have_source:
    .local pmc ro, type
    getprop ro, 'readonly', cont
    if null ro goto ro_ok
    unless ro goto ro_ok
    'die'('Cannot assign to readonly variable.')
  ro_ok:
    $I0 = defined source
    unless $I0 goto do_assign
    getprop type, 'type', cont
    if null type goto do_assign
    $I0 = type.'ACCEPTS'(source)
    if $I0 goto do_assign
    'die'("Type mismatch in assignment.")
  do_assign:
    eq_addr cont, source, skip_copy
    copy cont, source
  skip_copy:
    .return (cont)
.end


.sub 'infix:=' :multi(['Perl6Array'], _)
    .param pmc cont
    .param pmc source
    $I0 = isa cont, 'ObjectRef'
    unless $I0 goto cont_array
    # FIXME: use a :subid to directly lookup and call infix:=(_,_) above
    $P0 = get_hll_global 'Object'
    setref cont, $P0
    .tailcall 'infix:='(cont, source)

  cont_array:
    .tailcall cont.'!STORE'(source)
.end


.sub 'infix:=' :multi(['Perl6Hash'], _)
    .param pmc cont
    .param pmc source
    $I0 = isa cont, 'ObjectRef'
    unless $I0 goto cont_hash
    # FIXME: use a :subid to directly lookup and call infix:=(_,_) above
    $P0 = get_hll_global 'Object'
    setref cont, $P0
    .tailcall 'infix:='(cont, source)

  cont_hash:
    $P0 = source.'hash'()
    copy cont, $P0
    .return (cont)
.end


.sub 'infix:=' :multi(['List'], _)
    .param pmc list
    .param pmc source

    ##  get the list of containers and sources
    source = source.'list'()
    source.'!flatten'()

    ##  first, temporarily mark each container with a property
    ##  so we can clone it in source if needed
    .local pmc it, true
    it = iter list
    true = box 1
  mark_loop:
    unless it goto mark_done
    $P0 = shift it
    setprop $P0, 'target', true
    goto mark_loop
  mark_done:

    ## now build our 'real' source list, cloning any targets we encounter
    .local pmc slist
    slist = new 'List'
    it = iter source
  source_loop:
    unless it goto source_done
    $P0 = shift it
    $P1 = getprop 'target', $P0
    if null $P1 goto source_next
    $P0 = clone $P0
  source_next:
    push slist, $P0
    goto source_loop
  source_done:

    ## now perform the assignments, clearing targets as we go
    .local pmc pmcnull
    null pmcnull
    it = iter list
  assign_loop:
    unless it goto assign_done
    .local pmc cont
    cont = shift it
    setprop cont, 'target', pmcnull
    $I0 = isa cont, 'ObjectRef'
    if $I0 goto assign_scalar
    $I0 = isa cont, 'Perl6Array'
    if $I0 goto assign_array
    $I0 = isa cont, 'Perl6Hash'
    if $I0 goto assign_hash
  assign_scalar:
    $P0 = shift slist
    'infix:='(cont, $P0)
    goto assign_loop
  assign_array:
  assign_hash:
    'infix:='(cont, slist)
    slist = new 'Nil'
    goto assign_loop
  assign_done:
    .return (list)
.end


.sub '!REDUCEMETAOP'
    .param string opname
    .param pmc identity
    .param pmc args                # already :slurpy array by caller

    args.'!flatten'()
    if args goto reduce
    if identity == 'fail' goto fail
    .return (identity)

  fail:
    .tailcall '!FAIL'()

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

    # Make sure they're both lists. XXX Need to handle hashes in future.
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
    result = new 'Perl6Array'
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
    $P0 = new 'ObjectRef', $P0
    push result, $P0
    goto loop

  loop_end:
    .return (result)

  incompatible:
    'die'("Non-dwimmy hyperoperator cannot be used on arrays of different sizes or dimensions.")
.end


.sub '!CROSSMETAOP'
    .param string opname
    .param string identity
    .param int chain
    .param pmc a
    .param pmc b

    # Use the X operator to get all permutation lists.
    .local pmc lists
    lists = 'infix:X'(a, b)

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
