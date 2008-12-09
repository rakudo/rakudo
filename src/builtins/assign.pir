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
    .local pmc list, it, array
    ## empty the array
    array = new 'ResizablePMCArray'
    source = 'list'(source)
    it = iter source
  array_loop:
    unless it goto array_done
    $P0 = shift it
    $P0 = $P0.'Scalar'()
    $P0 = clone $P0
    push array, $P0
    goto array_loop
  array_done:
    $I0 = elements cont
    splice cont, array, 0, $I0
    .return (cont)
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


.sub '!HYPEROPNODWIM'
    .param string opname
    .param pmc a
    .param pmc b

    # Make sure they're both lists. XXX Need to handle hashes in future.
    a = a.'list'()
    b = b.'list'()

    # Ensure lengths are the same.
    $I0 = a.'elems'()
    $I1 = b.'elems'()
    if $I0 != $I1 goto incompatible

    # Create result list and get iterators over the two.
    .local pmc result, it_a, it_b
    result = new 'Perl6Array'
    it_a = iter a
    it_b = iter b

    # Go over them and do the op, recursing if we see a nested array.
    .local pmc opfunc
    $S0 = concat 'infix:', opname
    opfunc = find_name $S0
  loop:
    unless it_a goto loop_end
    $P0 = shift it_a
    $P1 = shift it_b
    $I0 = isa $P0, 'Perl6Array'
    if $I0 goto nested_array
    $P2 = opfunc($P0, $P1)
    push result, $P2
    goto loop

    # If it's a nested array on LHS, must be on RHS too.
  nested_array:
    $I0 = isa $P1, 'Perl6Array'
    unless $I0 goto incompatible
    $P2 = '!HYPEROPNODWIM'(opname, $P0, $P1)
    $P2 = new 'ObjectRef', $P2
    push result, $P2
    goto loop
  
  loop_end:    
    .return (result)
    
  incompatible:
    'die'("Non-dwimmy hyperoperator cannot be used on arrays of different sizes or dimensions.")
.end

=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
