=head1 NAME

src/builtins/assign.pir - assignment operations

=head1 Functions

=over 4

=cut

.namespace []
.sub '&infix:<=>' :multi(_,_)
    .param pmc cont
    .param pmc source

  cont_loop:
    # If the lhs isn't marked rw, throw exception
    .local pmc rw
    rw = getprop 'rw', cont
    unless null rw goto rw_ok
    '&die'('Cannot modify readonly value')
  rw_ok:

    # If the lhs isn't a scalar container, delegate to
    # object's STORE method.
    $P0 = getprop 'scalar', cont
    unless null $P0 goto scalar_store
    $I0 = can cont, '!STORE'
    if $I0 goto cont_store

    # We should never arrive here.  Anything that is marked 'rw'
    # should either be a Perl6Scalar (with the 'scalar' property
    # set) or a container that understands !STORE, such as Hash or Array.
    # However, there's some legacy code that fails to set 'scalar',
    # so we patch it in here to keep things going.
    $I0 = isa cont, ['ObjectRef']
    unless $I0 goto cont_store
    setprop cont, 'scalar', cont

  scalar_store:
    # perform any needed typecheck
    .local pmc type
    type = getprop 'type', cont
    if null type goto type_ok
    $P0 = type.'ACCEPTS'(source)
    if $P0 goto type_ok
    '&die'('Type check failed for assignment')
  type_ok:

    # Dereference the scalar LHS.  If the thing we're
    # currently referencing is itself an ObjectRef, delegate
    # the assignment to it.
    .local pmc tgt
    tgt = deref cont
    $I0 = isa tgt, ['ObjectRef']
    unless $I0 goto scalar_whence
    cont = tgt
    goto cont_loop

  scalar_whence:
    # Invoke any WHENCE property in the container
    .local pmc whence
    tgt = descalarref tgt
    whence = getprop 'WHENCE', tgt
    if null whence goto scalar_assign
    whence()

  scalar_assign:
    # check for Nil assignment
    $I0 = isa source, ['Parcel']
    unless $I0 goto item_assign
    $I0 = elements source
    if $I0 goto item_assign
  nil_assign:
    source = getprop 'type', cont
    unless null source goto have_source
    source = get_hll_global '$!OBJECTREF'
    goto have_source
  item_assign:
    # put the source in item context
    $I0 = can source, 'item'
    unless $I0 goto have_source
    source = source.'item'()
  have_source:
    source = descalarref source
    setref cont, source
    .return (cont)

  cont_store:
    .tailcall cont.'!STORE'(source)
.end


.sub '&infix:<=>' :multi(['Proxy'], _)
    .param pmc cont
    .param pmc source
    cont.'!VIVIFY'()
    $P0 = '&infix:<=>'(cont, source)
    .return ($P0)
.end


.sub '&infix:<=>' :multi(['Whatever'], _)
    .param pmc cont
    .param pmc source
    .return (cont)
.end


.sub '!gen_assign_metaop'
    .param string sym
    .local string opname, metaname
    $S0 = concat '&infix:<', sym
    opname = concat $S0, '>'
    metaname = concat $S0, '=>'
    $P0 = get_global metaname
    unless null $P0 goto done
    $P1 = box opname
    .lex '$opname', $P1
    .const 'Sub' metasub = '!assign_metaop'
    $P0 = newclosure metasub
    set_global metaname, $P0
  done:
.end

# XXX -- we might want this to be a Perl6MultiSub
.sub '!assign_metaop' :anon :outer('!gen_assign_metaop')
    .param pmc a
    .param pmc b
    $P0 = find_lex '$opname'
    $S0 = $P0
    $P0 = get_global $S0
    $P1 = a.'defined'()
    if $P1 goto defined
    $P2 = $P0()
    $P1 = $P0($P2, b)
    .tailcall '&infix:<=>'(a, $P1)
  defined:
    $P1 = $P0(a, b)
    .tailcall '&infix:<=>'(a, $P1)
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
