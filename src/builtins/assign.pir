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
    '&die'('Cannot assign to readonly value')
  rw_ok:

    # If the lhs isn't a scalar container, delegate to
    # object's STORE method.
    $P0 = getprop 'scalar', cont
    if null $P0 goto cont_store

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
    unless $I0 goto scalar_assign
    cont = tgt
    goto cont_loop

  scalar_assign:
    # fully dereference the source, put it in item context, and set the 
    # lhs objectref to it
    source = descalarref source
    $I0 = can source, 'item'
    unless $I0 goto have_source
    source = source.'item'()
  have_source:
    setref cont, source
    .return (cont)

  cont_store:
    .tailcall cont.'!STORE'(source)
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
    $P1 = $P0(a, b)
    .tailcall '&infix:<=>'(a, $P1)
.end

.sub '!gen_not_metaop'
    .param string sym
    .local string opname, metaname
    $S0 = concat sym, '>'
    opname = concat '&infix:<', $S0
    metaname = concat '&infix:<!', $S0
    $P0 = get_global metaname
    unless null $P0 goto done
    $P1 = box opname
    .lex '$opname', $P1
    .const 'Sub' metasub = '!not_metaop'
    $P0 = newclosure metasub
    set_global metaname, $P0
  done:
.end

# XXX -- we might want this to be a Perl6MultiSub
.sub '!not_metaop' :anon :outer('!gen_not_metaop')
    .param pmc a
    .param pmc b
    $P0 = find_lex '$opname'
    $S0 = $P0
    $P0 = get_global $S0
    $P1 = $P0(a, b)
    .tailcall '&prefix:<!>'($P1)
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
