=head1 NAME

src/builtins/assign.pir - assignment operations

=head1 Functions

=over 4

=cut

.namespace []
.sub '&infix:<=>' :multi(_,_)
    .param pmc cont
    .param pmc source

    .local pmc rw
    getprop rw, 'rw', cont
    unless null rw goto cont_store
    die 'Cannot assign to readonly value'
  cont_store:
    $I0 = can cont, '!STORE'
    unless $I0 goto obj_store
    .tailcall cont.'!STORE'(source)
  obj_store:
    .const 'Sub' $P0 = 'Mu::!STORE'
    .tailcall cont.$P0(source)
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

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
