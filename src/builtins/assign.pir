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
    .const 'Sub' $P0 = 'Object::!STORE'
    .tailcall cont.$P0(source)
.end

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
