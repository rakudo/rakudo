## $Id$

=head1 NAME

src/builtins/inplace.pir - Inplace assignments

=head1 Functions

=over 4

=cut

.namespace []

## assignment
## TODO: infix::= infix:::= infix:.=
##   -- these will likely be handled by compiler translation --Pm


.sub 'infix:='
    .param pmc cont
    .param pmc source

    $I0 = isa cont, 'ObjectRef'
    if $I0 goto object_ref
    $I0 = isa cont, 'Perl6Array'
    if $I0 goto array
    $I0 = isa cont, 'Perl6Hash'
    if $I0 goto hash

  object_ref:
    $I0 = can source, 'Scalar'
    unless $I0 goto have_source
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

  array:
    $P0 = get_hll_global 'list'
    $P0 = $P0(source)
    $I0 = elements cont
    splice cont, $P0, 0, $I0
    .return (cont)

  hash:
    $P0 = source.'hash'()
    copy cont, $P0
    .return (cont)
.end


.sub 'infix:~='
    .param pmc a
    .param pmc b
    concat a, b
    .return (a)
.end


.sub 'infix:+='
    .param pmc a
    .param pmc b
    a += b
    .return (a)
.end


.sub 'infix:-='
    .param pmc a
    .param pmc b
    a -= b
    .return (a)
.end


.sub 'infix:*='
    .param pmc a
    .param pmc b
    a *= b
    .return (a)
.end


.sub 'infix:/='
    .param pmc a
    .param pmc b
    a /= b
    .return (a)
.end


.sub 'infix:%='
    .param pmc a
    .param pmc b
    a %= b
    .return (a)
.end


.sub 'infix:x='
    .param pmc a
    .param pmc b
    repeat a, b
    .return (a)
.end


## TODO: infix:Y=
.sub 'infix:**='
    .param pmc a
    .param pmc b
    pow $P0, a, b
    'infix:='(a, $P0)
    .return (a)
.end


## TODO: infix:xx= infix:||= infix:&&= infix://= infix:^^=


.sub 'infix:+<='
    .param pmc a
    .param pmc b
    a <<= b
    .return (a)
.end


.sub 'infix:+>='
    .param pmc a
    .param pmc b
    a >>= b
    .return (a)
.end


.sub 'infix:+&='
    .param pmc a
    .param pmc b
    band a, b
    .return (a)
.end


.sub 'infix:+|='
    .param pmc a
    .param pmc b
    bor a, b
    .return (a)
.end


.sub 'infix:+^='
    .param pmc a
    .param pmc b
    bxor a, b
    .return (a)
.end


.sub 'infix:~&='
    .param pmc a
    .param pmc b
    a = bands a, b
    .return (a)
.end


.sub 'infix:~|='
    .param pmc a
    .param pmc b
    bors a, b
    .return (a)
.end


.sub 'infix:~^='
    .param pmc a
    .param pmc b
    bxors a, b
    .return (a)
.end


.sub 'infix:?&='
    .param pmc a
    .param pmc b
    band a, b
    $I0 = istrue a
    a = $I0
    .return (a)
.end


.sub 'infix:?|='
    .param pmc a
    .param pmc b
    bor a, b
    $I0 = istrue a
    a = $I0
    .return (a)
.end


.sub 'infix:?^='
    .param pmc a
    .param pmc b
    bxor a, b
    $I0 = istrue a
    a = $I0
    .return (a)
.end


=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
