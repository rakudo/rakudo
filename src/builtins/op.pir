## $Id$

=head1 NAME

src/builtins/op.pir - Perl6 builtin operators

=head1 Functions

=over 4

=cut

.namespace

## autoincrement
.sub 'postfix:++'
    .param pmc a
    $P0 = clone a
    inc a
    .return ($P0)
.end

.sub 'postfix:--'
    .param pmc a
    $P0 = clone a
    dec a
    .return ($P0)
.end


.sub 'prefix:++'
    .param pmc a
    inc a
    .return (a)
.end


.sub 'prefix:--'
    .param pmc a
    dec a
    .return (a)
.end


## exponentiation
.sub 'infix:**'
    .param num base
    .param num exp
    $N0 = pow base, exp
    .return ($N0)
.end


## symbolic unary
.sub 'prefix:!'
    .param pmc a
    $I0 = isfalse a
    .return ($I0)
.end


.sub 'prefix:+'
    .param pmc a
    $I0 = does a, 'float'
    if $I0 == 0 goto return_int
    $N0 = a
    .return ($N0)
  return_int:
    $I0 = a
    .return ($I0)
.end


.sub 'prefix:-'
    .param pmc a
    $N0 = a
    $N0 = neg $N0
    .return ($N0)
.end


.sub 'prefix:~'
    .param pmc a
    $S0 = a
    .return ($S0)
.end


.sub 'prefix:?'
    .param pmc a

    .local pmc bool
    bool = new .Perl6Bool
    $I0 = istrue a
    assign bool, $I0
    .return (bool)
.end


## TODO: prefix:= prefix:* prefix:** prefix:~^ prefix:+^


.sub 'prefix:?^'
    .param pmc a
    $I0 = isfalse a
    .return ($I0)
.end


## TODO: prefix:^


## multiplicative
.sub 'infix:*'
    .param pmc a
    .param pmc b
    $P0 = mul a, b
    .return ($P0)
.end


.sub 'infix:/'
    .param pmc a
    .param pmc b
    $P0 = div a, b
    .return ($P0)
.end


.sub 'infix:%'
    .param num a
    .param num b
    $N0 = mod a, b
    .return ($N0)
.end


.sub 'infix:x'
    .param string a
    .param int b
    $S0 = repeat a, b
    .return ($S0)
.end


.sub 'infix:xx'
    .param string a
    .param int b
    $P0 = new ResizablePMCArray
  lp:
    unless b, ex
    push $P0, a
    dec b
    branch lp
  ex:
    .return ($P0)
.end


.sub 'infix:+&'
    .param int a
    .param int b
    $I0 = band a, b
    .return ($I0)
.end


.sub 'infix:+<'
    .param int a
    .param int b
    $I0 = shl a, b
    .return ($I0)
.end


.sub 'infix:+>'
    .param int a
    .param int b
    $I0 = shr a, b
    .return ($I0)
.end


.sub 'infix:~&'
    .param string a
    .param string b
    $S0 = bands a, b
    .return ($S0)
.end


## TODO: infix:~< infix:~>


## additive
.sub 'infix:+'
    .param pmc a
    .param pmc b
    $P0 = add a, b
    .return ($P0)
.end


.sub 'infix:-'
    .param pmc a
    .param pmc b
    $P0 = sub a, b
    .return ($P0)
.end


.sub 'infix:~'
    .param string a
    .param string b
    $S0 = concat a, b
    .return ($S0)
.end


.sub 'infix:+|'
    .param int a
    .param int b
    $I0 = bor a, b
    .return ($I0)
.end


.sub 'infix:+^'
    .param int a
    .param int b
    $I0 = bxor a, b
    .return ($I0)
.end


.sub 'infix:~|'
    .param string a
    .param string b
    $S0 = bors a, b
    .return ($S0)
.end


.sub 'infix:~^'
    .param string a
    .param string b
    $S0 = bxors a, b
    .return ($S0)
.end


.sub 'infix:?&'
    .param int a
    .param int b
    $I0 = band a, b
    $I0 = isne $I0, 0
    .return ($I0)
.end


.sub 'infix:?|'
    .param int a
    .param int b
    $I0 = bor a, b
    $I0 = isne $I0, 0
    .return ($I0)
.end


.sub 'infix:?^'
    .param int a
    .param int b
    $I0 = bxor a, b
    $I0 = isne $I0, 0
    .return ($I0)
.end


=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
