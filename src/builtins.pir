## $Id$

.namespace [ "" ]

.sub 'print'
    .param pmc list            :slurpy
    .local pmc iter

    iter = new .Iterator, list
    iter = 0
  iter_loop:
    unless iter goto iter_end
    $P0 = shift iter
    print $P0
    goto iter_loop
  iter_end:
    .return (1)
.end


.sub 'say'
    .param pmc list            :slurpy
    'print'(list :flat)
    print "\n"
    .return (1)
.end


.sub 'list'
    .param pmc list            :slurpy
    .return (list)
.end


.sub 'use'
    .param pmc list            :slurpy
    .return ()
.end


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
    $N0 = a
    .return ($N0)
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
    $I0 = istrue a
    .return ($I0)
.end


## TODO: prefix:= prefix:* prefix:** prefix:~^


## TODO: unfiled
.sub 'prefix:?^'
    .param pmc a
    $I0 = isfalse a
    .return ($I0)
.end


.sub 'infix:<'
    .param pmc a
    .param pmc b
    $I0 = cmp_num a, b
    $I0 = islt $I0, 0
    .return ($I0)
.end


.sub 'infix:>'
    .param pmc a
    .param pmc b
    $I0 = cmp_num a, b
    $I0 = isgt $I0, 0
    .return ($I0)
.end


.sub 'infix:<='
    .param pmc a
    .param pmc b
    $I0 = cmp_num a, b
    $I0 = isle $I0, 0
    .return ($I0)
.end


.sub 'infix:>='
    .param pmc a
    .param pmc b
    $I0 = cmp_num a, b
    $I0 = isge $I0, 0
    .return ($I0)
.end


.sub 'infix:=='
    .param pmc a
    .param pmc b
    $I0 = cmp_num a, b
    $I0 = iseq $I0, 0
    .return ($I0)
.end


.sub 'infix:!='
    .param pmc a
    .param pmc b
    $I0 = cmp_num a, b
    $I0 = isne $I0, 0
    .return ($I0)
.end


.sub 'infix:lt'
    .param pmc a
    .param pmc b
    $I0 = cmp_str a, b
    $I0 = islt $I0, 0
    .return ($I0)
.end


.sub 'infix:gt'
    .param pmc a
    .param pmc b
    $I0 = cmp_str a, b
    $I0 = isgt $I0, 0
    .return ($I0)
.end


.sub 'infix:le'
    .param pmc a
    .param pmc b
    $I0 = cmp_str a, b
    $I0 = isle $I0, 0
    .return ($I0)
.end


.sub 'infix:ge'
    .param pmc a
    .param pmc b
    $I0 = cmp_str a, b
    $I0 = isge $I0, 0
    .return ($I0)
.end


.sub 'infix:eq'
    .param pmc a
    .param pmc b
    $I0 = cmp_str a, b
    $I0 = iseq $I0, 0
    .return ($I0)
.end


.sub 'infix:ne'
    .param pmc a
    .param pmc b
    $I0 = cmp_str a, b
    $I0 = isne $I0, 0
    .return ($I0)
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


.sub 'infix:~='
    .param pmc a
    .param pmc b
    concat a, b
    .return (a)
.end


.sub 'infix:+&='
    .param pmc a
    .param pmc b
    .local int i_a, i_b
    i_a = a
    i_b = b
    band i_a, i_b
    a = i_a
    .return (a)
.end


.sub 'infix:+|='
    .param pmc a
    .param pmc b
    .local int i_a, i_b
    i_a = a
    i_b = b
    bor i_a, i_b
    a = i_a
    .return (a)
.end


.sub 'infix:+^='
    .param pmc a
    .param pmc b
    .local int i_a, i_b
    i_a = a
    i_b = b
    bxor i_a, i_b
    a = i_a
    .return (a)
.end


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


.sub 'infix:**='
    .param pmc a
    .param pmc b
    a = a ** b
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
    repeat a, a, b
    .return (a)
.end


.sub 'infix:~&='
    .param pmc a
    .param pmc b
    bands a, b
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


.sub 'die'
    .param pmc list            :slurpy
    .local pmc iter
    .local string message

    message = ''
    iter = new .Iterator, list
    iter = 0
  iter_loop:
    unless iter goto iter_end
    $P0 = shift iter
    $S0 = $P0
    message .= $S0
    goto iter_loop
  iter_end:
    $P0 = new .Exception
    $P0['_message'] = message
    throw $P0
    .return ()
.end

## vim: expandtab sw=4
