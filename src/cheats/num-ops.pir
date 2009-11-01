

.sub '&infix:<+>' :multi(_,_)
    .param num a
    .param num b
    $N0 = add a, b
    .return ($N0)
.end

.sub '&infix:<->' :multi(_,_)
    .param num a
    .param num b
    $N0 = sub a, b
    .return ($N0)
.end

.sub '&infix:<*>' :multi(_,_)
    .param num a
    .param num b
    $N0 = mul a, b
    .return ($N0)
.end

.sub '&infix:</>' :multi(_,_)
    .param num a
    .param num b
    $N0 = div a, b
    .return ($N0)
.end

.sub '&infix:<%>' :multi(_,_)
    .param int a
    .param int b
    $I0 = mod a, b
    .return ($I0)
.end

.sub '&infix:<**>' :multi(_,_)
    .param num a
    .param num b
    $N0 = pow a, b
    .return ($N0)
.end

.sub '&infix:<+|>' :multi(_,_)
    .param int a
    .param int b
    $I0 = bor a, b
    .return ($I0)
.end

.sub '&infix:<+&>' :multi(_,_)
    .param int a
    .param int b
    $I0 = band a, b
    .return ($I0)
.end

.sub '&infix:<+^>' :multi(_,_)
    .param int a
    .param int b
    $I0 = bxor a, b
    .return ($I0)
.end

.sub '&infix:<+<>' :multi(_,_)
    .param int a
    .param int b
    $I0 = shl a, b
    .return ($I0)
.end

.sub '&infix:<+>>' :multi(_,_)
    .param int a
    .param int b
    $I0 = shr a, b
    .return ($I0)
.end

