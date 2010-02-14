
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

