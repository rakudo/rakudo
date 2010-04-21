
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

.sub '&prefix:<+>'
    .param pmc x
    $I0 = isa x, ['Mu']
    if $I0 goto p6_x
    $N0 = x
    .return ($N0)
  p6_x:
    $P0 = x.'Numeric'()
    .return ($P0)
.end
