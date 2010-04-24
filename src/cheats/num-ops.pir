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
