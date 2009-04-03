# This is a macro that helps in creating parametric roles. It exists to
# keep actions.pm a little shorter, but also to enable us to more easily
# construct parametric roles in PIR.
.macro create_parametric_role(mr)
    "!meta_compose"(.mr)
    .local pmc orig_role, meths, meth_iter
    orig_role = getprop "$!orig_role", .mr
    meths = orig_role."methods"()
    meth_iter = iter meths
  it_loop:
    unless meth_iter goto it_loop_end
    $S0 = shift meth_iter
    $P0 = meths[$S0]
    $P1 = getprop "$!signature", $P0
    $P0 = newclosure $P0
    setprop $P0, "$!signature", $P1
    .mr."add_method"($S0, $P0)
    goto it_loop
  it_loop_end:
    .return (.mr)
 .endm
