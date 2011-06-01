# What follows exists for the benefit of calling this op from C. Turns out
# Parrot_ext_call won't handle multis, and also there's That Tailcall Bug.
.namespace []
.sub '!only_infix:='
    .param pmc cont
    .param pmc source
    $P0 = '&infix:<=>'(cont, source)
    .return ($P0)
.end
