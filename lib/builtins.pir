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
    .return ()
.end


.sub 'say'
    .param pmc list            :slurpy
    'print'(list :flat)
    print "\n"
    .return ()
.end


.sub 'list'
    .param pmc list            :slurpy
    .return (list)
.end


.sub 'infix:**'
    .param num base
    .param num exp
    $N0 = pow base, exp
    .return ($N0)
.end
