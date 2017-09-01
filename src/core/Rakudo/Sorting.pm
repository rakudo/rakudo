my class Rakudo::Sorting {

    # Return new IterationBuffer with the two given values
    sub IB2(Mu \one,Mu \two --> IterationBuffer) {
        nqp::stmts(
          (my $buf := nqp::create(IterationBuffer)),
          nqp::bindpos($buf,0,one),
          nqp::bindpos($buf,1,two),
          $buf
        )
    }

    # https://en.wikipedia.org/wiki/Merge_sort#Bottom-up_implementation
    # The parameter is the HLL List to be sorted *in place* using simple cmp.
    method MERGESORT-REIFIED-LIST(\list) {
        nqp::if(
          nqp::isgt_i((my int $n = nqp::elems(
            # $A has the items to sort; $B is a work array
            my $A := nqp::getattr(list,List,'$!reified')
          )),2),
          nqp::stmts(     # we actually need to sort
            (my $B := nqp::setelems(nqp::create(IterationBuffer),$n)),

            # Each 1-element run in $A is already "sorted"
            # Make successively longer sorted runs of length 2, 4, 8, 16...
            # until $A is wholly sorted
            (my int $width = 1),
            nqp::while(
              nqp::islt_i($width,$n),
              nqp::stmts(
                (my int $l = 0),

                # $A is full of runs of length $width
                nqp::while(
                  nqp::islt_i($l,$n),

                  nqp::stmts(
                    (my int $left  = $l),
                    (my int $right = nqp::add_i($l,$width)),
                    nqp::if(nqp::isge_i($right,$n),($right = $n)),
                    (my int $end = nqp::add_i($l,nqp::add_i($width,$width))),
                    nqp::if(nqp::isge_i($end,$n),($end = $n)),

                    (my int $i = $left),
                    (my int $j = $right),
                    (my int $k = nqp::sub_i($left,1)),

                    # Merge two runs: $A[i       .. i+width-1] and
                    #                 $A[i+width .. i+2*width-1]
                    # to $B or copy $A[i..n-1] to $B[] ( if(i+width >= n) )
                    nqp::while(
                      nqp::islt_i(($k = nqp::add_i($k,1)),$end),
                      nqp::if(
                        nqp::islt_i($i,$right) && (
                          nqp::isge_i($j,$end)
                            || nqp::iseq_i(
                                 nqp::decont(  # for some reason we need this
                                   nqp::atpos($A,$i) cmp nqp::atpos($A,$j)
                                     || nqp::cmp_i($i,$j)
                                 ), # apparently code gen with || isn't right
                                 -1
                               )
                        ),
                        nqp::stmts(
                          (nqp::bindpos($B,$k,nqp::atpos($A,$i))),
                          ($i = nqp::add_i($i,1))
                        ),
                        nqp::stmts(
                          (nqp::bindpos($B,$k,nqp::atpos($A,$j))),
                          ($j = nqp::add_i($j,1))
                        )
                      )
                    ),
                    ($l = nqp::add_i($l,nqp::add_i($width,$width)))
                  )
                ),

                # Now work array $B is full of runs of length 2*width.
                # Copy array B to array A for next iteration.  A more
                # efficient implementation would swap the roles of A and B.
                (my $temp := $B),($B := $A),($A := $temp),   # swap
                # Now array $A is full of runs of length 2*width.

                ($width = nqp::add_i($width,$width))
              )
            ),
            nqp::p6bindattrinvres(list,List,'$!reified',$A)
          ),
          nqp::if(
            nqp::islt_i($n,2)
              || nqp::isle_i(nqp::atpos($A,0) cmp nqp::atpos($A,1),0),
            list,  # nothing to be done, we already have the result
            nqp::p6bindattrinvres(list,List,'$!reified',  # need to swap
              IB2(nqp::atpos($A,1),nqp::atpos($A,0)))
          )
        )
    }

    # Takes the HLL List to be sorted *in place* using the comparator
    method MERGESORT-REIFIED-LIST-WITH(\list, &comparator) {
        nqp::if(
          nqp::isgt_i((my int $n = nqp::elems(
            # $A has the items to sort; $B is a work array
            my $A := nqp::getattr(list,List,'$!reified')
          )),2),
          nqp::stmts(     # we actually need to sort
            (my $B := nqp::setelems(nqp::create(IterationBuffer),$n)),

            # Each 1-element run in $A is already "sorted"
            # Make successively longer sorted runs of length 2, 4, 8, 16...
            # until $A is wholly sorted
            (my int $width = 1),
            nqp::while(
              nqp::islt_i($width,$n),
              nqp::stmts(
                (my int $l = 0),

                # $A is full of runs of length $width
                nqp::while(
                  nqp::islt_i($l,$n),

                  nqp::stmts(
                    (my int $left  = $l),
                    (my int $right = nqp::add_i($l,$width)),
                    nqp::if(nqp::isge_i($right,$n),($right = $n)),
                    (my int $end = nqp::add_i($l,nqp::add_i($width,$width))),
                    nqp::if(nqp::isge_i($end,$n),($end = $n)),

                    (my int $i = $left),
                    (my int $j = $right),
                    (my int $k = nqp::sub_i($left,1)),

                    # Merge two runs: $A[i       .. i+width-1] and
                    #                 $A[i+width .. i+2*width-1]
                    # to $B or copy $A[i..n-1] to $B[] ( if(i+width >= n) )
                    nqp::while(
                      nqp::islt_i(($k = nqp::add_i($k,1)),$end),
                      nqp::if(
                        nqp::islt_i($i,$right) && (
                          nqp::isge_i($j,$end)
                            || nqp::iseq_i(
                                 nqp::decont(  # for some reason we need this
                                   comparator(
                                     nqp::atpos($A,$i),nqp::atpos($A,$j))
                                      || nqp::cmp_i($i,$j)
                                 ), # apparently code gen with || isn't right
                                 -1
                               )
                        ),
                        nqp::stmts(
                          (nqp::bindpos($B,$k,nqp::atpos($A,$i))),
                          ($i = nqp::add_i($i,1))
                        ),
                        nqp::stmts(
                          (nqp::bindpos($B,$k,nqp::atpos($A,$j))),
                          ($j = nqp::add_i($j,1))
                        )
                      )
                    ),
                    ($l = nqp::add_i($l,nqp::add_i($width,$width)))
                  )
                ),

                # Now work array $B is full of runs of length 2*width.
                # Copy array B to array A for next iteration. A more
                # efficient implementation would swap the roles of A and B.
                (my $temp := $B),($B := $A),($A := $temp),   # swap
                # Now array $A is full of runs of length 2*width.

                ($width = nqp::add_i($width,$width))
              )
            ),
            nqp::p6bindattrinvres(list,List,'$!reified',$A)
          ),
          nqp::if(
            nqp::islt_i($n,2)
              || nqp::iseq_i(
                  comparator(nqp::atpos($A,0),nqp::atpos($A,1)),-1),
            list,  # nothing to be done, we already have the result
            nqp::p6bindattrinvres(list,List,'$!reified',  # need to swap
              IB2(nqp::atpos($A,1),nqp::atpos($A,0)))
          )
        )
    }
    # Takes the HLL List to be sorted *in place* using the mapper
    method MERGESORT-REIFIED-LIST-AS(\list,&mapper) {
        nqp::if(
          nqp::isgt_i((my int $n = nqp::elems(
            my $O := nqp::getattr(list,List,'$!reified')    # Original
          )),2),
          nqp::stmts(     # we actually need to sort
            (my $S :=                                       # the Schwartz
              nqp::setelems(nqp::create(IterationBuffer),$n)),
            (my $A := nqp::setelems(nqp::list_i,$n)),       # indexes to sort
            (my $B := nqp::setelems(nqp::list_i,$n)),       # work array
            (my int $s = -1),
            nqp::while(  # set up the Schwartz and the initial indexes
              nqp::islt_i(($s = nqp::add_i($s,1)),$n),
              nqp::bindpos($S,nqp::bindpos_i($A,$s,$s),
                mapper(nqp::atpos($O,$s)))
            ),

            # Each 1-element run in $A is already "sorted"
            # Make successively longer sorted runs of length 2, 4, 8, 16...
            # until $A is wholly sorted
            (my int $width = 1),
            nqp::while(
              nqp::islt_i($width,$n),
              nqp::stmts(
                (my int $l = 0),

                # $A is full of runs of length $width
                nqp::while(
                  nqp::islt_i($l,$n),

                  nqp::stmts(
                    (my int $left  = $l),
                    (my int $right = nqp::add_i($l,$width)),
                    nqp::if(nqp::isge_i($right,$n),($right = $n)),
                    (my int $end = nqp::add_i($l,nqp::add_i($width,$width))),
                    nqp::if(nqp::isge_i($end,$n),($end = $n)),

                    (my int $i = $left),
                    (my int $j = $right),
                    (my int $k = nqp::sub_i($left,1)),

                    # Merge two runs: $A[i       .. i+width-1] and
                    #                 $A[i+width .. i+2*width-1]
                    # to $B or copy $A[i..n-1] to $B[] ( if(i+width >= n) )
                    nqp::while(
                      nqp::islt_i(($k = nqp::add_i($k,1)),$end),
                      nqp::if(
                        nqp::islt_i($i,$right) && (
                          nqp::isge_i($j,$end)
                            || (nqp::iseq_i(
                                 nqp::decont(
                                   nqp::atpos($S,nqp::atpos_i($A,$i))
                                     cmp nqp::atpos($S,nqp::atpos_i($A,$j))
                                     || nqp::cmp_i($i,$j)
                                 ),
                                 -1
                               ))
                        ),
                        nqp::stmts(
                          (nqp::bindpos_i($B,$k,nqp::atpos_i($A,$i))),
                          ($i = nqp::add_i($i,1))
                        ),
                        nqp::stmts(
                          (nqp::bindpos_i($B,$k,nqp::atpos_i($A,$j))),
                          ($j = nqp::add_i($j,1))
                        )
                      )
                    ),
                    ($l = nqp::add_i($l,nqp::add_i($width,$width)))
                  )
                ),

                # Now work array $B is full of runs of length 2*width.
                # Copy array B to array A for next iteration. A more
                # efficient implementation would swap the roles of A and B.
                (my $temp := $B),($B := $A),($A := $temp),   # swap
                # Now array $A is full of runs of length 2*width.

                ($width = nqp::add_i($width,$width))
              )
            ),
            ($s = -1),
            nqp::while(   # repurpose the Schwartz for the result
              nqp::islt_i(($s = nqp::add_i($s,1)),$n),
              nqp::bindpos($S,$s,nqp::atpos($O,nqp::atpos_i($A,$s)))
            ),
            nqp::p6bindattrinvres(list,List,'$!reified',$S)
          ),

          nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',
            nqp::if(
              nqp::islt_i($n,2)
                || nqp::iseq_i(
                    mapper(nqp::atpos($O,0)) cmp mapper(nqp::atpos($O,1)),-1),
              $O,  # nothing to be done, we already have the result
              IB2(nqp::atpos($O,1),nqp::atpos($O,0))  # need to swap
            )
          )
        )
    }

    # https://en.wikipedia.org/wiki/Merge_sort#Bottom-up_implementation
    # Sort a native str array (or nqp::list_s) and return the result.
    # Uses the given str array as one of the buffers for performance reasons.
    # Please nqp::clone first if you want to keep the original intact.
    method MERGESORT-str(Mu \sortable) {
        nqp::if(
          nqp::isgt_i((my int $n = nqp::elems(sortable)),2),

          # $A has the items to sort; $B is a work array
          nqp::stmts(
            (my Mu $A := sortable),
            (my Mu $B := nqp::setelems(nqp::list_s,$n)),

            # Each 1-element run in $A is already "sorted"
            # Make successively longer sorted runs of length 2, 4, 8, 16...
            # until $A is wholly sorted
            (my int $width = 1),
            nqp::while(
              nqp::islt_i($width,$n),
              nqp::stmts(
                (my int $l = 0),

                # $A is full of runs of length $width
                nqp::while(
                  nqp::islt_i($l,$n),

                  nqp::stmts(
                    (my int $left  = $l),
                    (my int $right = nqp::add_i($l,$width)),
                    nqp::if(nqp::isge_i($right,$n),($right = $n)),
                    (my int $end =
                      nqp::add_i($l,nqp::add_i($width,$width))),
                    nqp::if(nqp::isge_i($end,$n),($end = $n)),

                    (my int $i = $left),
                    (my int $j = $right),
                    (my int $k = nqp::sub_i($left,1)),

                    # Merge two runs: $A[i       .. i+width-1] and
                    #                 $A[i+width .. i+2*width-1]
                    # to $B or copy $A[i..n-1] to $B[] ( if(i+width >= n) )
                    nqp::while(
                      nqp::islt_i(($k = nqp::add_i($k,1)),$end),
                      nqp::if(
                        nqp::islt_i($i,$right) && (
                          nqp::isge_i($j,$end)
                            || nqp::islt_s(
                                 nqp::atpos_s($A,$i),
                                 nqp::atpos_s($A,$j)
                               )
                        ),
                        nqp::stmts(
                          (nqp::bindpos_s($B,$k,nqp::atpos_s($A,$i))),
                          ($i = nqp::add_i($i,1))
                        ),
                        nqp::stmts(
                          (nqp::bindpos_s($B,$k,nqp::atpos_s($A,$j))),
                          ($j = nqp::add_i($j,1))
                        )
                      )
                    ),
                    ($l = nqp::add_i($l,nqp::add_i($width,$width)))
                  )
                ),

                # Now work array $B is full of runs of length 2*width.
                # Copy array B to array A for next iteration.  A more
                # efficient implementation would swap the roles of A and B.
                (my Mu $temp := $B),($B := $A),($A := $temp),   # swap
                # Now array $A is full of runs of length 2*width.

                ($width = nqp::add_i($width,$width))
              )
            ),
            $A
          ),
          nqp::if(
            nqp::islt_i($n,2)
              || nqp::isle_s(nqp::atpos_s(sortable,0),nqp::atpos_s(sortable,1)),
            nqp::clone(sortable),  # we already have the result
            nqp::stmts(
              (my $R := nqp::setelems(nqp::list_s,2)),
              nqp::bindpos_s($R,0,nqp::atpos_s(sortable,1)),
              nqp::bindpos_s($R,1,nqp::atpos_s(sortable,0)),
              $R
            )
          )
        )
    }

    # https://en.wikipedia.org/wiki/Merge_sort#Bottom-up_implementation
    # Sort a native int array (or nqp::list_i) and return the result.
    # Uses the given int array as one of the buffers for performance reasons.
    # Please nqp::clone first if you want to keep the original intact.
    method MERGESORT-int(Mu \sortable) {
        nqp::if(
          nqp::isgt_i((my int $n = nqp::elems(sortable)),2),

          # $A has the items to sort; $B is a work array
          nqp::stmts(
            (my Mu $A := sortable),
            (my Mu $B := nqp::setelems(nqp::list_i,$n)),

            # Each 1-element run in $A is already "sorted"
            # Make successively longer sorted runs of length 2, 4, 8, 16...
            # until $A is wholly sorted
            (my int $width = 1),
            nqp::while(
              nqp::islt_i($width,$n),
              nqp::stmts(
                (my int $l = 0),

                # $A is full of runs of length $width
                nqp::while(
                  nqp::islt_i($l,$n),

                  nqp::stmts(
                    (my int $left  = $l),
                    (my int $right = nqp::add_i($l,$width)),
                    nqp::if(nqp::isge_i($right,$n),($right = $n)),
                    (my int $end =
                      nqp::add_i($l,nqp::add_i($width,$width))),
                    nqp::if(nqp::isge_i($end,$n),($end = $n)),

                    (my int $i = $left),
                    (my int $j = $right),
                    (my int $k = nqp::sub_i($left,1)),

                    # Merge two runs: $A[i       .. i+width-1] and
                    #                 $A[i+width .. i+2*width-1]
                    # to $B or copy $A[i..n-1] to $B[] ( if(i+width >= n) )
                    nqp::while(
                      nqp::islt_i(($k = nqp::add_i($k,1)),$end),
                      nqp::if(
                        nqp::islt_i($i,$right) && (
                          nqp::isge_i($j,$end)
                            || nqp::islt_i(
                                 nqp::atpos_i($A,$i),
                                 nqp::atpos_i($A,$j)
                               )
                        ),
                        nqp::stmts(
                          (nqp::bindpos_i($B,$k,nqp::atpos_i($A,$i))),
                          ($i = nqp::add_i($i,1))
                        ),
                        nqp::stmts(
                          (nqp::bindpos_i($B,$k,nqp::atpos_i($A,$j))),
                          ($j = nqp::add_i($j,1))
                        )
                      )
                    ),
                    ($l = nqp::add_i($l,nqp::add_i($width,$width)))
                  )
                ),

                # Now work array $B is full of runs of length 2*width.
                # Copy array B to array A for next iteration.  A more
                # efficient implementation would swap the roles of A and B.
                (my Mu $temp := $B),($B := $A),($A := $temp),   # swap
                # Now array $A is full of runs of length 2*width.

                ($width = nqp::add_i($width,$width))
              )
            ),
            $A
          ),
          nqp::if(
            nqp::islt_i($n,2)
              || nqp::isle_i(nqp::atpos_i(sortable,0),nqp::atpos_i(sortable,1)),
            nqp::clone(sortable),  # we already have the result
            nqp::stmts(
              (my $R := nqp::setelems(nqp::list_i,2)),
              nqp::bindpos_i($R,0,nqp::atpos_i(self,1)),
              nqp::bindpos_i($R,1,nqp::atpos_i(self,0)),
              $R
            )
          )
        )
    }

    # https://en.wikipedia.org/wiki/Merge_sort#Bottom-up_implementation
    # Sort a native num array (or nqp::list_n) and return the result.
    # Uses the given num array as one of the buffers for performance reasons.
    # Please nqp::clone first if you want to keep the original intact.
    method MERGESORT-num(Mu \sortable) {
        nqp::if(
          nqp::isgt_i((my int $n = nqp::elems(sortable)),2),

          # $A has the items to sort; $B is a work array
          nqp::stmts(
            (my Mu $A := sortable),
            (my Mu $B := nqp::setelems(nqp::list_n,$n)),

            # Each 1-element run in $A is already "sorted"
            # Make successively longer sorted runs of length 2, 4, 8, 16...
            # until $A is wholly sorted
            (my int $width = 1),
            nqp::while(
              nqp::islt_i($width,$n),
              nqp::stmts(
                (my int $l = 0),

                # $A is full of runs of length $width
                nqp::while(
                  nqp::islt_i($l,$n),

                  nqp::stmts(
                    (my int $left  = $l),
                    (my int $right = nqp::add_i($l,$width)),
                    nqp::if(nqp::isge_i($right,$n),($right = $n)),
                    (my int $end =
                      nqp::add_i($l,nqp::add_i($width,$width))),
                    nqp::if(nqp::isge_i($end,$n),($end = $n)),

                    (my int $i = $left),
                    (my int $j = $right),
                    (my int $k = nqp::sub_i($left,1)),

                    # Merge two runs: $A[i       .. i+width-1] and
                    #                 $A[i+width .. i+2*width-1]
                    # to $B or copy $A[i..n-1] to $B[] ( if(i+width >= n) )
                    nqp::while(
                      nqp::islt_i(($k = nqp::add_i($k,1)),$end),
                      nqp::if(
                        nqp::islt_i($i,$right) && (
                          nqp::isge_i($j,$end)
                            || nqp::islt_n(
                                 nqp::atpos_n($A,$i),
                                 nqp::atpos_n($A,$j)
                               )
                        ),
                        nqp::stmts(
                          (nqp::bindpos_n($B,$k,nqp::atpos_n($A,$i))),
                          ($i = nqp::add_i($i,1))
                        ),
                        nqp::stmts(
                          (nqp::bindpos_n($B,$k,nqp::atpos_n($A,$j))),
                          ($j = nqp::add_i($j,1))
                        )
                      )
                    ),
                    ($l = nqp::add_i($l,nqp::add_i($width,$width)))
                  )
                ),

                # Now work array $B is full of runs of length 2*width.
                # Copy array B to array A for next iteration.  A more
                # efficient implementation would swap the roles of A and B.
                (my Mu $temp := $B),($B := $A),($A := $temp),   # swap
                # Now array $A is full of runs of length 2*width.

                ($width = nqp::add_i($width,$width))
              )
            ),
            $A
          ),
          nqp::if(
            nqp::islt_i($n,2)
              || nqp::isle_n(nqp::atpos_n(sortable,0),nqp::atpos_n(sortable,1)),
            nqp::clone(self),  # we already have the result
            nqp::stmts(
              (my $R := nqp::setelems(nqp::list_n,2)),
              nqp::bindpos_n($R,0,nqp::atpos_n(sortable,1)),
              nqp::bindpos_n($R,1,nqp::atpos_n(sortable,0)),
              $R
            )
          )
        )
    }
}

# vim: ft=perl6 expandtab sw=4
