my class Rakudo::Sorting is implementation-detail {

    # Classifying costs a walk before a single comparison is made, which a
    # shorter list does not do enough comparisons to repay.  Sorting
    # elements and sorting by an extracted key cross over alike here.
    my constant SORT-CLASSIFY-FROM = 16;

    # helper sub to report whether reading a slot runs no code of its own,
    # which is true of a plain value and of a Scalar, and not of a
    # container that answers with a FETCH, such as a Proxy.  The slot is
    # taken as Mu so that binding it does not read it either.
    sub slot-reads-the-same(Mu \slot) {
        nqp::not_i(nqp::iscont(slot))
          || nqp::eqaddr(nqp::what_nd(slot),Scalar)
    }

    # https://en.wikipedia.org/wiki/Merge_sort#Bottom-up_implementation
    # The parameter is the HLL List to be sorted *in place* using simple cmp.
    method MERGESORT-REIFIED-LIST(\list) {
        # $A has the items to sort; $B is a work array
        my $A := nqp::getattr(list,List,'$!reified');
        my int $n = nqp::elems($A);

        my int $kind = nqp::isge_i($n,SORT-CLASSIFY-FROM)
          ?? self!SORT-ELEMENT-KIND(list)
          !! 0;
        return self!MERGESORT-REIFIED-LIST-Int(list)
          if nqp::iseq_i($kind,1);
        return self!MERGESORT-REIFIED-LIST-Str(list)
          if nqp::iseq_i($kind,2);

        nqp::if(
          nqp::isgt_i($n,2),
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
                      nqp::islt_i(++$k,$end),
                      nqp::if(
                        nqp::islt_i($i,$right) && (
                          nqp::isge_i($j,$end)
                            || nqp::eqaddr(
                                 (my $cmp := nqp::atpos($A,$i) cmp nqp::atpos($A,$j)),
                                 Order::Less
                               )
                            || (nqp::eqaddr($cmp,Order::Same)
                                 && nqp::iseq_i(nqp::cmp_i($i,$j),-1)
                               )
                        ),
                        nqp::stmts(
                          (nqp::bindpos($B,$k,nqp::atpos($A,$i))),
                          ++$i
                        ),
                        nqp::stmts(
                          (nqp::bindpos($B,$k,nqp::atpos($A,$j))),
                          ++$j
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
            )
          ),

          # N <= 2
          nqp::if(
            nqp::iseq_i($n,2) && nqp::eqaddr(
              nqp::atpos($A,0) cmp nqp::atpos($A,1),
              Order::More
            ),
            nqp::push($A,nqp::shift($A))  # wrong order, so swap
          )
        );

        nqp::p6bindattrinvres(list,List,'$!reified',$A)
    }

    # Report which specialized merge, if any, may sort the given HLL List:
    # 1 for a list of Int, 2 for a list of Str, 0 for anything else.  Every
    # element has to be concrete and of exactly that type: an Allomorph
    # carries both a numeric and a string value and picks the infix:<cmp>
    # candidate that breaks a numeric tie on the string, where nqp::cmp_I
    # calls the pair equal, so it would sort differently rather than
    # complain.
    method !SORT-ELEMENT-KIND(\list) {
        # The reified buffer is fetched here rather than passed in.  A list
        # built from a literal reifies into a bare VM array rather than an
        # IterationBuffer, and a bare VM array handed to a method call
        # arrives HLLized into a List, which nqp::elems cannot count.
        my $A := nqp::getattr(list,List,'$!reified');
        my int $n = nqp::elems($A);
        my int $kind = 0;

        # The first slot only says which type to expect.  Every element,
        # that one included, is then checked by the loop.
        nqp::if(
          nqp::isgt_i($n,2)
            && nqp::isgt_i(
                 ($kind = nqp::eqaddr(
                   (my $wanted := slot-reads-the-same(
                     my $slot := nqp::ifnull(nqp::atpos($A,0),Mu)
                   ) ?? nqp::what($slot) !! Mu),Int)
                   ?? 1
                   !! nqp::eqaddr($wanted,Str) ?? 2 !! 0),
                 0
               ),
          nqp::stmts(
            (my int $i = -1),
            nqp::while(
              nqp::islt_i(++$i,$n)
                && slot-reads-the-same(
                     $slot := nqp::ifnull(nqp::atpos($A,$i),Mu)
                   )
                && nqp::isconcrete(my $value := nqp::decont($slot))
                && nqp::eqaddr(nqp::what($value),$wanted),
              nqp::null
            ),
            nqp::unless(nqp::iseq_i($i,$n),($kind = 0))
          )
        );

        $kind
    }

    # Takes the HLL List to be sorted *in place*, every element of which is
    # exactly an Int, so cmp reduces to nqp::cmp_I on the two values, which
    # a Scalar element is decontainerized for.  The left run always sits
    # entirely before the right, so taking the left of an equal pair is
    # what the general merge's explicit tie-break amounts to.
    method !MERGESORT-REIFIED-LIST-Int(\list) {
        # $A has the items to sort; $B is a work array
        my $A := nqp::getattr(list,List,'$!reified');
        my int $n = nqp::elems($A);
        my $B := nqp::setelems(nqp::create(IterationBuffer),$n);

        # Each 1-element run in $A is already "sorted"
        # Make successively longer sorted runs of length 2, 4, 8, 16...
        # until $A is wholly sorted
        my int $width = 1;
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
                  nqp::islt_i(++$k,$end),
                  nqp::if(
                    nqp::islt_i($i,$right) && (
                      nqp::isge_i($j,$end)
                        || nqp::isle_i(nqp::cmp_I(
                             nqp::decont(nqp::atpos($A,$i)),
                             nqp::decont(nqp::atpos($A,$j))
                           ),0)
                    ),
                    nqp::stmts(
                      (nqp::bindpos($B,$k,nqp::atpos($A,$i))),
                      ++$i
                    ),
                    nqp::stmts(
                      (nqp::bindpos($B,$k,nqp::atpos($A,$j))),
                      ++$j
                    )
                  )
                ),
                ($l = nqp::add_i($l,nqp::add_i($width,$width)))
              )
            ),

            # Now work array $B is full of runs of length 2*width.
            (my $temp := $B),($B := $A),($A := $temp),   # swap
            # Now array $A is full of runs of length 2*width.

            ($width = nqp::add_i($width,$width))
          )
        );

        nqp::p6bindattrinvres(list,List,'$!reified',$A)
    }

    # As MERGESORT-REIFIED-LIST-Int, for a list of exactly Str, where cmp
    # reduces to nqp::cmp_s on the unboxed strings.
    method !MERGESORT-REIFIED-LIST-Str(\list) {
        # $A has the items to sort; $B is a work array
        my $A := nqp::getattr(list,List,'$!reified');
        my int $n = nqp::elems($A);
        my $B := nqp::setelems(nqp::create(IterationBuffer),$n);

        # Each 1-element run in $A is already "sorted"
        # Make successively longer sorted runs of length 2, 4, 8, 16...
        # until $A is wholly sorted
        my int $width = 1;
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
                  nqp::islt_i(++$k,$end),
                  nqp::if(
                    nqp::islt_i($i,$right) && (
                      nqp::isge_i($j,$end)
                        || nqp::isle_i(nqp::cmp_s(
                             nqp::unbox_s(nqp::decont(nqp::atpos($A,$i))),
                             nqp::unbox_s(nqp::decont(nqp::atpos($A,$j)))
                           ),0)
                    ),
                    nqp::stmts(
                      (nqp::bindpos($B,$k,nqp::atpos($A,$i))),
                      ++$i
                    ),
                    nqp::stmts(
                      (nqp::bindpos($B,$k,nqp::atpos($A,$j))),
                      ++$j
                    )
                  )
                ),
                ($l = nqp::add_i($l,nqp::add_i($width,$width)))
              )
            ),

            # Now work array $B is full of runs of length 2*width.
            (my $temp := $B),($B := $A),($A := $temp),   # swap
            # Now array $A is full of runs of length 2*width.

            ($width = nqp::add_i($width,$width))
          )
        );

        nqp::p6bindattrinvres(list,List,'$!reified',$A)
    }

    # Takes the HLL List to be sorted *in place* using a comparator
    method MERGESORT-REIFIED-LIST-WITH(\list, &comparator) {
        nqp::eqaddr(&comparator.returns,Order:D)
          ?? Rakudo::Sorting.MERGESORT-REIFIED-LIST-WITH-enum(list, &comparator)
          !! Rakudo::Sorting.MERGESORT-REIFIED-LIST-WITH-int(list, &comparator)
    }

    # Takes the HLL List to be sorted *in place* using the comparator
    # that is supposed to return some value that can be coerced to an int
    method MERGESORT-REIFIED-LIST-WITH-int(\list, &comparator) {
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
                      nqp::islt_i(++$k,$end),
                      nqp::if(
                        nqp::islt_i($i,$right) && (
                          nqp::isge_i($j,$end)
                            || nqp::islt_i(
                                 (my int $cmp = comparator(
                                   nqp::atpos($A,$i),
                                   nqp::atpos($A,$j)
                                 )),
                                 0
                               )
                            || (nqp::iseq_i($cmp,0)
                                 && nqp::islt_i(nqp::cmp_i($i,$j),0)
                               )
                        ),
                        nqp::stmts(
                          (nqp::bindpos($B,$k,nqp::atpos($A,$i))),
                          ++$i
                        ),
                        nqp::stmts(
                          (nqp::bindpos($B,$k,nqp::atpos($A,$j))),
                          ++$j
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
            )
          ),

          # N <= 2
          nqp::if(
            nqp::iseq_i($n,2) && nqp::isgt_i(
              comparator(nqp::atpos($A,0),nqp::atpos($A,1)),
              0
            ),
            nqp::push($A,nqp::shift($A))  # wrong order, so swap
          )
        );

        nqp::p6bindattrinvres(list,List,'$!reified',$A)
    }

    # Takes the HLL List to be sorted *in place* using the comparator
    # that is supposed to return an Order enum
    method MERGESORT-REIFIED-LIST-WITH-enum(\list, &comparator) {
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
                      nqp::islt_i(++$k,$end),
                      nqp::if(
                        nqp::islt_i($i,$right) && (
                          nqp::isge_i($j,$end)
                            || nqp::eqaddr(
                                 (my $cmp := comparator(nqp::atpos($A,$i),nqp::atpos($A,$j))),
                                 Order::Less
                               )
                            || (nqp::eqaddr($cmp,Order::Same)
                                 && nqp::iseq_i(nqp::cmp_i($i,$j),-1)
                               )
                        ),
                        nqp::stmts(
                          (nqp::bindpos($B,$k,nqp::atpos($A,$i))),
                          ++$i
                        ),
                        nqp::stmts(
                          (nqp::bindpos($B,$k,nqp::atpos($A,$j))),
                          ++$j
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
            )
          ),

          # N <= 2
          nqp::if(
            nqp::iseq_i($n,2) && nqp::eqaddr(
              comparator(nqp::atpos($A,0),nqp::atpos($A,1)),
              Order::More
            ),
            nqp::push($A,nqp::shift($A))  # wrong order, so swap
          )
        );

        nqp::p6bindattrinvres(list,List,'$!reified',$A)
    }

    # helper sub to handle degenerate sort for indices
    sub degenerate-indices(\list, :$swap --> Nil) {
        my $O := nqp::getattr(list,List,'$!reified');
        nqp::if(
          $swap,
          nqp::stmts(  # swapping, implies 2 elements
            nqp::bindpos($O,0,1),
            nqp::bindpos($O,1,0)
          ),
          nqp::if(
            (my int $n = nqp::elems($O)),
            nqp::stmts(
              nqp::bindpos($O,0,0),
              nqp::if(
                nqp::iseq_i($n,2),
                nqp::bindpos($O,1,1)
              )
            )
          )
        );
    }

    # Takes the HLL List to be sorted *in place* using the mapper
    method MERGESORT-REIFIED-LIST-AS(\list, &mapper, :$indices) {
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
            # $kind reports the one type every key came out as, as
            # !SORT-ELEMENT-KIND reports it for elements, and stays -1
            # until the first key is seen.  A key inside a container does
            # not qualify at all, whatever it holds: the mapper runs over
            # the whole list before any comparison, so a later call to it
            # can still change a key already classified.
            (my int $kind = nqp::isge_i($n,SORT-CLASSIFY-FROM) ?? -1 !! 0),
            nqp::while(  # set up the Schwartz and the initial indexes
              nqp::islt_i(($s = nqp::add_i($s,1)),$n),
              nqp::stmts(
                nqp::bindpos($S,nqp::bindpos_i($A,$s,$s),
                  (my $key := mapper(nqp::atpos($O,$s)))),
                nqp::unless(
                  nqp::iseq_i($kind,0),
                  nqp::stmts(
                    (my $keytype := nqp::isconcrete_nd($key)
                      ?? nqp::what_nd($key)
                      !! Mu),
                    (my int $this = nqp::eqaddr($keytype,Int)
                      ?? 1
                      !! nqp::eqaddr($keytype,Str) ?? 2 !! 0),
                    nqp::if(
                      nqp::iseq_i($kind,-1),
                      ($kind = $this),
                      nqp::unless(nqp::iseq_i($kind,$this),($kind = 0))
                    )
                  )
                )
              )
            ),
            nqp::if(
              nqp::isgt_i($kind,0),
              nqp::iseq_i($kind,1)
                ?? self!MERGESORT-AS-Int(list,$S,$indices)
                !! self!MERGESORT-AS-Str(list,$S,$indices),
              nqp::stmts(

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
                      nqp::islt_i(++$k,$end),
                      nqp::if(
                        nqp::islt_i($i,$right) && (
                          nqp::isge_i($j,$end)
                            || nqp::eqaddr(
                                 (my $cmp := nqp::atpos($S,nqp::atpos_i($A,$i))
                                         cmp nqp::atpos($S,nqp::atpos_i($A,$j))),
                                 Order::Less
                               )
                            || (nqp::eqaddr($cmp,Order::Same)
                                 && nqp::iseq_i(nqp::cmp_i($i,$j),-1)
                               )
                        ),
                        nqp::stmts(
                          (nqp::bindpos_i($B,$k,nqp::atpos_i($A,$i))),
                          ++$i
                        ),
                        nqp::stmts(
                          (nqp::bindpos_i($B,$k,nqp::atpos_i($A,$j))),
                          ++$j
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
            # repurpose the Schwartz for the result
            nqp::if(
              $indices,
              nqp::while(   # indices only
                nqp::islt_i(++$s,$n),
                nqp::bindpos($S,$s,nqp::atpos_i($A,$s))
              ),
              nqp::while(   # actual values
                nqp::islt_i(++$s,$n),
                nqp::bindpos($S,$s,nqp::atpos($O,nqp::atpos_i($A,$s)))
              )
            ),
            nqp::bindattr(list,List,'$!reified',$S)
              )
            )
          ),

          # N <= 2
          nqp::if(
            nqp::iseq_i($n,2) && nqp::eqaddr(
              mapper(nqp::atpos($O,0)) cmp mapper(nqp::atpos($O,1)),
              Order::More
            ),
            nqp::if(  # wrong order, so swap
              $indices,
              degenerate-indices(list, :swap),
              nqp::push($O,nqp::shift($O))
            ),
            nqp::if(  # no swapping needed
              $indices,
              degenerate-indices(list)
            )
          )
        );

        list   # we did changes in place
    }
    # Sorts the index array against a Schwartz of Int keys, where cmp reduces to nqp::cmp_I. The left run always
    # sits entirely before the right, so taking the left of an equal pair is
    # what the general merge's explicit tie-break amounts to and leaves the
    # result, or the indices that produced it, in that same buffer.
    method !MERGESORT-AS-Int(\list, \S, $indices) {
        my $O := nqp::getattr(list,List,'$!reified');   # Original
        my int $n = nqp::elems(S);
        my $A := nqp::setelems(nqp::list_i,$n);         # indexes to sort
        my $B := nqp::setelems(nqp::list_i,$n);         # work array
        my int $s = -1;
        nqp::while(
          nqp::islt_i(($s = nqp::add_i($s,1)),$n),
          nqp::bindpos_i($A,$s,$s)
        );

        # Each 1-element run in $A is already "sorted"
        # Make successively longer sorted runs of length 2, 4, 8, 16...
        # until $A is wholly sorted
        my int $width = 1;
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
                  nqp::islt_i(++$k,$end),
                  nqp::if(
                    nqp::islt_i($i,$right) && (
                      nqp::isge_i($j,$end)
                        || nqp::isle_i(
                             nqp::cmp_I(
                               nqp::atpos(S,nqp::atpos_i($A,$i)),
                               nqp::atpos(S,nqp::atpos_i($A,$j))
                             ),0)
                    ),
                    nqp::stmts(
                      (nqp::bindpos_i($B,$k,nqp::atpos_i($A,$i))),
                      ++$i
                    ),
                    nqp::stmts(
                      (nqp::bindpos_i($B,$k,nqp::atpos_i($A,$j))),
                      ++$j
                    )
                  )
                ),
                ($l = nqp::add_i($l,nqp::add_i($width,$width)))
              )
            ),

            # Now work array $B is full of runs of length 2*width.
            (my $temp := $B),($B := $A),($A := $temp),   # swap
            # Now array $A is full of runs of length 2*width.

            ($width = nqp::add_i($width,$width))
          )
        );

        $s = -1;
        # repurpose the Schwartz for the result
        nqp::if(
          $indices,
          nqp::while(   # indices only
            nqp::islt_i(++$s,$n),
            nqp::bindpos(S,$s,nqp::atpos_i($A,$s))
          ),
          nqp::while(   # actual values
            nqp::islt_i(++$s,$n),
            nqp::bindpos(S,$s,nqp::atpos($O,nqp::atpos_i($A,$s)))
          )
        );
        nqp::bindattr(list,List,'$!reified',S)
    }

    # Sorts the index array against a Schwartz of Str keys, where cmp reduces to nqp::cmp_s on the unboxed strings and leaves the
    # result, or the indices that produced it, in that same buffer.
    method !MERGESORT-AS-Str(\list, \S, $indices) {
        my $O := nqp::getattr(list,List,'$!reified');   # Original
        my int $n = nqp::elems(S);
        my $A := nqp::setelems(nqp::list_i,$n);         # indexes to sort
        my $B := nqp::setelems(nqp::list_i,$n);         # work array
        my int $s = -1;
        nqp::while(
          nqp::islt_i(($s = nqp::add_i($s,1)),$n),
          nqp::bindpos_i($A,$s,$s)
        );

        # Each 1-element run in $A is already "sorted"
        # Make successively longer sorted runs of length 2, 4, 8, 16...
        # until $A is wholly sorted
        my int $width = 1;
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
                  nqp::islt_i(++$k,$end),
                  nqp::if(
                    nqp::islt_i($i,$right) && (
                      nqp::isge_i($j,$end)
                        || nqp::isle_i(
                             nqp::cmp_s(
                               nqp::unbox_s(nqp::atpos(S,nqp::atpos_i($A,$i))),
                               nqp::unbox_s(nqp::atpos(S,nqp::atpos_i($A,$j)))
                             ),0)
                    ),
                    nqp::stmts(
                      (nqp::bindpos_i($B,$k,nqp::atpos_i($A,$i))),
                      ++$i
                    ),
                    nqp::stmts(
                      (nqp::bindpos_i($B,$k,nqp::atpos_i($A,$j))),
                      ++$j
                    )
                  )
                ),
                ($l = nqp::add_i($l,nqp::add_i($width,$width)))
              )
            ),

            # Now work array $B is full of runs of length 2*width.
            (my $temp := $B),($B := $A),($A := $temp),   # swap
            # Now array $A is full of runs of length 2*width.

            ($width = nqp::add_i($width,$width))
          )
        );

        $s = -1;
        # repurpose the Schwartz for the result
        nqp::if(
          $indices,
          nqp::while(   # indices only
            nqp::islt_i(++$s,$n),
            nqp::bindpos(S,$s,nqp::atpos_i($A,$s))
          ),
          nqp::while(   # actual values
            nqp::islt_i(++$s,$n),
            nqp::bindpos(S,$s,nqp::atpos($O,nqp::atpos_i($A,$s)))
          )
        );
        nqp::bindattr(list,List,'$!reified',S)
    }


    # Takes the HLL List to be sorted *in place* using the comparator
    # and always produce indices
    method MERGESORT-REIFIED-LIST-INDICES(\list, &comparator) {
        nqp::if(
          nqp::isgt_i((my int $n = nqp::elems(
            my $O := nqp::getattr(list,List,'$!reified')    # Original
          )),2),
          nqp::stmts(     # we actually need to sort
            (my $A := nqp::setelems(nqp::list_i,$n)),       # indexes to sort
            (my $B := nqp::setelems(nqp::list_i,$n)),       # work array
            (my int $s = -1),
            nqp::while(  # set up the initial indexes
              nqp::islt_i(++$s,$n),
              nqp::bindpos_i($A,$s,$s)
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
                      nqp::islt_i(++$k,$end),
                      nqp::if(
                        nqp::islt_i($i,$right) && (
                          nqp::isge_i($j,$end)
                            || nqp::eqaddr(
                                 (my $cmp := comparator(
                                   nqp::atpos($O,nqp::atpos_i($A,$i)),
                                   nqp::atpos($O,nqp::atpos_i($A,$j))
                                 )),
                                 Order::Less
                               )
                            || (nqp::eqaddr($cmp,Order::Same)
                                 && nqp::iseq_i(nqp::cmp_i($i,$j),-1)
                               )
                        ),
                        nqp::stmts(
                          (nqp::bindpos_i($B,$k,nqp::atpos_i($A,$i))),
                          ++$i
                        ),
                        nqp::stmts(
                          (nqp::bindpos_i($B,$k,nqp::atpos_i($A,$j))),
                          ++$j
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
            # repurpose the Original for the indices
            nqp::while(   # indices only
              nqp::islt_i(++$s,$n),
              nqp::bindpos($O,$s,nqp::atpos_i($A,$s))
            )
          ),

          # N <= 2
          degenerate-indices(
            list,
            :swap(nqp::iseq_i($n,2) && nqp::eqaddr(
              comparator(nqp::atpos($O,0),nqp::atpos($O,1)),
              Order::More
            ))
          )
        );

        list   # we did changes in place
    }

#- start of generated part of sorting strarray logic --------------------------
#- Generated on 2022-02-17T16:35:04+01:00 by ./tools/build/makeNATIVE_SORTING.raku
#- PLEASE DON'T CHANGE ANYTHING BELOW THIS LINE

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
            (my Mu $B := nqp::setelems(nqp::create(nqp::what(sortable)),$n)),

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
                      nqp::islt_i(++$k,$end),
                      nqp::if(
                        nqp::islt_i($i,$right) && (
                          nqp::isge_i($j,$end)
                            || nqp::islt_s(
                                 nqp::atpos_s($A,$i),
                                 nqp::atpos_s($A,$j)
                               )
                        ),
                        nqp::stmts(
                          nqp::bindpos_s($B,$k,nqp::atpos_s($A,$i)),
                          ++$i
                        ),
                        nqp::stmts(
                          nqp::bindpos_s($B,$k,nqp::atpos_s($A,$j)),
                          ++$j
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
          nqp::stmts(   # 2 elements or less
            (my \result := nqp::clone(sortable)),
            nqp::unless(
              nqp::islt_i($n,2)
                || nqp::isle_s(nqp::atpos_s(result,0),nqp::atpos_s(result,1)),
              nqp::push_s(result,nqp::shift_s(result))
            ),
            result
          )
        )
    }
#- PLEASE DON'T CHANGE ANYTHING ABOVE THIS LINE
#- end of generated part of sorting strarray logic ----------------------------

#- start of generated part of sorting intarray logic --------------------------
#- Generated on 2022-02-17T16:35:04+01:00 by ./tools/build/makeNATIVE_SORTING.raku
#- PLEASE DON'T CHANGE ANYTHING BELOW THIS LINE

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
            (my Mu $B := nqp::setelems(nqp::create(nqp::what(sortable)),$n)),

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
                      nqp::islt_i(++$k,$end),
                      nqp::if(
                        nqp::islt_i($i,$right) && (
                          nqp::isge_i($j,$end)
                            || nqp::islt_i(
                                 nqp::atpos_i($A,$i),
                                 nqp::atpos_i($A,$j)
                               )
                        ),
                        nqp::stmts(
                          nqp::bindpos_i($B,$k,nqp::atpos_i($A,$i)),
                          ++$i
                        ),
                        nqp::stmts(
                          nqp::bindpos_i($B,$k,nqp::atpos_i($A,$j)),
                          ++$j
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
          nqp::stmts(   # 2 elements or less
            (my \result := nqp::clone(sortable)),
            nqp::unless(
              nqp::islt_i($n,2)
                || nqp::isle_i(nqp::atpos_i(result,0),nqp::atpos_i(result,1)),
              nqp::push_i(result,nqp::shift_i(result))
            ),
            result
          )
        )
    }
#- PLEASE DON'T CHANGE ANYTHING ABOVE THIS LINE
#- end of generated part of sorting intarray logic ----------------------------

#- start of generated part of sorting uintarray logic --------------------------
#- Generated on 2022-02-17T16:35:04+01:00 by ./tools/build/makeNATIVE_SORTING.raku
#- PLEASE DON'T CHANGE ANYTHING BELOW THIS LINE

    # https://en.wikipedia.org/wiki/Merge_sort#Bottom-up_implementation
    # Sort a native uint array (or nqp::list_i) and return the result.
    # Uses the given uint array as one of the buffers for performance reasons.
    # Please nqp::clone first if you want to keep the original intact.
    method MERGESORT-uint(Mu \sortable) {
        nqp::if(
          nqp::isgt_i((my int $n = nqp::elems(sortable)),2),

          # $A has the items to sort; $B is a work array
          nqp::stmts(
            (my Mu $A := sortable),
            (my Mu $B := nqp::setelems(nqp::create(nqp::what(sortable)),$n)),

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
                      nqp::islt_i(++$k,$end),
                      nqp::if(
                        nqp::islt_i($i,$right) && (
                          nqp::isge_i($j,$end)
                            || nqp::islt_i(
                                 nqp::atpos_u($A,$i),
                                 nqp::atpos_u($A,$j)
                               )
                        ),
                        nqp::stmts(
                          nqp::bindpos_u($B,$k,nqp::atpos_u($A,$i)),
                          ++$i
                        ),
                        nqp::stmts(
                          nqp::bindpos_u($B,$k,nqp::atpos_u($A,$j)),
                          ++$j
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
          nqp::stmts(   # 2 elements or less
            (my \result := nqp::clone(sortable)),
            nqp::unless(
              nqp::islt_i($n,2)
                || nqp::isle_i(nqp::atpos_u(result,0),nqp::atpos_u(result,1)),
              nqp::push_i(result,nqp::shift_i(result))
            ),
            result
          )
        )
    }
#- PLEASE DON'T CHANGE ANYTHING ABOVE THIS LINE
#- end of generated part of sorting uintarray logic ----------------------------

#- start of generated part of sorting numarray logic --------------------------
#- Generated on 2022-02-17T16:35:04+01:00 by ./tools/build/makeNATIVE_SORTING.raku
#- PLEASE DON'T CHANGE ANYTHING BELOW THIS LINE

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
            (my Mu $B := nqp::setelems(nqp::create(nqp::what(sortable)),$n)),

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
                      nqp::islt_i(++$k,$end),
                      nqp::if(
                        nqp::islt_i($i,$right) && (
                          nqp::isge_i($j,$end)
                            || nqp::islt_n(
                                 nqp::atpos_n($A,$i),
                                 nqp::atpos_n($A,$j)
                               )
                        ),
                        nqp::stmts(
                          nqp::bindpos_n($B,$k,nqp::atpos_n($A,$i)),
                          ++$i
                        ),
                        nqp::stmts(
                          nqp::bindpos_n($B,$k,nqp::atpos_n($A,$j)),
                          ++$j
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
          nqp::stmts(   # 2 elements or less
            (my \result := nqp::clone(sortable)),
            nqp::unless(
              nqp::islt_i($n,2)
                || nqp::isle_n(nqp::atpos_n(result,0),nqp::atpos_n(result,1)),
              nqp::push_n(result,nqp::shift_n(result))
            ),
            result
          )
        )
    }
#- PLEASE DON'T CHANGE ANYTHING ABOVE THIS LINE
#- end of generated part of sorting numarray logic ----------------------------
}

# vim: expandtab shiftwidth=4
