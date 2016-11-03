# this is actually part of the Array class

    sub set-shape(\arr, \shape) {
        nqp::stmts(
          (my $shape := nqp::if(
            Metamodel::EnumHOW.ACCEPTS(shape.HOW),
            shape.^elems,
            nqp::if(
              nqp::istype(shape,List),
              shape,
              shape.list
            )
          )),
          (my int $dimensions = $shape.elems),  # reifies
          nqp::bindattr(arr,List,'$!reified',
            Rakudo::Internals.SHAPED-ARRAY-STORAGE($shape,nqp::knowhow,Mu)),
          arr does nqp::if(
            nqp::iseq_i($dimensions,1),
            Shaped1Array[Mu],
            nqp::if(
              nqp::iseq_i($dimensions,2),
              Shaped2Array[Mu],
              nqp::if(nqp::iseq_i($dimensions,3),
                Shaped3Array[Mu],
                ShapedArray[Mu]
              )
            )
          ),
          arr.^set_name('Array'),
          nqp::bindattr(arr, arr.WHAT, '$!shape', $shape),
          arr
        )
    }

# vim: ft=perl6 expandtab sw=4
