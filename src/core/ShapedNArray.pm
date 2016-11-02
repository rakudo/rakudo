
    sub set-shape(\arr, \values, \shape) {
        my $shape := Metamodel::EnumHOW.ACCEPTS(shape.HOW)
          ?? shape.^elems
          !! shape;
        if $shape.DEFINITE {
            my \list-shape = nqp::istype($shape,List) ?? $shape !! $shape.list;
            nqp::bindattr(arr,List,'$!reified',
              Rakudo::Internals.SHAPED-ARRAY-STORAGE(
                list-shape,nqp::knowhow,Mu));
            my int $dimensions = list-shape.elems;
            arr does
              nqp::if(nqp::iseq_i($dimensions,1), Shaped1Array[Mu],
              nqp::if(nqp::iseq_i($dimensions,2), Shaped2Array[Mu],
              nqp::if(nqp::iseq_i($dimensions,3), Shaped3Array[Mu],
              ShapedArray[Mu]
            )));
            arr.^set_name('Array');
            nqp::bindattr(arr, arr.WHAT, '$!shape', list-shape);
            arr.STORE(values) if values;
        }
        else {
            arr.STORE(values);
        }
        arr
    }

# vim: ft=perl6 expandtab sw=4
