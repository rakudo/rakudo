# this is actually part of the Array class

    my constant \dim2role =
      nqp::list(ShapedArray,Shaped1Array,Shaped2Array,Shaped3Array);

    sub set-shape(\base, \shape) is raw {
        nqp::stmts(
          (my $shape := nqp::decont(nqp::if(
            nqp::istype(shape,List),
            shape,
            shape.list
          ))),
          nqp::if(
            (my int $dims = $shape.elems),  # reifies
            nqp::stmts(
              nqp::unless(
                nqp::iseq_i($dims,1)
                  && nqp::istype(                 # ignore single [*] shape
                       nqp::atpos(nqp::getattr($shape,List,'$!reified'),0),
                       Whatever),
                nqp::stmts(
                  (my $what := base.WHAT.^mixin(
                    nqp::atpos(dim2role,nqp::isle_i($dims,3) && $dims))
                  ),
                  nqp::if(                        # correct name if needed
                    nqp::isne_s($what.^name,base.^name),
                    $what.^set_name(base.^name)
                  ),
                  nqp::p6bindattrinvres(
                    nqp::p6bindattrinvres(
                      nqp::create($what),List,'$!reified',
                        Rakudo::Internals.SHAPED-ARRAY-STORAGE(
                          $shape,nqp::knowhow,Mu)),
                    $what,'$!shape',$shape)
                ),
                nqp::create(base.WHAT)
              )
            ),
            X::NotEnoughDimensions.new(
              operation         => 'create',
              got-dimensions    => 0,
              needed-dimensions => '',
            ).throw
          )
        )
    }

# vim: ft=perl6 expandtab sw=4
