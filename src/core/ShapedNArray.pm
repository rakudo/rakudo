# this is actually part of the Array class

    constant dim2type := nqp::list(Mu,Shaped1Array,Shaped2Array,Shaped3Array);
    constant ArrayN := ShapedArray;

    sub set-shape(\base, \shape) is raw {
        nqp::stmts(
          (my $shape := nqp::decont(nqp::if(
            Metamodel::EnumHOW.ACCEPTS(shape.HOW),
            shape.^elems,
            nqp::if(
              nqp::istype(shape,List),
              shape,
              shape.list
            )
          ))),
          nqp::if(
            (my int $dimensions = $shape.elems),  # reifies
            nqp::stmts(
              nqp::unless(
                nqp::iseq_i($dimensions,1)
                  && nqp::istype(                 # ignore single [*] shape
                       nqp::atpos(nqp::getattr($shape,List,'$!reified'),0),
                       Whatever),
                nqp::stmts(
                  (my $what := base.WHAT.^mixin(
                    nqp::ifnull(nqp::atpos(dim2type,$dimensions),ArrayN))),
                  nqp::if(
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
