# this is actually part of the Array class

    my constant \dim2role =
      nqp::list(ShapedArray,Shaped1Array,Shaped2Array,Shaped3Array);

    proto sub set-shape(|) {*}
    multi sub set-shape(\base, Whatever) is raw {
        nqp::create(base.WHAT)
    }
    multi sub set-shape(\base, \shape) is raw {
        set-shape(base, shape.List)
    }
    multi sub set-shape(\base, List:D \shape) is raw {
        my int $dims = shape.elems;  # reifies
        my $reified := nqp::getattr(nqp::decont(shape),List,'$!reified');

        # just a list with Whatever, so no shape
        if nqp::iseq_i($dims,1)
          && nqp::istype(nqp::atpos($reified,0),Whatever) {
            nqp::create(base.WHAT)
        }

        # we haz dimensions
        elsif nqp::isgt_i($dims,0) {
            my $what := base.WHAT.^mixin(
              nqp::atpos(dim2role,nqp::isle_i($dims,3) && $dims)
            );
            $what.^set_name(base.^name)           # correct name if needed
              if nqp::isne_s($what.^name,base.^name);

            my $array := nqp::p6bindattrinvres(
              nqp::create($what),List,'$!reified',
              Rakudo::Internals.SHAPED-ARRAY-STORAGE(shape,nqp::knowhow,Mu)
            );
            nqp::p6bindattrinvres($array,$what,'$!shape',nqp::decont(shape))
        }

        # flatland
        else {
            X::NotEnoughDimensions.new(
              operation         => 'create',
              got-dimensions    => 0,
              needed-dimensions => '',
            ).throw
        }
    }

# vim: expandtab shiftwidth=4
