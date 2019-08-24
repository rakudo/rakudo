# The [...] term creates an Array.
proto sub circumfix:<[ ]>(Mu $?, *%) {*}
multi sub circumfix:<[ ]>() {
    nqp::create(Array)
}
multi sub circumfix:<[ ]>(Iterable:D \iterable) {
    nqp::if(
      nqp::iscont(iterable),
      nqp::p6bindattrinvres(
        nqp::create(Array),List,'$!reified',
        nqp::stmts(
          (my \scalar := nqp::create(Scalar)),
          nqp::bindattr(
            scalar, Scalar, '$!descriptor',
            BEGIN nqp::getcurhllsym('default_cont_spec')
          ),
          nqp::bindattr(scalar,Scalar,'$!value',nqp::decont(iterable)),
          nqp::bindpos((my \reified := nqp::create(IterationBuffer)),0,scalar),
          reified
        )
      ),
      nqp::if(
        nqp::istype(iterable,List) && nqp::isfalse(iterable.is-lazy),
        Array.from-list(iterable),
        Array.from-iterator(iterable.iterator)
      )
    )
}
multi sub circumfix:<[ ]>(Mu \x) {   # really only for [$foo]
    nqp::p6bindattrinvres(
      nqp::create(Array),List,'$!reified',
      nqp::stmts(
        nqp::bindpos(
          (my \reified := nqp::create(IterationBuffer)),
          0,
          nqp::p6scalarwithvalue(
            (BEGIN nqp::getcurhllsym('default_cont_spec')),
            nqp::decont(x)
          )
        ),
        reified
      )
    )
}

proto sub pop($, *%) {*}
multi sub pop(@a) { @a.pop }

proto sub shift($, *%) {*}
multi sub shift(@a) { @a.shift }

proto sub push($, |) {*}
multi sub push(\a, |elems) {
    nqp::elems(nqp::getattr(elems,Capture,q/%!hash/))
      ?? X::AdHoc.new( payload => "Unexpected named argument '{elems.hash.head.key}' passed" ).throw
      !! a.push: |elems
}

proto sub append($, |) {*}
multi sub append(\a, |elems) {
    nqp::elems(nqp::getattr(elems,Capture,q/%!hash/))
      ?? X::AdHoc.new( payload => "Unexpected named argument '{elems.hash.head.key}' passed" ).throw
      !! a.append: |elems
}

proto sub unshift($, |) {*}
multi sub unshift(\a, |elems) {
    nqp::elems(nqp::getattr(elems,Capture,q/%!hash/))
      ?? X::AdHoc.new( payload => "Unexpected named argument '{elems.hash.head.key}' passed" ).throw
      !! a.unshift: |elems
}

proto sub prepend($, |) {*}
multi sub prepend(\a, |elems) {
    nqp::elems(nqp::getattr(elems,Capture,q/%!hash/))
      ?? X::AdHoc.new( payload => "Unexpected named argument '{elems.hash.head.key}' passed" ).throw
      !! a.prepend: |elems
}

proto sub splice($, |) {*}
multi sub splice(@arr, |c) { @arr.splice(|c) }

# vim: ft=perl6 expandtab sw=4
