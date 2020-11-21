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
multi sub pop(@a) is raw { @a.pop }

proto sub shift($, *%) {*}
multi sub shift(@a) is raw { @a.shift }

proto sub push($, |) {*}
multi sub push(\a,   \b       ) { a.push:   b }
multi sub push(\a, **@b is raw) { a.push: |@b }

proto sub append($, |) {*}
multi sub append(\a,   \b       ) { a.append:  b }
multi sub append(\a, **@b is raw) { a.append: @b }

proto sub unshift($, |) {*}
multi sub unshift(\a,   \b       ) { a.unshift:   b }
multi sub unshift(\a, **@b is raw) { a.unshift: |@b }

proto sub prepend($, |) {*}
multi sub prepend(\a,   \b       ) { a.prepend:  b }
multi sub prepend(\a, **@b is raw) { a.prepend: @b }

proto sub splice($, |) {*}
multi sub splice(@arr, |c) { @arr.splice(|c) }

# vim: expandtab shiftwidth=4
