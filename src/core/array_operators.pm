# close down the Array class
} 

# The [...] term creates an Array.
proto circumfix:<[ ]>(|) { * }
multi circumfix:<[ ]>() {
    nqp::create(Array)
}
multi circumfix:<[ ]>(Iterable:D \iterable) {
    my $reified;
    nqp::if(
      nqp::iscont(iterable),
      nqp::p6bindattrinvres(
        nqp::create(Array),List,'$!reified',
        nqp::stmts(
          nqp::push(
            ($reified := nqp::create(IterationBuffer)),
            nqp::assign(nqp::p6scalarfromdesc(nqp::null),iterable)
          ),
          $reified
        )
      ),
      nqp::if(
        nqp::eqaddr(iterable.WHAT,List),
        nqp::if(
          iterable.is-lazy,
          Array.from-iterator(iterable.iterator),
          nqp::stmts(     # immutable List
            (my int $elems = iterable.elems),  # reifies
            (my $params  := nqp::getattr(iterable,List,'$!reified')),
            (my int $i    = -1),
            ($reified := nqp::setelems(nqp::create(IterationBuffer),$elems)),
            nqp::while(
              nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
              nqp::bindpos($reified,$i,nqp::assign(
                nqp::p6scalarfromdesc(nqp::null),nqp::atpos($params,$i))
              )
            ),
            nqp::p6bindattrinvres(nqp::create(Array),List,'$!reified',$reified)
          ),
        ),
        Array.from-iterator(iterable.iterator)
      )
    )
}
multi circumfix:<[ ]>(Mu \x) {   # really only for [$foo]
    nqp::p6bindattrinvres(
      nqp::create(Array),List,'$!reified',
      nqp::stmts(
        nqp::push(
          (my $reified := nqp::create(IterationBuffer)),
          nqp::assign(nqp::p6scalarfromdesc(nqp::null),x)
        ),
        $reified
      )
    )
}

proto sub pop(@) {*}
multi sub pop(@a) { @a.pop }

proto sub shift(@) {*}
multi sub shift(@a) { @a.shift }

sub push   (\a, |elems) { a.push:    |elems }
sub append (\a, |elems) { a.append:  |elems }
sub unshift(\a, |elems) { a.unshift: |elems }
sub prepend(\a, |elems) { a.prepend: |elems }

sub splice(@arr, |c)         { @arr.splice(|c) }

# vim: ft=perl6 expandtab sw=4
