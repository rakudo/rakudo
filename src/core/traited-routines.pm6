
# In core, we can't use traits that mixin roles into routines before
# Callable and all the types that use that role have already been composed.
# This file is to keep such routines, if otherwise they'd go to some earlier
# place in the setting. Issue: https://github.com/rakudo/rakudo/issues/1566

{ # "If the method won't come to the Muhammad, Muhammad will… use a hack."
    # We poke into types, looking for routines we marked with traits earlier
    # and recompose them so that `Callable` role becomes visible correctly.
    my @recompose = Mu => <splice>, Any => <EXISTS-KEY>;
    for @recompose {
        my \T := .key;
        T.^lookup($_).^compose for .values;
    }
}

# From Mu.pm6
proto sub defined(Mu) is pure {*}
multi sub defined(Mu \x) { x.defined }

proto sub infix:<=:=>(Mu $?, Mu $?) is pure {*}
multi sub infix:<=:=>($?)      { Bool::True }
multi sub infix:<=:=>(Mu \a, Mu \b) {
    nqp::p6bool(nqp::eqaddr(a, b));
}
proto sub infix:<eqv>(Any $?, Any $?) is pure {*}
multi sub infix:<eqv>($?)            { Bool::True }
# Last ditch snapshot semantics.  We shouldn't come here too often, so
# please do not change this to be faster but wronger.  (Instead, add
# specialized multis for datatypes that can be tested piecemeal.)
multi sub infix:<eqv>(Any:U \a, Any:U \b) {
    nqp::p6bool(nqp::eqaddr(nqp::decont(a),nqp::decont(b)))
}
multi sub infix:<eqv>(Any:D \a, Any:U \b) { False }
multi sub infix:<eqv>(Any:U \a, Any:D \b) { False }
multi sub infix:<eqv>(Any:D \a, Any:D \b) {
    nqp::p6bool(
      nqp::eqaddr(a,b)
        || (nqp::eqaddr(a.WHAT,b.WHAT) && nqp::iseq_s(a.perl,b.perl))
    )
}

multi sub infix:<eqv>(Iterable:D \a, Iterable:D \b) {
    nqp::p6bool(
      nqp::unless(
        nqp::eqaddr(nqp::decont(a),nqp::decont(b)),
        nqp::if(                                 # not same object
          nqp::eqaddr(a.WHAT,b.WHAT),
          nqp::if(                               # same type
            a.is-lazy,
            nqp::if(                             # a lazy
              b.is-lazy,
              die(X::Cannot::Lazy.new: :action<eqv>) # a && b lazy
            ),
            nqp::if(                             # a NOT lazy
              b.is-lazy,
              0,                                 # b lazy
              nqp::if(                           # a && b NOT lazy
                nqp::iseq_i((my int $elems = a.elems),b.elems),
                nqp::stmts(                      # same # elems
                  (my int $i = -1),
                  nqp::while(
                    nqp::islt_i(($i = nqp::add_i($i,1)),$elems) # not exhausted
                      && a.AT-POS($i) eqv b.AT-POS($i),         # still same
                    nqp::null
                  ),
                  nqp::iseq_i($i,$elems)         # exhausted = success!
                )
              )
            )
          )
        )
      )
    )
}


# From Stringy.pm6


# From Any.pm6



# From Any-iterable-methods.pm6

proto sub min(|) is pure {*}
multi sub min(+args, :&by!) { args.min(&by) }
multi sub min(+args)        { args.min      }
