# This class contains methods for generating callables to be used
# in metaoperators.  There are two reasons for having this in a
# separate class:
#
# 1. It needs to know about all possible builtin operators.  If it
#    would be part of Rakudo::Internals, it would be too early in
#    building the settings.  Augmenting Rakudo::Internals at the
#    the end of building the settings would also have been an option,
#    but that would probably slow down settings building significantly.
#    And since the class name is really not that important, this seemed
#    like a good solution.
# 2. Nice to have a separate file for similar stuff.  Rakudo::Internals
#    has become a hodgepodge of stuff of late.

class Rakudo::Metaops {

    my $mappers := nqp::hash(
      nqp::tostr_I(&infix:<+>.WHERE),      # optimized version for &[+]
      -> \list {
          nqp::if(
            nqp::iseq_i(nqp::elems(list),2),
            (nqp::atpos(list,0) + nqp::atpos(list,1)),
            nqp::if(
              nqp::elems(list),
              nqp::stmts(
                (my $result := nqp::shift(list)),
                nqp::while(
                  nqp::elems(list),
                  ($result := $result + nqp::shift(list))
                ),
                $result
              ),
              0
            )
          )
      },
      nqp::tostr_I(&infix:<~>.WHERE),      # optimized version for &[~]
      -> \list {
          nqp::if(
            nqp::iseq_i(nqp::elems(list),2),
            (nqp::atpos(list,0) ~ nqp::atpos(list,1)),
            nqp::if(
              nqp::elems(list),
              nqp::stmts(                  # could possibly be done smarter
                (my $result := nqp::shift(list)),
                nqp::while(
                  nqp::elems(list),
                  ($result := $result ~ nqp::shift(list))
                ),
                $result
              ),
              ''
            )
          )
      },
      nqp::tostr_I(&infix:<< => >>.WHERE), # optimized version for &[=>]
      -> \list {
          nqp::if(
            nqp::iseq_i(nqp::elems(list),2),
            Pair.new(nqp::atpos(list,0),nqp::atpos(list,1)),
            nqp::if(
              nqp::isgt_i(nqp::elems(list),2),
              nqp::stmts(
                (my $result := nqp::pop(list)),
                nqp::while(
                  nqp::elems(list),
                  ($result := Pair.new(nqp::pop(list),$result))
                ),
                $result
              ),
              (die "Too few positionals passed; expected 2 arguments but got {nqp::elems(list)}")
            )
          )
      },
      nqp::tostr_I(&infix:<,>.WHERE),      # optimized version for &[,]
      -> \list {
          nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',list)
      }
    );

    method MapperForOp(&op) is raw {
        nqp::if(
          nqp::existskey($mappers,(my str $where = nqp::tostr_I(&op.WHERE))),
          nqp::atkey($mappers,$where),
          nqp::if(
            nqp::iseq_i(nqp::chars(my str $assoc = &op.prec("assoc")),0)
              || nqp::iseq_s($assoc,'left'),
            -> \list {                   # generic left-assoc op
                nqp::if(
                  nqp::iseq_i(nqp::elems(list),2),
                  op(nqp::atpos(list,0),nqp::atpos(list,1)),
                  nqp::if(
                    nqp::elems(list),
                    nqp::stmts(
                      (my $result := nqp::shift(list)),
                      nqp::while(
                        nqp::elems(list),
                        ($result := op($result,nqp::shift(list)))
                      ),
                      $result
                    ),
                    op()
                  )
                )
            },
            nqp::if(
              nqp::iseq_s($assoc,"chain"),
              -> \list {                 # generic chain-assoc op
                  nqp::if(
                    nqp::iseq_i(nqp::elems(list),2),
                    op(nqp::atpos(list,0),nqp::atpos(list,1)),
                    nqp::if(
                      nqp::elems(list),
                      nqp::stmts(
                        (my $state = True),
                        (my $current := nqp::shift(list)),
                        nqp::while(
                          nqp::elems(list)
                            && ($state := op(
                              $current,
                              (my $next := nqp::shift(list))
                            )),
                          ($current := $next)
                        ),
                        $state
                      ),
                      op()
                    )
                  )
              },
              nqp::if(
                nqp::iseq_s($assoc,'right'),
                -> \list {                 # generic right-assoc op
                    nqp::if(
                      nqp::iseq_i(nqp::elems(list),2),
                      op(nqp::atpos(list,0),nqp::atpos(list,1)),
                      nqp::if(
                        nqp::elems(list),
                        nqp::stmts(
                          (my $result := nqp::pop(list)),
                          nqp::while(
                            nqp::elems(list),
                            ($result := op(nqp::pop(list),$result))
                          ),
                          $result
                        ),
                        op()
                      )
                    )
                },
                nqp::if(
                  nqp::iseq_s($assoc,'non'),
                  -> \list {               # generic non-assoc op
                      nqp::if(
                        nqp::iseq_i(nqp::elems(list),2),
                        op(nqp::atpos(list,0),nqp::atpos(list,1)),
                        (die "Incorrect number of elements for non-associative operator: expected 2, got {nqp::elems(list)}")
                      )
                  },
                  nqp::if(
                    nqp::iseq_s($assoc,"list"),
                    -> \list {               # generic list/listinfix op
                        op(
                          nqp::p6bindattrinvres(
                            nqp::create(List),List,'$!reified',list)
                        )
                    },
                    (die "Don't know how to process '$assoc' associativity")
                  )
                )
              )
            )
          )
        )
    }
}

# vim: expandtab shiftwidth=4
