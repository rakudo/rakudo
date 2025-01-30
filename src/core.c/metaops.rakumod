
sub METAOP_ASSIGN(\op) is implementation-detail {
    Rakudo::Internals.METAOP_ASSIGN(op)
}

sub METAOP_TEST_ASSIGN:<//>(\lhs, $rhs) is raw is implementation-detail {
    lhs // (lhs = $rhs())
}
sub METAOP_TEST_ASSIGN:<||>(\lhs, $rhs) is raw is implementation-detail {
    lhs || (lhs = $rhs())
}
sub METAOP_TEST_ASSIGN:<&&>(\lhs, $rhs) is raw is implementation-detail {
    lhs && (lhs = $rhs())
}
sub METAOP_TEST_ASSIGN:<or>(\lhs, $rhs) is raw is implementation-detail {
    lhs or (lhs = $rhs())
}
sub METAOP_TEST_ASSIGN:<and>(\lhs, $rhs) is raw is implementation-detail {
    lhs and (lhs = $rhs())
}
sub METAOP_TEST_ASSIGN:<andthen>(\lhs, $rhs) is raw is implementation-detail {
    lhs andthen (lhs = $rhs())
}
sub METAOP_TEST_ASSIGN:<notandthen>(\lhs, $rhs) is raw is implementation-detail {
    lhs notandthen (lhs = $rhs())
}
sub METAOP_TEST_ASSIGN:<orelse>(\lhs, $rhs) is raw is implementation-detail {
    lhs orelse (lhs = $rhs())
}

sub METAOP_TEST_ASSIGN_VALUE:<//>(\lhs, $rhs) is raw is implementation-detail {
    lhs // (lhs = $rhs)
}
sub METAOP_TEST_ASSIGN_VALUE:<||>(\lhs, $rhs) is raw is implementation-detail {
    lhs || (lhs = $rhs)
}
sub METAOP_TEST_ASSIGN_VALUE:<&&>(\lhs, $rhs) is raw is implementation-detail {
    lhs && (lhs = $rhs)
}
sub METAOP_TEST_ASSIGN_VALUE:<or>(\lhs, $rhs) is raw is implementation-detail {
    lhs or (lhs = $rhs)
}
sub METAOP_TEST_ASSIGN_VALUE:<and>(\lhs, $rhs) is raw is implementation-detail {
    lhs and (lhs = $rhs)
}
sub METAOP_TEST_ASSIGN_VALUE:<andthen>(\lhs, $rhs) is raw is implementation-detail {
    lhs andthen (lhs = $rhs)
}
sub METAOP_TEST_ASSIGN_VALUE:<notandthen>(\lhs, $rhs) is raw is implementation-detail {
    lhs notandthen (lhs = $rhs)
}
sub METAOP_TEST_ASSIGN_VALUE:<orelse>(\lhs, $rhs) is raw is implementation-detail {
    lhs orelse (lhs = $rhs)
}

sub METAOP_NEGATE(\op) is implementation-detail {
    -> |c { c.elems > 1 ?? !op.(|c) !! True }
}

sub METAOP_REVERSE(\op) is implementation-detail {
    -> |args { op.(|args.reverse) }
}

sub METAOP_CROSS(\op, &reduce) is implementation-detail {
    nqp::if(op.thunky.starts-with('.'),
    -> +lol {
        my $rop = lol.elems == 2 ?? op !! &reduce(op);
        my $laze = False;
        my @loi is List = eager for lol -> \elem {
            if nqp::iscont(elem) {
                $laze = False;
                (elem,).iterator
            }
            else {
                $laze = True if elem.is-lazy;
                elem.iterator
            }
        }
        my Mu $cache := nqp::list();
        my int $i = 0;
        for ^lol.elems {
            $i = $_;
            my Mu $rpa := nqp::list();
            nqp::bindpos($cache, $i, $rpa);
        }
        my int $n = lol.elems - 1;
        my $j = 0;
        my @j;
        my @v;

        $i = 0;
        gather {
            while $i >= 0 {
                my Mu $sublist := nqp::atpos($cache, $i);
                if $j < nqp::elems($sublist) {
                    my Mu $o := nqp::atpos($sublist, $j);
                    @v[$i] := $o;
                    $j = $j + 1;
                    if $i >= $n { take lol.elems == 2 ?? $rop(|@v) !! $rop(@v); }
                    else { $i = $i + 1; @j.push($j); $j = 0; }
                }
                elsif nqp::not_i(nqp::eqaddr((my \value = @loi[$i].pull-one),IterationEnd)) {
                    nqp::bindpos($sublist, $j, value);
                    redo;
                }
                else {
                    $i = $i - 1;
                    if $i { $j = @j.pop if $i > 0 }  # continue previous dimension where we left off
                    else  {
                        $j = 0;
                        my Mu $sublist := nqp::atpos($cache,$i);
                        nqp::pop($sublist);          # don't cache 1st dimension (could be infinite)
                    }
                }
            }
        }.lazy-if($laze);
    },
    -> +lol {
        Seq.new(Rakudo::Iterator.CrossIterablesOp(lol,op))
    }
    )
}

sub METAOP_ZIP(\op, &reduce) is implementation-detail {
   nqp::if(op.thunky.starts-with('.'),
   -> +lol {
        my $arity = lol.elems;
        my $rop = $arity == 2 ?? op !! &reduce(op);
        my $laze = True;
        my @loi is List = eager for lol -> \elem {
            if nqp::iscont(elem) {
                $laze = False;
                Rakudo::Iterator.OneValue(elem)
            }
            else {
                $laze = False unless elem.is-lazy;
                Rakudo::Iterator.Whatever(elem.iterator)
            }
        }
        gather {
            loop {
                my \z = @loi.map: {
                    my \value = .pull-one;
                    last if nqp::eqaddr(value,IterationEnd);
                    value
                };
                my $z = List.from-iterator(z.iterator);
                $z.eager;
                last if $z.elems < $arity;
                take-rw $arity == 2 ?? $rop(|$z) !! $rop(@$z);
            }
        }.lazy-if($laze)
    },
    -> +lol {
        Seq.new(Rakudo::Iterator.ZipIterablesOp(lol,op))
    }
    )
}

proto sub METAOP_REDUCE_LEFT(|) is implementation-detail {*}
multi sub METAOP_REDUCE_LEFT(\op, \triangle) {

    my class TriangleLeftN does Iterator {
        has     $!operator;
        has int $!count;
        has     $!iterator;
        has     $!result;
        has     $!args;

        method SET-SELF($operator, \values) {
            my $iterator := values.iterator;
            my $result   := $iterator.pull-one;

            if nqp::eqaddr($result,IterationEnd) {
                Rakudo::Iterator.Empty
            }
            else {
                $!operator := $operator;
                $!count     = $operator.count;
                $!iterator := $iterator;
                $!result   := $result;
                nqp::push(($!args := nqp::create(IterationBuffer)),$result);
                self
            }
        }

        method pull-one() {
            my $result   := $!result;  # save return value
            my $args     := $!args;    # lexicals are faster
            my int $count = $!count;

            # Make sure we have enough args
            nqp::until(
              nqp::iseq_i(nqp::elems($args),$count)
                || nqp::eqaddr((my $value := $!iterator.pull-one),IterationEnd),
              nqp::push($args,$value)
            );

            # Exactly enough args
            if nqp::isge_i(nqp::elems($args),$count) {
                $!result := $!operator(|$args.List);
                nqp::setelems($args,0);
                nqp::push($args,$!result);
            }

            # Not enough, done next iteration
            else {
                $!result := IterationEnd;
            }

            $result
        }

        method is-lazy() { $!iterator.is-lazy }
    }

    my class TriangleLeft2 does Iterator {
        has $!operator;
        has $!iterator;
        has $!result;

        method SET-SELF($operator, \values) {
            my $iterator := values.iterator;
            my $result   := $iterator.pull-one;

            if nqp::eqaddr($result,IterationEnd) {
                Rakudo::Iterator.Empty
            }
            else {
                $!operator := $operator;
                $!iterator := $iterator;
                $!result   := $result;
                self
            }
        }

        method pull-one() {
            my $result := $!result;  # save return value
            my $value  := $!iterator.pull-one;

            $!result := nqp::eqaddr($value,IterationEnd)
              ?? IterationEnd
              !! $!operator($!result, $value);

            $result
        }

        method is-lazy() { $!iterator.is-lazy }
    }

    -> +values {
        Seq.new: nqp::create(
          2 < op.count < Inf ?? TriangleLeftN !! TriangleLeft2
        ).SET-SELF(op, values)
    }
}

multi sub METAOP_REDUCE_LEFT(\op) {

    2 < op.count < Inf
      ?? sub (+values) is raw {  # cannot be a block because of "is raw"
             my int $count = op.count;
             my $iterator := values.iterator;
             my $args := nqp::create(IterationBuffer);
             my $value;

             # Initial fill of the args
             nqp::until(
               nqp::iseq_i(nqp::elems($args),$count)
                || nqp::eqaddr(($value := $iterator.pull-one),IterationEnd),
               nqp::push($args,$value)
             );
             my $result := op.(|$args.List);

             # Could be we need to do more
             if nqp::iseq_i(nqp::elems($args),$count) {
                 nqp::setelems($args,0);
                 nqp::push($args,$result);

                 nqp::until(
                   nqp::eqaddr(($value := $iterator.pull-one),IterationEnd),
                   nqp::stmts(
                     nqp::push($args,$value),
                     nqp::if(
                       nqp::iseq_i(nqp::elems($args),$count),
                       nqp::stmts(
                         ($result := op.(|$args.List)),
                         nqp::setelems($args,0),
                         nqp::push($args,$result)
                       )
                     )
                   )
                 );
             }

             $result
         }
      !! sub (+values) is raw {  # cannot be a block because of "is raw"
             my $iterator := values.iterator;
             nqp::if(
               nqp::eqaddr((my $result := $iterator.pull-one),IterationEnd),
               op.(),                         # identity
               nqp::if(
                 nqp::eqaddr((my $value := $iterator.pull-one),IterationEnd),
                 nqp::if(
                   nqp::isle_i(op.arity,1),
                   op.($result),              # can call with 1 param
                   $result                    # what we got
                 ),
                 nqp::stmts(
                   ($result := op.($result,$value)),
                   nqp::until(
                     nqp::eqaddr(($value := $iterator.pull-one),IterationEnd),
                     ($result := op.($result,$value))
                   ),
                   $result                    # final result
                 )
               )
             )
         }
}

proto sub METAOP_REDUCE_RIGHT(|) is implementation-detail {*}
multi sub METAOP_REDUCE_RIGHT(\op, \triangle) {

    class RightTriangleN does Iterator {
        has $!op;
        has $!reified;
        has $!result;
        has int $!count;
        has int $!i;

        method SET-SELF($op, $count, $i, \list) {
            $!op      := $op;
            $!count    = $count;
            $!i        = $i;
            $!reified := nqp::getattr(list,List,'$!reified');
            $!result  := nqp::null;
            self
        }

        method pull-one() is raw {
            nqp::if(
              nqp::isnull($!result),
              ($!result := nqp::atpos($!reified,--$!i)),
              nqp::stmts(
                (my $args := nqp::list($!result)),
                nqp::until(
                  nqp::iseq_i(nqp::elems($args),$!count)
                    || nqp::islt_i(--$!i,0),
                  nqp::unshift($args,nqp::atpos($!reified,$!i))
                ),
                nqp::if(
                  nqp::isgt_i(nqp::elems($args),1),
                  ($!result := $!op.(|nqp::hllize($args))),
                  IterationEnd
                )
              )
            )
        }
    }

    my class RightTriangle2 does Iterator {
        has $!op;
        has $!reified;
        has $!result;
        has int $!i;

        method SET-SELF($op, $i, \list) {
            $!op      := $op;
            $!i        = $i;
            $!reified := nqp::getattr(list,List,'$!reified');
            $!result  := nqp::null;
            self
        }

        method pull-one() is raw {
            nqp::isnull($!result)
              ?? ($!result := nqp::atpos($!reified,--$!i))
              !! nqp::isge_i(--$!i,0)
                ?? ($!result := $!op.(nqp::atpos($!reified,$!i),$!result))
                !! IterationEnd
        }

        method push-all($target --> IterationEnd) {
            my $op      := $!op;
            my $reified := $!reified;
            my int $i    = $!i;

            $target.push(my $result := nqp::atpos($reified,--$i));
            nqp::while(
              nqp::isge_i(--$i,0),
              $target.push($result := $op.(nqp::atpos($reified,$i),$result))
            );
        }
    }

    op.count < Inf && nqp::isgt_i((my int $count = op.count),2)
      ?? -> +values {
             my $list := nqp::istype(values,List) ?? values !! values.List;
             Seq.new:
               nqp::isge_i((my int $i = $list.elems),$count)   # reifies
                 ?? nqp::create(RightTriangleN).SET-SELF(op,$count,$i,$list)
                 !! Rakudo::Iterator.OneValue($i ?? op.(|$list) !! op.())
         }
      !! -> +values {
             my $list := nqp::istype(values,List) ?? values !! values.List;
             Seq.new:
               nqp::isgt_i((my int $i = $list.elems),1)   # reifies
                 ?? nqp::create(RightTriangle2).SET-SELF(op, $i, $list)
                 !! Rakudo::Iterator.OneValue($i ?? op.($list.head) !! op.())
         }
}
multi sub METAOP_REDUCE_RIGHT(\op) {
    nqp::if(
      op.count < Inf && nqp::isgt_i((my int $count = op.count),2),
      -> +values {
          # get a reified list
          (my $list := nqp::istype(values,List) ?? values !! values.List).elems;
          my $reified := nqp::getattr($list,List,'$!reified');

          nqp::if(
            nqp::isgt_i((my int $i = nqp::elems($reified)),1),
            nqp::stmts(
              (my $args := nqp::list(my $result := nqp::atpos($reified,--$i))),
              nqp::until(
                nqp::islt_i(--$i,0),
                nqp::stmts(
                  nqp::unshift($args,nqp::atpos($reified,$i)),
                  nqp::if(
                    nqp::iseq_i(nqp::elems($args),$count),
                    nqp::stmts(
                      ($result := op.(|nqp::hllize($args))),
                      nqp::bindpos(nqp::setelems($args,1),0,$result)
                    )
                  )
                )
              ),
              nqp::if(
                nqp::isgt_i(nqp::elems($args),1),
                op.(|nqp::hllize($args)),    # something left to process
                $result
              )
            ),
            nqp::if(
              $i,
              op.(|$reified),
              op.()
            )
        )
      },
      -> +values {
          # get a reified list
          (my $list := nqp::istype(values,List) ?? values !! values.List).elems;
          my $reified := nqp::getattr($list,List,'$!reified');

          nqp::if(
            nqp::isgt_i((my int $i = nqp::elems($reified)),1),
            nqp::stmts(
              (my $result := nqp::atpos($reified,--$i)),
              nqp::while(
                nqp::isge_i(--$i,0),
                ($result := op.(nqp::atpos($reified,$i), $result))
              ),
              $result
            ),
            nqp::if(
              $i,
              op.(nqp::atpos($reified,0)),
              op.()
            )
          )
      }
    )
}

proto sub METAOP_REDUCE_LIST(|) is implementation-detail {*}
multi sub METAOP_REDUCE_LIST(\op, \triangle) {

    my class TriangleList does Iterator {
        has $!operator;
        has $!iterator;
        has $!result;
        has $!args;

        method SET-SELF($operator, \values) {
            my $iterator := values.iterator;
            my $value    := $iterator.pull-one;
            if nqp::eqaddr($value,IterationEnd) {
                Rakudo::Iterator.Empty
            }
            else {
                $!operator := $operator;
                $!iterator := $iterator;
                nqp::push(
                  ($!args   := nqp::create(IterationBuffer)),
                  ($!result := $operator($value))
                );
                self
            }
        }

        method pull-one() {
            my $result := $!result;

            my $value := $!iterator.pull-one;
            $!result := nqp::if(
              nqp::eqaddr($value,IterationEnd),
              IterationEnd,
              nqp::stmts(
                nqp::push($!args,$value),
                nqp::if(
                  nqp::eqaddr(
                    ($value := $!operator(|$!args.List)),
                    Empty
                  ),
                  IterationEnd,
                  $value
                )
              )
            );

            $result
        }

        method is-lazy() { $!iterator.is-lazy }
    }

    -> +values {
        Seq.new: nqp::create(TriangleList).SET-SELF(op, values)
    }
}
multi sub METAOP_REDUCE_LIST(&op) {
    -> +values { op(|values) }
}

proto sub METAOP_REDUCE_LISTINFIX(|) is implementation-detail {*}
multi sub METAOP_REDUCE_LISTINFIX(\op, \triangle) {
    sub (|values) {
        my \p = values[0];
        return () unless p.elems;

        my int $i;
        GATHER({
            my @list;
            while $i < p.elems {
                @list.push(p[$i++]);
                take op.(|@list.map({nqp::decont($_)}));
            }
        }).lazy-if(p.is-lazy);
    }
}
multi sub METAOP_REDUCE_LISTINFIX(\op) {
    -> +values {

        # Create decontainerized list
        my $iterator := values.iterator;
        my $buffer   := nqp::create(IterationBuffer);
        nqp::until(
          nqp::eqaddr((my $pulled := $iterator.pull-one),IterationEnd),
          nqp::push($buffer,nqp::decont($pulled))
        );

        op.(|$buffer.List);
    }
}

proto sub METAOP_REDUCE_CHAIN(|) is implementation-detail {*}
multi sub METAOP_REDUCE_CHAIN(\op, \triangle) {
    sub (+values) {
        my $state = True;
        my \iter = values.iterator;
        my Mu $current = iter.pull-one;
        gather {
            take $state;
            while $state && nqp::not_i(nqp::eqaddr((my $next := iter.pull-one),IterationEnd)) {
                $state = op.($current, $next);
                take $state;
                $current := $next;
            }
            unless $state {
                take False until nqp::eqaddr(iter.pull-one,IterationEnd);
            }
        }.lazy-if(values.is-lazy);
    }
}
multi sub METAOP_REDUCE_CHAIN(\op) {
    sub (+values) {
        nqp::if(
          nqp::eqaddr(
            (my $current := (my $iter := values.iterator).pull-one),
            IterationEnd
          ),
          True,
          nqp::stmts(
            nqp::while(
              nqp::not_i(nqp::eqaddr((my $next := $iter.pull-one),IterationEnd))
                && op.($current,$next),
              $current := $next
            ),
            nqp::hllbool(nqp::eqaddr($next,IterationEnd))
          )
        )
    }
}

sub METAOP_REDUCE_XOR(\op, $triangle?) is implementation-detail {
    NYI('xor reduce').throw;
}

sub METAOP_HYPER(\op, *%opt) is implementation-detail {
    -> Mu \a, Mu \b { HYPER(op, a, b, |%opt) }
}

proto sub METAOP_HYPER_POSTFIX(|) is implementation-detail {*}
multi sub METAOP_HYPER_POSTFIX(&op) {
    nqp::can(&op,"nodal") ?? *.nodemap(&op) !! *.deepmap(&op)
}

# no indirection for subscripts and such
proto sub METAOP_HYPER_POSTFIX_ARGS(|) is implementation-detail {*}
multi sub METAOP_HYPER_POSTFIX_ARGS(\obj, &op) {
    nqp::can(&op,"nodal") ?? obj.nodemap(&op) !! obj.deepmap(&op)
}
multi sub METAOP_HYPER_POSTFIX_ARGS(\obj, @args, &op) {
    nqp::can(&op,"nodal")
      ?? obj.nodemap(-> \o { op(o, @args) })
      !! obj.deepmap(-> \o { op(o, @args) })
}
multi sub METAOP_HYPER_POSTFIX_ARGS(\obj, \args, &op) {
    nqp::can(&op,"nodal")
      ?? obj.nodemap( -> \o { op(o,|args) })
      !! obj.deepmap( -> \o { op(o,|args) })
}

sub METAOP_HYPER_PREFIX(&op) is implementation-detail {
    nqp::can(&op,"nodal") ?? *.nodemap(&op) !! *.deepmap(&op)
}

sub METAOP_HYPER_CALL(\list, |args) is implementation-detail {
    list.deepmap(-> &code { code(|args) })
}

sub HYPER(\operator, :$dwim-left, :$dwim-right, |c) is implementation-detail {
    Hyper.new(operator, :$dwim-left, :$dwim-right).infix(|c)
}

# vim: expandtab shiftwidth=4
