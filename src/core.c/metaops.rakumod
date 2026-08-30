
sub METAOP_ASSIGN(\op) is implementation-detail {
    Rakudo::Internals.METAOP_ASSIGN(op)
}

sub METAOP_TEST_ASSIGN:<//>(Mu \lhs, Mu $rhs) is raw is implementation-detail {
    lhs // (lhs = $rhs())
}
sub METAOP_TEST_ASSIGN:<||>(Mu \lhs, Mu $rhs) is raw is implementation-detail {
    lhs || (lhs = $rhs())
}
sub METAOP_TEST_ASSIGN:<&&>(Mu \lhs, Mu $rhs) is raw is implementation-detail {
    lhs && (lhs = $rhs())
}
sub METAOP_TEST_ASSIGN:<or>(Mu \lhs, Mu $rhs) is raw is implementation-detail {
    lhs or (lhs = $rhs())
}
sub METAOP_TEST_ASSIGN:<and>(Mu \lhs, Mu $rhs) is raw is implementation-detail {
    lhs and (lhs = $rhs())
}
sub METAOP_TEST_ASSIGN:<andthen>(Mu \lhs, Mu $rhs) is raw is implementation-detail {
    lhs andthen (lhs = $rhs())
}
sub METAOP_TEST_ASSIGN:<notandthen>(Mu \lhs, Mu $rhs) is raw is implementation-detail {
    lhs notandthen (lhs = $rhs())
}
sub METAOP_TEST_ASSIGN:<orelse>(Mu \lhs, Mu $rhs) is raw is implementation-detail {
    lhs orelse (lhs = $rhs())
}

sub METAOP_TEST_ASSIGN_VALUE:<//>(Mu \lhs, Mu $rhs) is raw is implementation-detail {
    lhs // (lhs = $rhs)
}
sub METAOP_TEST_ASSIGN_VALUE:<||>(Mu \lhs, Mu $rhs) is raw is implementation-detail {
    lhs || (lhs = $rhs)
}
sub METAOP_TEST_ASSIGN_VALUE:<&&>(Mu \lhs, Mu $rhs) is raw is implementation-detail {
    lhs && (lhs = $rhs)
}
sub METAOP_TEST_ASSIGN_VALUE:<or>(Mu \lhs, Mu $rhs) is raw is implementation-detail {
    lhs or (lhs = $rhs)
}
sub METAOP_TEST_ASSIGN_VALUE:<and>(Mu \lhs, Mu $rhs) is raw is implementation-detail {
    lhs and (lhs = $rhs)
}
sub METAOP_TEST_ASSIGN_VALUE:<andthen>(Mu \lhs, Mu $rhs) is raw is implementation-detail {
    lhs andthen (lhs = $rhs)
}
sub METAOP_TEST_ASSIGN_VALUE:<notandthen>(Mu \lhs, Mu $rhs) is raw is implementation-detail {
    lhs notandthen (lhs = $rhs)
}
sub METAOP_TEST_ASSIGN_VALUE:<orelse>(Mu \lhs, Mu $rhs) is raw is implementation-detail {
    lhs orelse (lhs = $rhs)
}

proto sub METAOP_NEGATE(|) is implementation-detail is revision-gated("6.c") {*}
multi sub METAOP_NEGATE(\op) is implementation-detail is revision-gated("6.c") {
    -> |c { c.elems > 1 ?? not op.(|c) !! True }
}
multi sub METAOP_NEGATE(\op) is implementation-detail is revision-gated("6.e") {
    -> |c { c.elems > 1 ?? op.(op.(|c), False) !! True }
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

# A reduction with a short-circuit operator settles on a value that no
# further operand can change: a true value for ||/or, a false value for
# &&/and, a defined value for //.
my sub SETTLED-ON-TRUE(Mu \result)    is implementation-detail { result.Bool    }
my sub SETTLED-ON-FALSE(Mu \result)   is implementation-detail { !result.Bool   }
my sub SETTLED-ON-DEFINED(Mu \result) is implementation-detail { result.defined }

# Returns the test for having settled, or the Callable type object for
# operators that never settle.  The setting's own operators are matched by
# address, so an operator declared elsewhere under one of these names folds
# like any other.
my sub REDUCE-SETTLED(\op) is implementation-detail {
    my \candidate := nqp::decont(op);
    nqp::eqaddr(candidate,nqp::decont(&infix:<||>))
      || nqp::eqaddr(candidate,nqp::decont(&infix:<or>))
      ?? &SETTLED-ON-TRUE
      !! nqp::eqaddr(candidate,nqp::decont(&infix:<&&>))
           || nqp::eqaddr(candidate,nqp::decont(&infix:<and>))
        ?? &SETTLED-ON-FALSE
        !! nqp::eqaddr(candidate,nqp::decont(&infix:<//>))
          ?? &SETTLED-ON-DEFINED
          !! Callable
}

# Reports whether a value is a Range that goes on producing values.  An
# itemized argument counts as one value rather than as a sequence, so it is
# not one of these.  Reporting itself infinite does not make a Range produce
# anything, so the first value settles that.  The answers taken from one of
# these assume the values a Range produces are the ones its endpoints
# describe, so a Range producing values of its own choosing is not one.
my sub REDUCE-ENDLESS-RANGE(Mu \value) is implementation-detail {
    nqp::not_i(nqp::iscont(value))
      && nqp::eqaddr(nqp::what(value),Range)
      && nqp::isconcrete(value)
      && value.infinite
      && nqp::not_i(nqp::eqaddr(value.head,Nil))
}

# Reports whether a value is a Range producing a run of Ints
# its endpoints describe.
my sub REDUCE-INT-RANGE(Mu \value) is implementation-detail {
    nqp::not_i(nqp::iscont(value))
      && nqp::eqaddr(nqp::what(value),Range)
      && nqp::isconcrete(value)
      && nqp::getattr_i(value,Range,'$!is-int')
}

# The settling reduction and the one for + each keep their own copy of this
# fold: the first adds a test to the loop, and the second names its operator
# rather than reaching it through a reference.
my sub REDUCE-LEFT-FOLD(\op) is implementation-detail {
    sub (+values) is raw {  # cannot be a block because of "is raw"
        my $iterator := values.iterator;
        nqp::if(
          nqp::eqaddr((my $result := $iterator.pull-one),IterationEnd),
          op.(),                         # identity
          nqp::if(
            nqp::eqaddr((my $value := $iterator.pull-one),IterationEnd),
            op.($result),                # call with 1 param
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

# Stops folding once the result has settled.  The test runs only after a
# further value has been pulled, which is exactly when the operator itself
# would have inspected the result, so a result the operator would have
# handed back untouched stays untouched.
my sub REDUCE-LEFT-SETTLING(\op, &settled) is implementation-detail {
    sub (+values) is raw {  # cannot be a block because of "is raw"
        my $iterator := values.iterator;
        nqp::if(
          nqp::eqaddr((my $result := $iterator.pull-one),IterationEnd),
          op.(),                         # identity
          nqp::if(
            nqp::eqaddr((my $value := $iterator.pull-one),IterationEnd),
            op.($result),                # call with 1 param
            nqp::stmts(
              ($result := op.($result,$value)),
              nqp::until(
                nqp::eqaddr(($value := $iterator.pull-one),IterationEnd)
                  || settled($result),
                ($result := op.($result,$value))
              ),
              $result                    # final result
            )
          )
        )
    }
}

my sub REDUCE-PLUS-VALUES(+values) is raw is implementation-detail {
    my $iterator := values.iterator;
    nqp::if(
      nqp::eqaddr((my $result := $iterator.pull-one),IterationEnd),
      infix:<+>(),                     # identity
      nqp::if(
        nqp::eqaddr((my $value := $iterator.pull-one),IterationEnd),
        infix:<+>($result),            # call with 1 param
        nqp::stmts(
          ($result := infix:<+>($result,$value)),
          nqp::until(
            nqp::eqaddr(($value := $iterator.pull-one),IterationEnd),
            ($result := infix:<+>($result,$value))
          ),
          $result                      # final result
        )
      )
    )
}

# Takes the list rather than its buffer, since a raw VM array arrives as a
# List across a Raku signature.  Callers hand over a concrete List whose
# buffer is a flat VM array holding every value.
my sub REDUCE-PLUS-REIFIED(\values) is raw is implementation-detail {
    # Summing can run code that reaches the list it is summing.  Which slots
    # are summed and what sits in each is settled here, so shortening the list
    # cannot read past its end.  A value assigned into a container a slot
    # already holds is still read when that slot is summed.
    my \reified := nqp::clone(nqp::getattr(values,List,'$!reified'));
    my int $elems = nqp::elems(reified);

    # A hole warns under the name it has in its list, which pulling the
    # values gives it and the buffer alone does not, so any hole sends the
    # whole list to the fold that pulls.
    my int $i = -1;
    nqp::while(
      nqp::islt_i(++$i,$elems)
        && nqp::not_i(nqp::isnull(nqp::atpos(reified,$i))),
      nqp::null
    );
    nqp::if(
      nqp::islt_i($i,$elems),
      REDUCE-PLUS-VALUES(values),
      nqp::if(
        $elems,
        nqp::stmts(
          # Without the deconts the accumulator would start as the slot's
          # container while every later iteration rebinds it to the
          # operator's bare return, and a stored value would arrive as
          # whatever its slot holds, so the operator's callsite would
          # keep several shapes live. Bare values keep it to one shape,
          # and no candidate of the plus operator writes to its
          # arguments.
          (my $result := nqp::decont(nqp::atpos(reified,0))),
          nqp::if(
            nqp::iseq_i($elems,1),
            infix:<+>($result),        # call with 1 param
            nqp::stmts(
              (my int $j = 0),
              nqp::while(
                nqp::islt_i(++$j,$elems),
                ($result := infix:<+>($result,nqp::decont(nqp::atpos(reified,$j))))
              ),
              $result                  # final result
            )
          )
        ),
        infix:<+>()                    # identity
      )
    )
}

# Range.sum decides for a Range producing a run of Ints, where its formula
# arrives at the same Int the fold does, and for one with Real endpoints that
# goes on producing values.  A finite Range with other endpoint types folds,
# since Range.sum answers an integer valued Num endpoint with an Int where
# the fold answers with a Num.  A sole list holding nothing left to produce
# has its values in a buffer already, so they are summed from there.  The
# reductions of min and max keep their own copies of the probe for one, since
# a shared probe would cost a call where most of its tests cost less.
my sub REDUCE-LEFT-SUM(|args) is raw is implementation-detail {
    my \list := nqp::getattr(nqp::decont(args),Capture,'@!list');
    nqp::if(
      nqp::not_i(nqp::isnull(list)) && nqp::iseq_i(nqp::elems(list),1),
      nqp::if(
        nqp::not_i(nqp::iscont(my \first := nqp::atpos(list,0)))
          && nqp::istype(first,List)
          && nqp::isconcrete(first)
          && nqp::eqaddr(nqp::tryfindmethod(first,'list'),nqp::findmethod(List,'list'))
          && nqp::not_i(nqp::isconcrete(nqp::getattr(first,List,'$!todo')))
          && nqp::isconcrete(my \reified := nqp::getattr(first,List,'$!reified'))
          && nqp::iseq_s(nqp::reprname(reified),'VMArray'),
        REDUCE-PLUS-REIFIED(first),
        nqp::if(
          nqp::istype(first,Range)
            && (REDUCE-INT-RANGE(first)
                 || (REDUCE-ENDLESS-RANGE(first)
                      && nqp::istype(first.min,Real)
                      && nqp::istype(first.max,Real))),
          first.sum,
          REDUCE-PLUS-VALUES(|args))),
      REDUCE-PLUS-VALUES(|args))
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
      !! nqp::isconcrete(my \settled := REDUCE-SETTLED(op))
        ?? REDUCE-LEFT-SETTLING(op, settled)
        !! nqp::eqaddr(nqp::decont(op),nqp::decont(&infix:<+>))
          ?? &REDUCE-LEFT-SUM
          !! REDUCE-LEFT-FOLD(op)
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

# Reports whether any value sits in the buffer as a Slip.  Takes the list
# rather than its buffer for the reason REDUCE-PLUS-REIFIED does.
my sub REDUCE-LIST-HOLDS-SLIP(\values) is implementation-detail {
    my \reified := nqp::getattr(values,List,'$!reified');
    my int $elems = nqp::elems(reified);
    my int $i     = -1;
    nqp::while(
      nqp::islt_i(++$i,$elems)
        && nqp::not_i(nqp::istype(nqp::atpos(reified,$i),Slip)),
      nqp::null
    );
    nqp::islt_i($i,$elems)
}

# Spreading a pair over an argument list is what reaches the candidates the
# operator declares for two values.  Any more reach the one candidate taking
# them all whether spread or not, and handing them over whole is the cheaper
# way there and carries more values than an argument list could, so only what
# a Slip rules out is spread.  Values still producing answer neither how many
# there are nor the operator taking them whole, and values that keep no buffer
# of their own leave REDUCE-LIST-HOLDS-SLIP nothing to look through, so both
# of those are spread whatever their length.  Counting the values first is
# what fills that buffer.  min and max each keep their own copy of this
# choice, naming their operator rather than reaching it through a reference.
my sub REDUCE-MIN-VALUES(+values) is implementation-detail {
    nqp::if(
      values.is-lazy
        || nqp::islt_i(values.elems,3)
        || nqp::not_i(nqp::istype(values,List))
        || REDUCE-LIST-HOLDS-SLIP(values),
      infix:<min>(|values),
      infix:<min>(values))
}

my sub REDUCE-MAX-VALUES(+values) is implementation-detail {
    nqp::if(
      values.is-lazy
        || nqp::islt_i(values.elems,3)
        || nqp::not_i(nqp::istype(values,List))
        || REDUCE-LIST-HOLDS-SLIP(values),
      infix:<max>(|values),
      infix:<max>(values))
}

# A sole argument holding nothing left to produce keeps its values in a flat
# buffer, and they go to the operator straight from there.  A list that
# answers list for itself decides what its values are, so only one answering
# it the way a List does has its buffer read.
# A Range of Reals never steps below where it starts, so its start is the
# smallest value it produces.  One climbing through values that are not Reals
# can step below its start, the way a Str rolls over from z to aa, so it is
# folded.  A run of Ints is taken however long it is, and one producing
# nothing keeps the identity the operator gives an empty sequence.
my sub REDUCE-LIST-MIN(|args) is implementation-detail {
    my \list := nqp::getattr(nqp::decont(args),Capture,'@!list');
    nqp::if(
      nqp::not_i(nqp::isnull(list)) && nqp::iseq_i(nqp::elems(list),1),
      nqp::if(
        nqp::not_i(nqp::iscont(my \first := nqp::atpos(list,0)))
          && nqp::istype(first,List)
          && nqp::isconcrete(first)
          && nqp::eqaddr(nqp::tryfindmethod(first,'list'),nqp::findmethod(List,'list'))
          && nqp::not_i(nqp::isconcrete(nqp::getattr(first,List,'$!todo')))
          && nqp::isconcrete(my \reified := nqp::getattr(first,List,'$!reified'))
          && nqp::iseq_s(nqp::reprname(reified),'VMArray'),
        nqp::if(
          nqp::islt_i(nqp::elems(reified),3)
            || REDUCE-LIST-HOLDS-SLIP(first),
          infix:<min>(|first),
          infix:<min>(first)),
        nqp::if(
          nqp::istype(first,Range)
            && (REDUCE-INT-RANGE(first)
                 || (REDUCE-ENDLESS-RANGE(first)
                      && nqp::istype(first.min,Real))),
          nqp::if(
            nqp::eqaddr((my \head := first.head),Nil),
            infix:<min>(),
            head),
          REDUCE-MIN-VALUES(|args))),
      REDUCE-MIN-VALUES(|args))
}

# A Range producing a run of Ints climbs to its top endpoint less its
# exclusion, and the subtraction answers with the plain Int the run itself
# ends on, whatever Int subtype the endpoint is.  One producing nothing
# keeps the operator's empty identity.
# An Int or a Rational climbs by one without bound, so a Range of either
# towards Inf exceeds every value it produces, though one with nothing to
# divide by stays put, which misplaces the answer only at -Inf.  A Num stops
# climbing once a step of one is below its precision, and other values never
# approach the Inf the endpoint names, so Ranges of all of those are folded.
# So is one whose endpoint is a NaN, which no amount of climbing reaches.
my sub REDUCE-LIST-MAX(|args) is implementation-detail {
    my \list := nqp::getattr(nqp::decont(args),Capture,'@!list');
    nqp::if(
      nqp::not_i(nqp::isnull(list)) && nqp::iseq_i(nqp::elems(list),1),
      nqp::if(
        nqp::not_i(nqp::iscont(my \first := nqp::atpos(list,0)))
          && nqp::istype(first,List)
          && nqp::isconcrete(first)
          && nqp::eqaddr(nqp::tryfindmethod(first,'list'),nqp::findmethod(List,'list'))
          && nqp::not_i(nqp::isconcrete(nqp::getattr(first,List,'$!todo')))
          && nqp::isconcrete(my \reified := nqp::getattr(first,List,'$!reified'))
          && nqp::iseq_s(nqp::reprname(reified),'VMArray'),
        nqp::if(
          nqp::islt_i(nqp::elems(reified),3)
            || REDUCE-LIST-HOLDS-SLIP(first),
          infix:<max>(|first),
          infix:<max>(first)),
        nqp::if(
          nqp::istype(first,Range)
            && REDUCE-INT-RANGE(first),
          nqp::if(
            nqp::eqaddr(first.head,Nil),
            infix:<max>(),
            nqp::getattr(first,Range,'$!max')
              - nqp::getattr_i(first,Range,'$!excludes-max')),
          nqp::if(
            nqp::istype(first,Range)
              && REDUCE-ENDLESS-RANGE(first)
              && (nqp::istype((my \low := first.min),Int)
                    || nqp::istype(low,Rational))
              && low != -Inf
              && nqp::istype((my \high := first.max),Real)
              && high == Inf,
            Inf,
            REDUCE-MAX-VALUES(|args)))),
      REDUCE-MAX-VALUES(|args))
}

proto sub METAOP_REDUCE_LIST(|) is implementation-detail {*}
multi sub METAOP_REDUCE_LIST(\op, \triangle) {

    my class TriangleList does Iterator {
        has $!operator;
        has $!iterator;
        has $!args;

        method SET-SELF($operator, \values) {
            $!iterator := values.iterator;
            $!operator := $operator<>;
            $!args     := nqp::create(IterationBuffer);
            self
        }

        method pull-one() {
            my $value := $!iterator.pull-one;
            if nqp::eqaddr($value,IterationEnd) {
                IterationEnd
            }
            else {
                nqp::push($!args,$value);
                nqp::eqaddr(($value := $!operator(|$!args.List)),Empty)
                  ?? IterationEnd
                  !! $value
            }
        }

        method is-lazy() { $!iterator.is-lazy }
    }

    -> +values {
        Seq.new: nqp::create(TriangleList).SET-SELF(op, values)
    }
}

multi sub METAOP_REDUCE_LIST(&op) {
    my \candidate := nqp::decont(&op);
    nqp::eqaddr(candidate,nqp::decont(&infix:<min>))
      ?? &REDUCE-LIST-MIN
      !! nqp::eqaddr(candidate,nqp::decont(&infix:<max>))
        ?? &REDUCE-LIST-MAX
        !! -> +values { op(|values) }
}

proto sub METAOP_REDUCE_LISTINFIX(|) is implementation-detail {*}
multi sub METAOP_REDUCE_LISTINFIX(\op, \triangle) {

    my class TriangleListInfix does Iterator {
        has $!operator;
        has $!iterator;
        has $!buffer;

        method SET-SELF($operator, $values) {
            $!operator := $operator;
            $!iterator := $values.iterator;
            $!buffer   := nqp::create(IterationBuffer);
            self
        }

        method pull-one() {
            nqp::if(
              nqp::eqaddr((my $value := $!iterator.pull-one),IterationEnd),
              IterationEnd,
              nqp::stmts(
                nqp::push($!buffer,nqp::decont($value)),
                $!operator(|$!buffer.List)
              )
            )
        }

        method is-lazy() { $!iterator.is-lazy }
    }

    -> $values {
        Seq.new:
          nqp::create(TriangleListInfix).SET-SELF(op, $values)
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
    -> | {
        my int $elems = nqp::elems(my Mu $args := nqp::p6argvmarray);
        $elems == 2
          ?? HYPER(op, nqp::atpos($args,0), nqp::atpos($args,1), |%opt)
          !! $elems == 1
            ?? op.(nqp::atpos($args,0))
            !! $elems
              ?? die("Got $elems to hyper, expected 0,1,2")
              !! op.()
    }
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
