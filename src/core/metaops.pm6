
sub METAOP_ASSIGN(\op) { Rakudo::Internals.METAOP_ASSIGN(op) }

sub METAOP_TEST_ASSIGN:<//>(\lhs, $rhs) is raw { lhs // (lhs = $rhs()) }
sub METAOP_TEST_ASSIGN:<||>(\lhs, $rhs) is raw { lhs || (lhs = $rhs()) }
sub METAOP_TEST_ASSIGN:<&&>(\lhs, $rhs) is raw { lhs && (lhs = $rhs()) }
sub METAOP_TEST_ASSIGN:<or>(\lhs, $rhs) is raw { lhs or (lhs = $rhs()) }
sub METAOP_TEST_ASSIGN:<and>(       \lhs, $rhs) is raw { lhs and        (lhs = $rhs()) }
sub METAOP_TEST_ASSIGN:<andthen>(   \lhs, $rhs) is raw { lhs andthen    (lhs = $rhs()) }
sub METAOP_TEST_ASSIGN:<notandthen>(\lhs, $rhs) is raw { lhs notandthen (lhs = $rhs()) }
sub METAOP_TEST_ASSIGN:<orelse>(    \lhs, $rhs) is raw { lhs orelse     (lhs = $rhs()) }

sub METAOP_NEGATE(\op) {
    -> |c { c.elems > 1 ?? !op.(|c) !! True }
}

sub METAOP_REVERSE(\op) {
    -> |args { op.(|args.reverse) }
}

sub METAOP_CROSS(\op, &reduce) {
    nqp::if(op.prec('thunky').starts-with('.'),
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

sub METAOP_ZIP(\op, &reduce) {
   nqp::if(op.prec('thunky').starts-with('.'),
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

proto sub METAOP_REDUCE_LEFT(|) {*}
multi sub METAOP_REDUCE_LEFT(\op, \triangle) {
    if op.count > 2 and op.count < Inf {
        my $count = op.count;
        sub (+values) {
            my \source = values.iterator;

            my \first = source.pull-one;
            return () if nqp::eqaddr(first,IterationEnd);

            my @args.push: first;
            GATHER({
                take first;
                until nqp::eqaddr((my \current = source.pull-one),IterationEnd) {
                    @args.push: current;
                    if @args.elems == $count {
                        my \val = op.(|@args);
                        take val;
                        @args = ();
                        @args.push: val;  # use of push allows op to return a Slip
                    }
                }
            }).lazy-if(source.is-lazy);
        }
    }
    else {
        sub (+values) {
            my \source = values.iterator;

            my \first = source.pull-one;
            return () if nqp::eqaddr(first,IterationEnd);

            my $result := first;
            GATHER({
                take first;
                until nqp::eqaddr((my \value = source.pull-one),IterationEnd) {
                    take ($result := op.($result, value));
                }
            }).lazy-if(source.is-lazy);
        }
    }
}

multi sub METAOP_REDUCE_LEFT(\op) {
    if op.count > 2 and op.count < Inf {
        my $count = op.count;
        sub (+values) {
            my \iter = values.iterator;
            my \first = iter.pull-one;
            return op.() if nqp::eqaddr(first,IterationEnd);

            my @args.push: first;
            my $result := first;
            until nqp::eqaddr((my \value = iter.pull-one),IterationEnd) {
                @args.push: value;
                if @args.elems == $count {
                    my \val = op.(|@args);
                    @args = ();
                    @args.push: val;  # use of push allows op to return a Slip
                    $result := val;
                }
            }
            $result;
        }
    }
    else {
        nqp::eqaddr(op,&infix:<+>)
        ?? &sum
        !! sub (+values) {
            nqp::stmts(
              (my $iter := values.iterator),
              nqp::if(
                nqp::eqaddr((my $result := $iter.pull-one),IterationEnd),
                op.(),                         # identity
                nqp::if(
                  nqp::eqaddr((my $value := $iter.pull-one),IterationEnd),
                  nqp::if(
                    nqp::isle_i(op.arity,1),
                    op.($result),              # can call with 1 param
                    $result                    # what we got
                  ),
                  nqp::stmts(
                    ($result := op.($result,$value)),
                    nqp::until(
                      nqp::eqaddr(($value := $iter.pull-one),IterationEnd),
                      ($result := op.($result,$value))
                    ),
                    $result                    # final result
                  )
                )
              )
            )
        }
    }
}

proto sub METAOP_REDUCE_RIGHT(|) {*}
multi sub METAOP_REDUCE_RIGHT(\op, \triangle) {
    nqp::if(
      op.count < Inf && nqp::isgt_i((my int $count = op.count),2),
      sub (+values) {
          Seq.new(nqp::if(
            nqp::isge_i((my int $i = (my $v :=
                nqp::if(nqp::istype(values,List),values,values.List)
              ).elems),                                       # reifies
              $count
            ),   # reifies
            class :: does Iterator {
                has $!op;
                has $!reified;
                has $!result;
                has int $!count;
                has int $!i;
                method !SET-SELF(\op,\list,\count,\index) {
                    nqp::stmts(
                      ($!op := op),
                      ($!reified := nqp::getattr(list,List,'$!reified')),
                      ($!count = count),
                      ($!i = index),
                      self
                    )
                }
                method new(\op,\list,\count,\index) {
                    nqp::create(self)!SET-SELF(op,list,count,index)
                }
                method pull-one() is raw {
                    nqp::if(
                      nqp::attrinited(self,self.WHAT,'$!result'),
                      nqp::stmts(
                        (my $args := nqp::list($!result)),
                        nqp::until(
                          nqp::iseq_i(nqp::elems($args),$!count)
                            || nqp::islt_i(($!i = nqp::sub_i($!i,1)),0),
                          nqp::unshift($args,nqp::atpos($!reified,$!i))
                        ),
                        nqp::if(
                          nqp::isgt_i(nqp::elems($args),1),
                          ($!result := op.(|nqp::hllize($args))),
                          IterationEnd
                        )
                      ),
                      ($!result := nqp::atpos(
                        $!reified,
                        ($!i = nqp::sub_i($!i,1))
                      ))
                    )
                }
            }.new(op,$v,$count,$i),
            Rakudo::Iterator.OneValue(
              nqp::if(
                $i,
                op.(|nqp::getattr($v,List,'$!reified')),
                op.()
              )
            )
          ))
      },
      sub (+values) {
          Seq.new(nqp::if(
            nqp::isgt_i((my int $i = (my $v :=
                nqp::if(nqp::istype(values,List),values,values.List)
              ).elems),                                       # reifies
              1
            ),
            class :: does Iterator {
                has $!op;
                has $!reified;
                has $!result;
                has int $!i;
                method !SET-SELF(\op,\list,\count) {
                    nqp::stmts(
                      ($!op := op),
                      ($!reified := nqp::getattr(list,List,'$!reified')),
                      ($!i = count),
                      self
                    )
                }
                method new(\op,\li,\co) { nqp::create(self)!SET-SELF(op,li,co) }
                method pull-one() is raw {
                    nqp::if(
                      nqp::attrinited(self,self.WHAT,'$!result'),
                      nqp::if(
                        nqp::isge_i(($!i = nqp::sub_i($!i,1)),0),
                        ($!result := $!op.(nqp::atpos($!reified,$!i),$!result)),
                        IterationEnd
                      ),
                      ($!result := nqp::atpos(
                        $!reified,
                        ($!i = nqp::sub_i($!i,1))
                      ))
                    )
                }
            }.new(op,$v,$i),
            Rakudo::Iterator.OneValue(
              nqp::if(
                $i,
                op.(nqp::atpos(nqp::getattr($v,List,'$!reified'),0)),
                op.()
              )
            )
          ))
      }
    )
}
multi sub METAOP_REDUCE_RIGHT(\op) {
    nqp::if(
      op.count < Inf && nqp::isgt_i((my int $count = op.count),2),
      sub (+values) {
          nqp::if(
            nqp::isge_i((my int $i = (my $v :=
                nqp::if(nqp::istype(values,List),values,values.List)
              ).elems),                                       # reifies
              $count
            ),   # reifies
            nqp::stmts(
              (my $args := nqp::list(
                my $result := nqp::atpos(
                  (my $reified := nqp::getattr($v,List,'$!reified')),
                  ($i = nqp::sub_i($i,1))
                )
              )),
              nqp::until(
                nqp::islt_i(($i = nqp::sub_i($i,1)),0),
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
              op.(|nqp::getattr($v,List,'$!reified')),
              op.()
            )
        )
      },
      sub (+values) {
          nqp::if(
            nqp::isgt_i((my int $i = (my $v :=
                nqp::if(nqp::istype(values,List),values,values.List)
              ).elems),                                       # reifies
              1
            ),
            nqp::stmts(
              (my $result := nqp::atpos(
                nqp::getattr($v,List,'$!reified'),
                ($i = nqp::sub_i($i,1))
              )),
              nqp::while(
                nqp::isge_i(($i = nqp::sub_i($i,1)),0),
                ($result := op.(
                  nqp::atpos(nqp::getattr($v,List,'$!reified'),$i),
                  $result
                ))
              ),
              $result
            ),
            nqp::if(
              $i,
              op.(nqp::atpos(nqp::getattr($v,List,'$!reified'),0)),
              op.()
            )
          )
      }
    )
}

proto sub METAOP_REDUCE_LIST(|) {*}
multi sub METAOP_REDUCE_LIST(\op, \triangle) {
    sub (+values) {
        GATHER({
            my @list;
            for values -> \v {
                @list.push(v);
                take op.(|@list);
            }
        }).lazy-if(values.is-lazy);
    }
}
multi sub METAOP_REDUCE_LIST(\op) {
    sub (+values) { op.(|values) }
}

proto sub METAOP_REDUCE_LISTINFIX(|) {*}
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
    sub (+values) {
        op.(|values.map({nqp::decont($_)}));
    }
}

proto sub METAOP_REDUCE_CHAIN(|) {*}
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

sub METAOP_REDUCE_XOR(\op, $triangle?) {
    X::NYI.new(feature => 'xor reduce').throw;
}

sub METAOP_HYPER(\op, *%opt) {
    -> Mu \a, Mu \b { HYPER(op, a, b, |%opt) }
}

proto sub METAOP_HYPER_POSTFIX(|) {*}
multi sub METAOP_HYPER_POSTFIX(\op) {
    nqp::if(
      nqp::can(op,"nodal"),
      (-> \obj { nodemap(op, obj) }),
      (-> \obj { deepmap(op, obj) })
    )
}

# no indirection for subscripts and such
proto sub METAOP_HYPER_POSTFIX_ARGS(|) {*}
multi sub METAOP_HYPER_POSTFIX_ARGS(\obj,\op) {
    nqp::if(
      nqp::can(op,"nodal"),
      nodemap(op, obj),
      deepmap(op, obj)
    )
}
multi sub METAOP_HYPER_POSTFIX_ARGS(\obj, @args, \op) {
    nqp::if(
      nqp::can(op,"nodal"),
      nodemap( -> \o { op.(o,@args) }, obj ),
      deepmap( -> \o { op.(o,@args) }, obj )
    )
}
multi sub METAOP_HYPER_POSTFIX_ARGS(\obj, \args, \op) {
    nqp::if(
      nqp::can(op,"nodal"),
      nodemap( -> \o { op.(o,|args) }, obj ),
      deepmap( -> \o { op.(o,|args) }, obj )
    )
}

sub METAOP_HYPER_PREFIX(\op) {
    nqp::if(
      nqp::can(op,"nodal"),      # rarely true for prefixes
      (-> \obj { nodemap(op, obj) }),
      (-> \obj { deepmap(op, obj) })
    )
}

sub METAOP_HYPER_CALL(\list, |args) { deepmap(-> $c { $c(|args) }, list) }

sub HYPER(\operator, :$dwim-left, :$dwim-right, |c) {
    Hyper.new(operator, :$dwim-left, :$dwim-right).infix(|c)
}

proto sub deepmap($, $, *%) {*}
multi sub deepmap(\op, \obj) {
    Rakudo::Internals.coremap(op, obj, :deep)
}
multi sub deepmap(\op, Associative \h) {
    my @keys = h.keys;
    hash @keys Z deepmap(op, h{@keys})
}

proto sub nodemap($, $, *%) {*}
multi sub nodemap(\op, \obj) {
    my Mu $rpa := nqp::create(IterationBuffer);
    my \objs := obj.list;
    # as a wanted side-effect is-lazy reifies the list
    fail X::Cannot::Lazy.new(:action<nodemap>) if objs.is-lazy;
    my Mu $items := nqp::getattr(objs, List, '$!reified');
    my Mu $o;
    # We process the elements in two passes, end to start, to
    # prevent users from relying on a sequential ordering of hyper.
    # Also, starting at the end pre-allocates $rpa for us.
    my int $i = nqp::elems($items) - 1;
    nqp::while(
        nqp::isge_i($i, 0),
        nqp::stmts(
            nqp::bindpos($rpa, $i, op.(nqp::atpos($items, $i))),
            $i = nqp::sub_i($i, 2)
        )
    );
    $i = nqp::elems($items) - 2;
    nqp::while(
        nqp::isge_i($i, 0),
        nqp::stmts(
            nqp::bindpos($rpa, $i, op.(nqp::atpos($items, $i))),
            $i = nqp::sub_i($i, 2)
        )
    );
    nqp::p6bindattrinvres(nqp::create(List), List, '$!reified', $rpa)
}

multi sub nodemap(\op, Associative \h) {
    my @keys = h.keys;
    hash @keys Z nodemap(op, h{@keys})
}

proto sub duckmap($, $, *%) {*}
multi sub duckmap(\op, \obj) {
    Rakudo::Internals.coremap(sub (\arg) { CATCH { return arg ~~ Iterable:D ?? duckmap(op,arg) !! arg }; op.(arg); }, obj);
}

multi sub duckmap(\op, Associative \h) {
    my @keys = h.keys;
    hash @keys Z duckmap(op, h{@keys})
}

# vim: ft=perl6 expandtab sw=4
