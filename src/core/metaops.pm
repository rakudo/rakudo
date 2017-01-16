
sub METAOP_ASSIGN(\op) {
    -> Mu \a, Mu \b { a = op.( ( a.DEFINITE ?? a !! op.() ), b) }
}

sub METAOP_TEST_ASSIGN:<//>(\lhs, $rhs) is raw { lhs // (lhs = $rhs()) }
sub METAOP_TEST_ASSIGN:<||>(\lhs, $rhs) is raw { lhs || (lhs = $rhs()) }
sub METAOP_TEST_ASSIGN:<&&>(\lhs, $rhs) is raw { lhs && (lhs = $rhs()) }
sub METAOP_TEST_ASSIGN:<andthen>(\lhs, $rhs) is raw { lhs andthen (lhs = $rhs()) }
sub METAOP_TEST_ASSIGN:<orelse>(\lhs, $rhs) is raw { lhs orelse (lhs = $rhs()) }

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
        my @loi = eager for lol -> \elem {
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
        my @loi = eager for lol -> \elem {
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

proto sub METAOP_REDUCE_LEFT(|) { * }
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

proto sub METAOP_REDUCE_RIGHT(|) { * }
multi sub METAOP_REDUCE_RIGHT(\op, \triangle) {
    if op.count > 2 and op.count < Inf {
        my $count = op.count;
        sub (+values) {
            my \source = values.reverse.iterator;
            my \first = source.pull-one;
            return () if nqp::eqaddr(first,IterationEnd);

            my @args.unshift: first;
            GATHER({
                take first;
                until nqp::eqaddr((my \current = source.pull-one),IterationEnd) {
                    @args.unshift: current;
                    if @args.elems == $count {
                        my \val = op.(|@args);
                        take val;
                        @args = ();
                        @args.unshift: val;  # allow op to return a Slip
                    }
                }
            }).lazy-if(source.is-lazy);
        }
    }
    else {
        sub (+values) {
            my \iter = values.reverse.iterator;
            my $result := iter.pull-one;
            return () if nqp::eqaddr($result,IterationEnd);

            gather {
                take $result;
                until nqp::eqaddr((my $elem := iter.pull-one),IterationEnd) {
                    take $result := op.($elem, $result)
                }
            }.lazy-if(values.is-lazy);
        }
    }
}
multi sub METAOP_REDUCE_RIGHT(\op) {
    if op.count > 2 and op.count < Inf {
        my $count = op.count;
        sub (+values) {
            my \iter = values.reverse.iterator;
            my \first = iter.pull-one;
           return op.() if nqp::eqaddr(first,IterationEnd);

            my @args.unshift: first;
            my $result := first;
            until nqp::eqaddr((my \value = iter.pull-one),IterationEnd) {
                @args.unshift: value;
                if @args.elems == $count {
                    my \val = op.(|@args);
                    @args = ();
                    @args.unshift: val;  # allow op to return a Slip
                    $result := val;
                }
            }
            $result;
        }
    }
    else {
        sub (+values) {
            my \iter = values.reverse.iterator;
            my \first = iter.pull-one;
            return op.() if nqp::eqaddr(first,IterationEnd);

            my \second = iter.pull-one;
            return op.(first) if nqp::eqaddr(second,IterationEnd);

            my $result := op.(second, first);
            until nqp::eqaddr((my \value = iter.pull-one),IterationEnd) {
                $result := op.(value, $result);
            }
            $result;
        }
    }
}

proto sub METAOP_REDUCE_LIST(|) { * }
multi sub METAOP_REDUCE_LIST(\op, \triangle) {
    sub (+values) {
        GATHER({
            my @list;
            for values -> \v {
                @list.append(v);
                take op.(|@list);
            }
        }).lazy-if(values.is-lazy);
    }
}
multi sub METAOP_REDUCE_LIST(\op) {
    sub (+values) { op.(|values) }
}

proto sub METAOP_REDUCE_LISTINFIX(|) { * }
multi sub METAOP_REDUCE_LISTINFIX(\op, \triangle) {
    sub (|values) {
        my \p = values[0];
        return () unless p.elems;

        my int $i;
        GATHER({
            my @list;
            while $i < p.elems {
                @list.append(p[$i]);
                $i = $i + 1;
                take op.(|@list);
            }
        }).lazy-if(p.is-lazy);
    }
}
multi sub METAOP_REDUCE_LISTINFIX(\op) {
    sub (+values) {
        op.(|values.map({nqp::decont($_)}));
    }
}

proto sub METAOP_REDUCE_CHAIN(|) { * }
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
        my $state := True;
        my \iter = values.iterator;
        my $current := iter.pull-one;
        return True if nqp::eqaddr($current,IterationEnd);

        until nqp::eqaddr((my $next := iter.pull-one),IterationEnd) {
            $state := op.($current, $next);
            return $state unless $state;
            $current := $next;
        }
        $state;
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

proto sub HYPER(|) { * }

multi sub HYPER(&op, \left, \right, :$dwim-left, :$dwim-right) {
    op(left, right);
}

multi sub HYPER(&op, Associative:D \left, Associative:D \right, :$dwim-left, :$dwim-right) {
    my %keyset;
    if !$dwim-left {
        %keyset{$_} = 1 for left.keys;
    }
    else {
        %keyset{$_} = 1 if right.EXISTS-KEY($_) for left.keys;
    }
    if !$dwim-right {
        %keyset{$_} = 1 for right.keys;
    }
    my @keys = %keyset.keys;
    my $type = left.WHAT;
    my \result := $type.new;
    result = quietly @keys Z=> HYPER(&op, left{@keys}, right{@keys}, :$dwim-left, :$dwim-right);
    nqp::iscont(left) ?? result.item !! result;
}

multi sub HYPER(&op, Associative:D \left, \right, :$dwim-left, :$dwim-right) {
    my @keys = left.keys;
    my $type = left.WHAT;
    my \result := $type.new;
    result = @keys Z=> HYPER(&op, left{@keys}, right, :$dwim-left, :$dwim-right);
    nqp::iscont(left) ?? result.item !! result;
}

multi sub HYPER(&op, \left, Associative:D \right, :$dwim-left, :$dwim-right) {
    my @keys = right.keys;
    my $type = right.WHAT;
    my \result := $type.new;
    result = @keys Z=> HYPER(&op, left, right{@keys}, :$dwim-left, :$dwim-right);
    nqp::iscont(right) ?? result.item !! result;
}

multi sub HYPER(&operator, Positional:D \left, \right, :$dwim-left, :$dwim-right) {
    my @result;
    X::HyperOp::Infinite.new(:side<left>, :&operator).throw if left.is-lazy;
    my int $elems = left.elems;
    X::HyperOp::NonDWIM.new(:&operator, :left-elems($elems), :right-elems(1), :recursing(callframe(3).code.name eq 'HYPER')).throw
        unless $elems == 1 or $elems > 1 and $dwim-right or $elems == 0 and $dwim-left || $dwim-right;
    my \lefti := left.iterator;
    my int $i = 0;
    until nqp::eqaddr((my \value := lefti.pull-one),IterationEnd) {
        @result[$i++] := HYPER(&operator, value, right, :$dwim-left, :$dwim-right);
    }
    # Coerce to the original type if it's a subtype of List
    my $type = nqp::istype(left, List) ?? left.WHAT !! List;
    nqp::iscont(left) ?? $type(|@result.eager).item !! $type(|@result.eager)
}

multi sub HYPER(&operator, \left, Positional:D \right, :$dwim-left, :$dwim-right) {
    my @result;
    X::HyperOp::Infinite.new(:side<right>, :&operator).throw if right.is-lazy;
    my int $elems = right.elems;
    X::HyperOp::NonDWIM.new(:&operator, :left-elems(1), :right-elems($elems), :recursing(callframe(3).code.name eq 'HYPER')).throw
        unless $elems == 1 or $elems > 1 and $dwim-left or $elems == 0 and $dwim-left || $dwim-right;
    my \righti := right.iterator;
    my int $i = 0;
    until nqp::eqaddr((my \value := righti.pull-one),IterationEnd) {
        @result[$i++] := HYPER(&operator, left, value, :$dwim-left, :$dwim-right);
    }
    # Coerce to the original type if it's a subtype of List
    my $type = nqp::istype(right, List) ?? right.WHAT !! List;
    nqp::iscont(right) ?? $type(|@result.eager).item !! $type(|@result.eager)
}

multi sub HYPER(&operator, Iterable:D \left, Iterable:D \right, :$dwim-left, :$dwim-right) {
    my \left-iterator = left.iterator;
    my \right-iterator = right.iterator;

    # Check whether any side is lazy. They must not be to proceed.
    if left-iterator.is-lazy {
        X::HyperOp::Infinite.new(:side<both>, :&operator).throw if right-iterator.is-lazy;
        X::HyperOp::Infinite.new(:side<left>, :&operator).throw if not $dwim-left or $dwim-right;
    }
    X::HyperOp::Infinite.new(:side<right>, :&operator).throw if right-iterator.is-lazy and
        (not $dwim-right or $dwim-left);

    my \lefti  := Rakudo::Iterator.DWIM(left-iterator);
    my \righti := Rakudo::Iterator.DWIM(right-iterator);

    my \result := IterationBuffer.new;
    loop {
        my \leftv := lefti.pull-one;
        my \rightv := righti.pull-one;

        X::HyperOp::NonDWIM.new(:&operator, :left-elems(lefti.count-elems), :right-elems(righti.count-elems), :recursing(callframe(3).code.name eq 'HYPER')).throw
            if !$dwim-left and !$dwim-right and (lefti.ended != righti.ended);

        last if ($dwim-left and $dwim-right) ?? (lefti.ended and righti.ended) !!
               (($dwim-left or lefti.ended) and ($dwim-right or righti.ended));
        last if $++ == 0 and ($dwim-left and lefti.ended or $dwim-right and righti.ended);

        result.push(HYPER(&operator, leftv, rightv, :$dwim-left, :$dwim-right));
    }

    # Coerce to the original type
    my $type = nqp::istype(left, List) ?? left.WHAT !! List; # keep subtypes of List
    my \retval = $type.new;
    nqp::bindattr(retval, List, '$!reified', result);
    nqp::iscont(left) ?? retval.item !! retval;
}

multi sub HYPER(\op, \obj) {
    nqp::if(
      nqp::can(op,"nodal"),
      nodemap(op, obj),
      deepmap(op,obj)
    )
}

proto sub deepmap(|) { * }

multi sub deepmap(\op, \obj) {
    Rakudo::Internals.coremap(op, obj, :deep)
}

multi sub deepmap(\op, Associative \h) {
    my @keys = h.keys;
    hash @keys Z deepmap(op, h{@keys})
}

proto sub nodemap(|) { * }
multi sub nodemap(\op, \obj) {
    my Mu $rpa := nqp::list();
    my \objs := obj.list;
    # as a wanted side-effect is-lazy reifies the list
    fail X::Cannot::Lazy.new(:action('deepmap')) if objs.is-lazy;
    my Mu $items := nqp::getattr(objs, List, '$!reified');
    my Mu $o;
    # We process the elements in two passes, end to start, to
    # prevent users from relying on a sequential ordering of hyper.
    # Also, starting at the end pre-allocates $rpa for us.
    my int $i = nqp::elems($items) - 1;
    nqp::while(
        nqp::isge_i($i, 0),
        nqp::stmts(
            ($o := nqp::atpos($items, $i)),
            nqp::bindpos($rpa, $i,
                nqp::if(Mu,             # hack cuz I don't understand nqp
                        $o.new(nodemap(op, $o)).item,
                        op.($o))),
            $i = nqp::sub_i($i, 2)
        )
    );
    $i = nqp::elems($items) - 2;
    nqp::while(
        nqp::isge_i($i, 0),
        nqp::stmts(
            ($o := nqp::atpos($items, $i)),
            nqp::bindpos($rpa, $i,
                nqp::if(Mu,             # hack cuz I don't understand nqp
                        $o.new(nodemap(op, $o)).item,
                        op.($o))),
            $i = nqp::sub_i($i, 2)
        )
    );
    nqp::p6bindattrinvres(nqp::create(List), List, '$!reified', $rpa)
}

multi sub nodemap(\op, Associative \h) {
    my @keys = h.keys;
    hash @keys Z nodemap(op, h{@keys})
}

proto sub duckmap(|) { * }
multi sub duckmap(\op, \obj) {
    Rakudo::Internals.coremap(sub (\arg) { CATCH { return arg ~~ Iterable:D ?? duckmap(op,arg) !! arg }; op.(arg); }, obj);
}

multi sub duckmap(\op, Associative \h) {
    my @keys = h.keys;
    hash @keys Z duckmap(op, h{@keys})
}

# vim: ft=perl6 expandtab sw=4
