
sub METAOP_ASSIGN(\op) {
    -> Mu \a, Mu \b { a = op.( a // op.(), b) }
}

sub METAOP_TEST_ASSIGN:<//>(\lhs, $rhs) is rw { lhs // (lhs = $rhs()) }
sub METAOP_TEST_ASSIGN:<||>(\lhs, $rhs) is rw { lhs || (lhs = $rhs()) }
sub METAOP_TEST_ASSIGN:<&&>(\lhs, $rhs) is rw { lhs && (lhs = $rhs()) }

sub METAOP_NEGATE(\op) {
    -> Mu \a, Mu \b { !op.(a ,b) }
}

sub METAOP_REVERSE(\op) {
    -> Mu \a, Mu \b { op.(b, a) }
}

sub METAOP_CROSS(\op, &reduce) {
    return &infix:<X> if op === &infix:<,>;

    -> |lol {
        my $rop = lol.elems == 2 ?? op !! &reduce(op);
        my @lol = eager for ^lol.elems -> $i {
            my \elem = lol[$i];         # can't use mapping here, mustn't flatten

            if nqp::iscont(elem) { (elem,).list.item }
            else                 { (elem,).flat.item }
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
                    if $i >= $n { take $rop(|@v); }
                    else { $i = $i + 1; @j.push($j); $j = 0; }
                }
                elsif @lol[$i].gimme(1) {
                    my Mu $o := @lol[$i].shift;
                    nqp::bindpos($sublist, $j, $o);
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
        }
    }
}

sub METAOP_ZIP(\op, &reduce) {
    -> |lol {
        my $arity = lol.elems;
        my $rop = $arity == 2 ?? op !! &reduce(op);
        my @lol = eager for ^lol.elems -> $i {
            my \elem = lol[$i];         # can't use mapping here, mustn't flatten

            if nqp::iscont(elem) { (elem,).list.item }
            else                 { (elem,).flat.item }
        }
        gather {
            loop {
                my \z = @lol.map: { last unless .gimme(1); .shift }
                last if z.elems < $arity;
                take-rw $rop(|z);
            }
        }
    }
}

sub METAOP_REDUCE_LEFT(\op, :$triangle) {
    my $x := $triangle ??
        (sub (*@values) {
            return () unless @values.gimme(1);
            GATHER({
                my $result := @values.shift;
                take $result;
                take ($result := op.($result, @values.shift))
                    while @values.gimme(1);
            }, :infinite(@values.infinite))
        }) !!
        (sub (*@values) {
            return op.() unless @values.gimme(1);
            my $result := @values.shift;
            return op.($result) unless @values.gimme(1);
            my int $i;
            while my int $c = @values.gimme(1000) {
                $i = 0;
                $result := op.($result, @values.shift)
                    while ($i = $i + 1) <= $c;
            }
            $result;
        })
}

sub METAOP_REDUCE_RIGHT(\op, :$triangle) {
    my $x :=
    sub (*@values) {
        my $list = @values.reverse;
        if $triangle {
            return () unless $list.gimme(1);
            gather {
                my $result := $list.shift;
                take $result;
                take ($result := op.($list.shift, $result))
                    while $list.gimme(1);
            }
        }
        else {
            return op.() unless $list.gimme(1);
            my $result := $list.shift;
            return op.($result) unless $list.gimme(1);
            my int $i;
            while my int $c = $list.gimme(1000) {
                $i = 0;
                $result := op.($list.shift, $result)
                    while ($i = $i + 1) <= $c;
            }
            $result;
        }
    }
}


sub METAOP_REDUCE_LIST(\op, :$triangle) {
    $triangle
        ??  sub (*@values) {
                return () unless @values.gimme(1);
                GATHER({
                    my @list;
                    while @values {
                        @list.push(@values.shift);
                        take op.(|@list);
                    }
                }, :infinite(@values.infinite))
            }
        !!  sub (*@values) { op.(|@values) }
}


sub METAOP_REDUCE_LISTINFIX(\op, :$triangle) {
    $triangle
        ??  sub (|values) {
                my \p = values[0];
                return () unless p.elems;
                my int $i = 0;
                GATHER({
                    my @list;
                    while $i < p.elems {
                        @list.push(p[$i]);
                        $i = $i + 1;
                        take op.(|@list);
                    }
                }, :infinite(p.infinite))
            }
        !!  sub (|values) { my \p = values[0]; op.(|p) }
}


sub METAOP_REDUCE_CHAIN(\op, :$triangle) {
    $triangle
        ??  sub (*@values) {
                my $state = True;
                my Mu $current = @values.shift;
                gather {
                    take $state;
                    while $state && @values.gimme(1) {
                        $state = op.($current, @values[0]);
                        take $state;
                        $current = @values.shift;
                    }
                    take False for @values;
                }

            }
        !! sub (*@values) {
                my $state = True;
                my Mu $current = @values.shift;
                while @values.gimme(1) {
                    $state = op.($current, @values[0]);
                    $current = @values.shift;
                    return $state unless $state;
                }
                $state;
            }
}


sub METAOP_REDUCE_XOR(\op, :$triangle) {
    X::NYI.new(feature => 'xor reduce').throw;
}

sub METAOP_HYPER(\op, *%opt) {
    -> Mu \a, Mu \b { hyper(op, a, b, |%opt) }
}

proto sub METAOP_HYPER_POSTFIX(|) {*}
multi sub METAOP_HYPER_POSTFIX(\obj, \op) { flatmap(op, obj) }
multi sub METAOP_HYPER_POSTFIX(\obj, \args, \op) { flatmap( -> \o { op.(o,|args) }, obj ) }

sub METAOP_HYPER_PREFIX(\op, \obj) { deepmap(op, obj) }

sub METAOP_HYPER_CALL(\list, |args) { deepmap(-> $c { $c(|args) }, list) }

proto sub hyper(|) { * }
multi sub hyper(\op, \a, \b, :$dwim-left, :$dwim-right) { 
    my @alist := a.DEFINITE ?? a.flat !! (a,).list;
    my @blist := b.DEFINITE ?? b.flat !! (b,).list;
    my $elems = 0;
    if $dwim-left && $dwim-right { $elems = max(@alist.elems, @blist.elems) }
    elsif $dwim-left { $elems = @blist.elems }
    elsif $dwim-right { $elems = @alist.elems }
    else { 
        X::HyperOp::NonDWIM.new(
                operator    => op,
                left-elems  => @alist.elems,
                right-elems => @blist.elems,
        ).throw
            if @alist.elems != @blist.elems
    }
    @alist := (@alist xx *).munch($elems) if @alist.elems < $elems;
    @blist := (@blist xx *).munch($elems) if @blist.elems < $elems;

    (@alist Z @blist).map(
        -> \x, \y {
            Iterable.ACCEPTS(x)
              ?? x.new(hyper(op, x, y, :$dwim-left, :$dwim-right)).item
              !! (Iterable.ACCEPTS(y)
                    ?? y.new(hyper(op, x, y, :$dwim-left, :$dwim-right)).item
                    !! op.(x, y))
        }
    ).eager
}

multi sub hyper(\op, \obj) {
    # fake it till we get a nodal trait
    my $nodal = True;

    $nodal ?? flatmap(op, obj) !! deepmap(op,obj);
}

proto sub deepmap(|) { * }

multi sub deepmap(\op, \obj) {
    my Mu $rpa := nqp::list();
    my Mu $items := nqp::p6listitems(obj.flat.eager);
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
                nqp::if(nqp::istype($o, Iterable),
                        $o.new(deepmap(op, $o)).item,
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
                nqp::if(nqp::istype($o, Iterable),
                        $o.new(deepmap(op, $o)).item,
                        op.($o))),
            $i = nqp::sub_i($i, 2)
        )
    );
    nqp::p6parcel($rpa, Nil);
}

multi sub deepmap(\op, Associative \h) {
    my @keys = h.keys;
    hash @keys Z deepmap(op, h{@keys})
}

proto sub flatmap(|) { * }
multi sub flatmap(\op, \obj) {
    my Mu $rpa := nqp::list();
    my Mu $items := nqp::p6listitems(obj.flat.eager);
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
                        $o.new(flatmap(op, $o)).item,
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
                        $o.new(flatmap(op, $o)).item,
                        op.($o))),
            $i = nqp::sub_i($i, 2)
        )
    );
    nqp::p6parcel($rpa, Nil);
}

multi sub flatmap(\op, Associative \h) {
    my @keys = h.keys;
    hash @keys Z flatmap(op, h{@keys})
}

proto sub duckmap(|) { * }
multi sub duckmap(\op, \obj) {
    flatmap(-> \arg { try { op.(arg) } // try { duckmap(op,arg) } }, obj); 
}

multi sub duckmap(\op, Associative \h) {
    my @keys = h.keys;
    hash @keys Z duckmap(op, h{@keys})
}

multi sub hyper(\op, Associative \a, Associative \b, :$dwim-left, :$dwim-right) {
    my %k;
    if !$dwim-left {
        %k{$_} = 1 for a.keys;
    }
    else {
        %k{$_} = 1 if b.exists_key($_) for a.keys;
    }
    if !$dwim-right {
        %k{$_} = 1 for b.keys;
    }
    my @keys := %k.keys;
    hash @keys Z hyper(op, a{@keys}, b{@keys}, :$dwim-left, :$dwim-right)
}

multi sub hyper(\op, Associative \a, \b, :$dwim-left, :$dwim-right) {
    my @keys = a.keys;
    hash @keys Z hyper(op, a{@keys}, b, :$dwim-left, :$dwim-right);
}

multi sub hyper(\op, \a, Associative \b, :$dwim-left, :$dwim-right) {
    my @keys = b.keys;
    hash @keys Z hyper(op, a, b{@keys}, :$dwim-left, :$dwim-right);
}


# vim: ft=perl6 expandtab sw=4
