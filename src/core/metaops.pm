
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
    -> **@lol {
        my $rop = @lol.elems == 2 ?? op !! &reduce(op);
        my @l;
        my @v;
        @l[0] = (@lol[0].flat,).list;
        my int $i = 0;
        my int $n = @lol.elems - 1;
        gather {
            while $i >= 0 {
                if @l[$i].gimme(1) {
                    @v[$i] = @l[$i].shift;
                    if $i >= $n { my @x = @v; take $rop(|@x); }
                    else {
                        $i = $i + 1;
                        @l[$i] = (@lol[$i].flat,).list;
                    }
                }
                else { $i = $i - 1; }
            }
        }
    }
}

sub METAOP_ZIP(\op, &reduce) {
    -> **@lol {
        my $rop = @lol.elems == 2 ?? op !! &reduce(op);
        my @l = @lol.map({ (.flat,).list.item });
        gather {
            my $loop = 1;
            while $loop {
                my @z = @l.map({ $loop = 0 unless $_; .shift });
                take-rw $rop(|@z) if $loop;
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


sub METAOP_REDUCE_CHAIN(\op, :$triangle) {
    $triangle
        ??  sub (*@values) {
                my Mu $current = @values.shift;
                my $state = op.();
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
                my $state = op.();
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

sub METAOP_HYPER_POSTFIX(\obj, \op) { hyper(op, obj) }

sub METAOP_HYPER_PREFIX(\op, \obj) { hyper(op, obj) }

sub METAOP_HYPER_CALL(\list, |args) { hyper(-> $c { $c(|args) }, list) }

proto sub hyper(|) { * }
multi sub hyper(\op, \a, \b, :$dwim-left, :$dwim-right) { 
    my @alist := a.DEFINITE ?? a.flat !! [a];
    my @blist := b.DEFINITE ?? b.flat !! [b];
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
                        $o.new(hyper(op, $o)).item,
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
                        $o.new(hyper(op, $o)).item,
                        op.($o))),
            $i = nqp::sub_i($i, 2)
        )
    );
    nqp::p6parcel($rpa, Nil);
}

multi sub hyper(\op, Associative \h) {
    my @keys = h.keys;
    hash @keys Z hyper(op, h{@keys})
}

multi sub hyper(\op, Associative \a, Associative \b, :$dwim-left, :$dwim-right) {
    my %k;
    for a.keys { %k{$_} = 1 if !$dwim-left || b.exists($_) }
    for b.keys { %k{$_} = 1 if !$dwim-right }
    my @keys = %k.keys;
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

