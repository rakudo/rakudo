class LoL { # declared in BOOTSTRAP
    # class LoL is List {
    #     has Mu $!descriptor;

    method new(|) {
        my Mu $args := nqp::p6argvmarray();
        nqp::shift($args);
        nqp::p6list($args, self.WHAT, Mu);
    }

    method AT-POS($pos is copy) {
        $pos = $pos.Int;
        self.EXISTS-POS($pos)
          ?? nqp::findmethod(List, 'AT-POS')(self, $pos)
          !! nqp::p6bindattrinvres(my $v, Scalar, '$!whence',
                 -> { nqp::findmethod(List, 'STORE_AT_POS')(self, $pos, $v) } )
    }

    multi method perl(LoL:D \SELF:) {
        '(' ~ self.map({.list.map({.perl}).join(', ')}).join('; ') ~ ')'
            ~ ('.item' if nqp::iscont(SELF));
    }

    method REIFY(Parcel \parcel, Mu \nextiter) {
        my Mu $rpa := nqp::getattr(parcel, Parcel, '$!storage');
        my Mu $iter := nqp::iterator($rpa);
        my int $i = 0;
        while $iter {
            nqp::bindpos($rpa, $i, my $v = nqp::shift($iter));
            $i = $i + 1;
        }
        nqp::findmethod(List, 'REIFY')(self, parcel, nextiter)
    }

    method STORE_AT_POS(\pos, Mu $v is copy) {
        nqp::findmethod(List, 'STORE_AT_POS')(self, pos, $v);
    }

}

sub lol (**@l) { @l }

sub find-reducer-for-op($op) {
    try my %prec := $op.prec;
    return &METAOP_REDUCE_LEFT if (nqp::isnull(%prec) or ! %prec);
    my $reducer = %prec<prec> eq 'f='
        ?? 'listinfix'
        !! %prec<assoc> // 'left';
    ::('&METAOP_REDUCE_' ~ $reducer.uc);
}

sub infix:<X>(|lol) {
    if lol.hash {
        my $op = lol.hash<with>;
        return METAOP_CROSS($op, find-reducer-for-op($op))(|lol.list) if $op;
    }
    my int $n = lol.elems - 1;
    my $Inf = False;
    my @l = eager for 0..$n -> $i {
        my \elem = lol[$i];         # can't use mapping here, mustn't flatten
        $Inf = True if $i and elem.infinite;
        if nqp::iscont(elem) { (elem,).list.item }
        else                 { (elem,).flat.item }
    }

    # eagerize 2nd and subsequent lists if finite
    my Mu $end := nqp::list_i();
    if !$Inf {
        for 1 .. $n -> $i {
            nqp::bindpos_i($end,$i,@l[$i].elems);
        }
    }

    my Mu $v := nqp::list();
    my int $i = 0;

    # Don't care if a finite Range is lazy
    my $policy = &list;
    if nqp::istype(lol[0],Range) {
        $policy = &EAGER unless $Inf || lol[0].infinite;
    }

    if $Inf {  # general case treats all lists as potentially lazy
        return gather {
            while $i >= 0 {
                if @l[$i].gimme(1) {
                    nqp::bindpos($v, $i, @l[$i].shift);
                    if $i >= $n { take nqp::clone($v) }
                    else {
                        $i = $i + 1;
                        my \elem = lol[$i];
                        @l[$i] = nqp::iscont(elem) ?? (elem,).list.item !! (elem,).flat.item;
                    }
                }
                else { $i = $i - 1 }
            }
        }
    }
    # optimize for 2D and 3D crosses
    elsif $n == 1 { # 2-dimensional
        $policy(gather {
            my int $e = nqp::atpos_i($end,1);
            my $l0 = @l[0];
            my $l1 = @l[1];
            while $l0.gimme(1) {
                nqp::bindpos($v, 0, @l[0].shift);
                loop (my int $j = 0; $j < $e; $j = $j + 1) {
                    nqp::bindpos($v, 1, $l1[$j]);
                    take nqp::clone($v);
                }
            }
        })
    }
    elsif $n == 2 { # 3-dimensional
        $policy(gather {
            my int $e1 = nqp::atpos_i($end,1);
            my int $e2 = nqp::atpos_i($end,2);
            my $l0 = @l[0];
            my $l1 = @l[1];
            my $l2 = @l[2];
            while $l0.gimme(1) {
                nqp::bindpos($v, 0, @l[0].shift);
                loop (my int $j = 0; $j < $e1; $j = $j + 1) {
                    nqp::bindpos($v, 1, $l1[$j]);
                    loop (my int $k = 0; $k < $e2; $k = $k + 1) {
                        nqp::bindpos($v, 2, $l2[$k]);
                        take nqp::clone($v);
                    }
                }
            }
        })
    }
    else { # more than 3 dimensions
        my Mu $jsave := nqp::list_i();
        $policy(gather {
            while $i == 0 {
                if @l[0].gimme(1) {
                    nqp::bindpos($v, $i, @l[0].shift);

                    if $i >= $n { take nqp::clone($v) }
                    else { $i = $i + 1; }

                    my int $j = 0;
                    while $i >= 1 {
                        if $j < nqp::atpos_i($end,$i) {
                            nqp::bindpos($v, $i, @l[$i][$j]);
                            $j = $j + 1;

                            if $i >= $n { take nqp::clone($v) }
                            else {
                                nqp::bindpos_i($jsave, $i, $j);
                                $i = $i + 1;
                                $j = 0;
                            }
                        }
                        else {
                            $i = $i - 1;
                            $j = nqp::atpos_i($jsave,$i);
                        }
                    }
                }
                else { $i = $i - 1 }
            }
        })
    }
}

my &cross = &infix:<X>;

sub infix:<Z>(|lol) {
    if lol.hash {
        my $op = lol.hash<with>;
        return METAOP_ZIP($op, find-reducer-for-op($op))(|lol.list) if $op;
    }
    my $arity = lol.elems;
    return if $arity == 0;
    my @l = eager for ^$arity -> $i {
            my \elem = lol[$i];         # can't use mapping here, mustn't flatten

            if nqp::iscont(elem) { (elem,).list.item }
            else                 { (elem,).flat.item }
        }

    gather {
        loop {
            my \p = @l.for: { last unless .gimme(1); .shift }
            last if p.elems < $arity;
            take-rw p.Parcel;
        }
    }
}

my &zip := &infix:<Z>;

sub roundrobin(**@lol) {
    my @l = @lol.for({ (.flat,).list.item });
    gather {
        my $p;
        while $p := @l.grep(*.Bool).for(*.shift).eager.Parcel {
            take $p;
        }
    }
}

# vim: ft=perl6 expandtab sw=4
