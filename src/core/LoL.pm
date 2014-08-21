class LoL { # declared in BOOTSTRAP
    # class LoL is List {
    #     has Mu $!descriptor;

    method new(|) { 
        my Mu $args := nqp::p6argvmarray();
        nqp::shift($args);
        nqp::p6list($args, self.WHAT, Mu);
    }
    
    method at_pos($pos is copy) {
        $pos = $pos.Int;
        self.exists_pos($pos)
          ?? nqp::findmethod(List, 'at_pos')(self, $pos)
          !! nqp::p6bindattrinvres(my $v, Scalar, '$!whence',
                 -> { nqp::findmethod(List, 'STORE_AT_POS')(self, $pos, $v) } )
    }

    multi method perl(LoL:D \SELF:) {
        'lol(' ~ self.map({.perl}).join(', ') ~ ')'
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

sub infix:<X>(|lol) {
    my @l;
    my \elem = lol[0];
    @l[0] = nqp::iscont(elem) ?? (elem,).list.item !! (elem,).flat.item;
    my int $n = lol.elems - 1;
    my Mu $end := nqp::list_i();
    for 1..$n -> $i {
        my \elem = lol[$i];
        @l[$i] = nqp::iscont(elem) ?? (elem,).list.item !! (elem,).flat.item;
        nqp::bindpos_i($end,$i,@l[$i].elems);  # presumably eagerifies 2nd and subsequent lists
    }

    my Mu $v := nqp::list();

    # optimize for 2D and 3D crosses
    my @result := do {
        if $n == 1 { # 2-dimensional
            gather {
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
            }
        }
        elsif $n == 2 { # 3-dimensional
            gather {
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
            }
        }
        else {  # general case
            my int $i = 0;
            my Mu $jsave := nqp::list_i();
            gather {
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
            }
        }
    }

    # Don't care if a Range is lazy
    given elem {
        when Range { eager @result }
        default { @result }
    }
}

sub infix:<Z>(|lol) {
    my $arity = lol.elems;
    return if $arity == 0;
    my @l = eager for ^$arity -> $i {
            my \elem = lol[$i];         # can't use mapping here, mustn't flatten

            if nqp::iscont(elem) { (elem,).list.item }
            else                 { (elem,).flat.item }
        }

    gather {
        loop {
            my \p = @l.map: { last unless .gimme(1); .shift }
            last if p.elems < $arity;
            take-rw p.Parcel;
        }
    }
}

my &zip := &infix:<Z>;

sub roundrobin(**@lol) {
    my @l = @lol.map({ (.flat,).list.item });
    gather {
        my $p;
        while $p := @l.grep(*.Bool).map(*.shift).eager.Parcel {
            take $p;
        }
    }
}

# vim: ft=perl6 expandtab sw=4
