class LoL { # declared in BOOTSTRAP
    # class LoL is List {
    #     has $!descriptor;

    method new(|) { 
        my Mu $args := nqp::p6argvmarray();
        nqp::shift($args);
        nqp::p6list($args, self.WHAT, Mu);
    }
    
    method at_pos($pos is copy) {
        $pos = $pos.Int;
        self.exists($pos)
          ?? nqp::findmethod(List, 'at_pos')(self, $pos)
          !! nqp::p6bindattrinvres(my $v, Scalar, '$!whence',
                 -> { nqp::findmethod(List, 'STORE_AT_POS')(self, $pos, $v) } )
    }

    multi method perl(LoL:D \SELF:) {
        self.WHAT.perl ~ '.new(' ~ self.map({.perl}).join(', ') ~ ')'
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


sub infix:<X>(**@lol) {
    my @l;
    @l[0] = (@lol[0].flat,).list;
    my int $i = 0;
    my int $n = @lol.elems - 1;
    my Mu $v := nqp::list();
    gather {
        while $i >= 0 {
            if @l[$i] {
                nqp::bindpos($v, $i, @l[$i].shift);
                if $i >= $n { take nqp::p6parcel(nqp::clone($v), nqp::null()) }
                else {
                    $i = $i + 1;
                    @l[$i] = (@lol[$i].flat,).list;
                }
            }
            else { $i = $i - 1 }
        }
    }
};

sub infix:<Z>(**@lol) {
    my @l = @lol.map({ (.flat,).list.item });
    gather {
        my $loop = 1;
        while $loop {
            my $p := @l.map({ $loop = 0 unless $_; .shift }).eager.Parcel;
            take $p if $loop;
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
