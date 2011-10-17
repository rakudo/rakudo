class LoL {
    # declared in BOOTSTRAP:
    #    is List;              # parent class

    method new(|$) { 
        my Mu $args := pir::perl6_current_args_rpa__P();
        nqp::shift($args);
        nqp::p6list($args, self.WHAT, Mu);
    }
    
    method at_pos($pos is copy) {
        $pos = $pos.Int;
        self.exists($pos)
          ?? pir::find_method__PPs(List, 'at_pos')(self, $pos)
          !! pir::setattribute__0PPsP(my $v, Scalar, '$!whence',
                 -> { pir::find_method__PPs(List, 'STORE_AT_POS')(self, $pos, $v) } )
    }

    multi method perl(LoL:D \$self:) {
        self.WHAT.perl ~ '.new(' ~ self.map({.perl}).join(', ') ~ ')'
            ~ ('.item' if nqp::iscont($self));
    }

    method REIFY(Parcel \$parcel) {
        my Mu $rpa := nqp::getattr($parcel, Parcel, '$!storage');
        my Mu $iter := nqp::iterator($rpa);
        my int $i = 0;
        while $iter {
            nqp::bindpos($rpa, $i, my $v = nqp::shift($iter));
            $i = $i + 1;
        }
        pir::find_method__PPs(List, 'REIFY')(self, $parcel)
    }

    method STORE_AT_POS(\$pos, Mu $v is copy) {
        pir::find_method__PPs(List, 'STORE_AT_POS')(self, $pos, $v);
    }

}


sub infix:<X>(**@lol) {
    my @l;
    my @v;
    @l[0] = (@lol[0].flat,).list;
    my int $i = 0;
    my int $n = @lol.elems - 1;
    gather {
        while $i >= 0 {
            if @l[$i] {
                @v[$i] = @l[$i].shift;
                if $i >= $n { my @x = @v; take @x.Parcel }
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

