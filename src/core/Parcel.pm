my class Parcel {
    has $!storage;             # RPA of Parcel's elements

    method flat() {
        nqp::p6list(pir::clone__PP($!storage), List, 1.Bool)
    }

    method list() {
        nqp::p6list(pir::clone__PP($!storage), List, Mu)
    }

    method at_pos(Parcel:D: \$x) { self.flat.at_pos($x); }

    multi method postcircumfix:<[ ]>(Parcel:D: \$x) { self.flat.[$x] }

    multi method gist(Parcel:D:) {
        my Mu $gist := nqp::list();
        my Mu $iter := nqp::iterator($!storage);
        nqp::push($gist, nqp::unbox_s(nqp::shift($iter).gist)) while $iter;
        nqp::p6box_s(nqp::join(' ', $gist))
    }

    multi method perl(Parcel:D:) {
        my Mu $rpa := pir::clone($!storage);
        my $perl = '(';
        if $rpa {
            $perl = $perl ~ nqp::shift($rpa).perl;
            if $rpa {
                $perl = $perl ~ ', ' ~ nqp::shift($rpa).perl while $rpa;
            }
            else {
                $perl = $perl ~ ',';
            }
        }
        $perl ~ ')';
    }

    method RPA() { $!storage }

    method STORE(|$) {
        # get the list of rvalues to store and lhs containers
        my Mu $args := pir::perl6_current_args_rpa__P();
        nqp::shift($args);
        my $rhs := nqp::p6list($args, List, 1.Bool);   # XXX this might need to be Seq

        # first pass -- scan lhs containers and pick out
        # scalar versus list assignment.  This also reifies
        # the rhs values 
        my Mu $lhs := nqp::clone($!storage);
        my Mu $tv := nqp::list();
        while ($lhs) {
            my Mu $x := $lhs.shift;
            if nqp::iscont($x) {
                # container: scalar assignment
                nqp::push($tv, $x);
                nqp::push($tv, $rhs ?? pir::perl6_decontainerize__PP($rhs.shift) !! Nil);
            }
            elsif nqp::istype($x, Whatever) {
                # Whatever: skip assigning value
                $rhs.shift;
            }
            elsif nqp::istype($x, Parcel) {
                # Parcel: splice into current lhs
                nqp::splice($lhs, nqp::getattr($x, Parcel, '$!storage'), 0, 0)
            }
            else {
                # store entire rhs
                nqp::push($tv, $x);
                nqp::push($tv, $rhs);
                $rhs := ().list;
            }
        }

        # second pass, perform the assignments
        while ($tv) { my $x := nqp::shift($tv); $x = nqp::shift($tv); }
        self
    }

    multi method DUMP(Parcel:D:) {
        self.DUMP-ID() ~ '(:storage(' ~ DUMP($!storage) ~ '))'
    }

    method ARGLIST_FLATTENABLE() { $!storage }
}


my sub infix:<,>(|$) {
    # pir::perl6_box_rpa__PP(pir::perl6_current_args_rpa__P())
    pir::setattribute__0PPsP(
        pir::repr_instance_of__PP(Parcel),
        Parcel,
        '$!storage',
        pir::perl6_current_args_rpa__P());
}


# I'm getting tired of building and unpacking Parcel RPAs by hand,
# so here are some pir::lookalikes to do it.  Eventually we may just
# turn these into opcodes, since it's a common and sometimes
# speed-critical operation.  (I'm leaving &infix:<,> alone above
# to avoid the extra subcall as it's very common.)
sub pir__perl6_box_rpa__PP(|$) {
    pir::setattribute__0PPsP(
        pir::repr_instance_of__PP(Parcel),
        Parcel,
        '$!storage',
        nqp::shift(pir::perl6_current_args_rpa__P()))
}

sub pir__perl6_unbox_rpa__PP(\$parcel) {
    pir::getattribute__PPPs($parcel, Parcel, '$!storage')
}

