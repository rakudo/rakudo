my class Parcel {
    has $!storage;             # RPA of Parcel's elements

    method flat() {
        pir::perl6_list_from_rpa__PPPP(List, pir::clone__PP($!storage), 1.Bool)
    }

    method list() {
        pir::perl6_list_from_rpa__PPPP(List, pir::clone__PP($!storage), Mu)
    }

    multi method gist(Parcel:D:) {
        my @gist;
        my Mu $rpa := pir::clone__PP($!storage);
        @gist.push( pir::shift__PP($rpa).gist ) while $rpa;
        @gist;
    }

    multi method perl(Parcel:D:) {
        my Mu $rpa := pir::clone($!storage);
        my $perl = '(';
        if $rpa {
            $perl = $perl ~ pir::shift__PP($rpa).perl;
            if $rpa {
                $perl = $perl ~ ', ' ~ pir::shift__PP($rpa).perl while $rpa;
            }
            else {
                $perl = $perl ~ ',';
            }
        }
        $perl ~ ')';
    }

    multi method DUMP(Parcel:D:) {
        self.DUMP-ID() ~ '(:storage(' ~ DUMP($!storage) ~ '))'
    }
}


my class Nil is Parcel { 
    method new() { Nil }
    method flat() { ().flat }
    method list() { ().list }
    method gist() { 'Nil' }
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
        pir::shift__PP(pir::perl6_current_args_rpa__P()))
}

sub pir__perl6_unbox_rpa__PP(\$parcel) {
    pir::getattribute__PPPs($parcel, Parcel, '$!storage')
}
