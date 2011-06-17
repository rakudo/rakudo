my class Parcel is Iterable {
    has $!storage;             # RPA of Parcel's elements

    method flat() {
        pir::perl6_list_from_rpa__PPPP(List, pir::clone__PP($!storage), 1.Bool)
    }

    method list() {
        pir::perl6_list_from_rpa__PPPP(List, pir::clone__PP($!storage), Mu)
    }

    # Since Parcels are immutable, they are their own (self-reifying) iterator
    method iterator() { self }
    method reify($n?) { self }

    method perl() {
        my Mu $rpa := pir::clone($!storage);
        my $perl = '(';
        $perl = $perl ~ pir::shift__PP($rpa).perl if $rpa;
        $perl = $perl ~ ', ' ~ pir::shift__PP($rpa).perl while $rpa;
        $perl = $perl ~ ')';
    }

}


my sub infix:<,>(|$) {
    pir::setattribute__0PPsP(
        pir::repr_instance_of__PP(Parcel),
        Parcel,
        '$!storage',
        pir::perl6_current_args_rpa__P());
}


