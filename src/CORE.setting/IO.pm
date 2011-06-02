# XXX Very cheaty, just to get us able to output something.
sub say(Mu $value) {
    pir::say__vS(pir::repr_unbox_str__SP(pir::perl6_decontainerize__PP($value).Str))
}
