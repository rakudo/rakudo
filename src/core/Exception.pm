my class Exception {
    has $!ex;

    method Str() {
        nqp::p6box_s(nqp::atkey($!ex, 'message'))
    }

    method throw() {
        nqp::bindattr(self, Exception, '$!ex', pir::new('Exception'))
            unless pir::defined($!ex);
        pir::setattribute__vPsP($!ex, 'payload', pir::perl6_decontainerize__PP(self));
        pir::throw__0P($!ex)
    }
    method rethrow() {
        pir::rethrow__0P($!ex)
    }
}

sub EXCEPTION(|$) {
    my Mu $parrot_ex := nqp::shift(pir::perl6_current_args_rpa__P());
    my Mu $payload   := nqp::atkey($parrot_ex, 'payload');
    my $ex := pir::defined($payload) ?? $payload !! nqp::create(Exception);
    nqp::bindattr($ex, Exception, '$!ex', $parrot_ex);
    $ex;
}
