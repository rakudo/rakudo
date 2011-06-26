my class Exception {
    has $!ex;
}

sub EXCEPTION(|$) {
    my $ex := nqp::create(Exception);
    nqp::bindattr($ex, Exception, '$!ex', 
        nqp::shift(pir::perl6_current_args_rpa__P()));
    $ex;
}
