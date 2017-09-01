sub gethostname( --> Str:D){
    nqp::p6box_s(nqp::gethostname());
}

# vim: ft=perl6 expandtab sw=4
