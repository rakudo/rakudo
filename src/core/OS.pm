sub gethostname( --> Str){
    return nqp::p6box_s(nqp::gethostname());
}
