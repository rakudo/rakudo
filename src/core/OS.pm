proto sub gethost(|) {*}
multi sub gethost(){
    return OS::Name.new(name => nqp::p6box_s(nqp::gethostname()));
}
