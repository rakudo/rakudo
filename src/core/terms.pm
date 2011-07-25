sub term:<time>() { nqp::p6box_i(pir::time__I()) }

{
    my @ARGS;
    my Mu $argiter := nqp::iterator(pir::get_hll_global__Ps('@!ARGS'));
    $argiter := nqp::iterator(pir::get_hll_global__Ps('@!ARGS'));
    nqp::shift($argiter) if $argiter;
    @ARGS.push(nqp::p6box_s(nqp::shift($argiter))) while $argiter;
    nqp::bindkey(pir::get_who__PP(PROCESS), '@ARGS', @ARGS);
    $PROCESS::ARGFILES = ArgFiles.new(:args(@ARGS));
}

