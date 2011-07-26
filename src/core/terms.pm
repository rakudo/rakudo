sub term:<time>() { nqp::p6box_i(pir::time__I()) }

{
    my @ARGS;
    my Mu $argiter := pir::get_hll_global__Ps('$!ARGITER');
    @ARGS.push(nqp::p6box_s(nqp::shift($argiter))) while $argiter;
    nqp::bindkey(pir::get_who__PP(PROCESS), '@ARGS', @ARGS);
    $PROCESS::ARGFILES = ArgFiles.new(:args(@ARGS));
}

