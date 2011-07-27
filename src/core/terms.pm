sub term:<time>() { nqp::p6box_i(pir::time__I()) }

{
    my @ARGS;
    my Mu $argiter := pir::get_hll_global__Ps('$!ARGITER');
    @ARGS.push(nqp::p6box_s(nqp::shift($argiter))) while $argiter;
    nqp::bindkey(pir::get_who__PP(PROCESS), '@ARGS', @ARGS);
    $PROCESS::ARGFILES = ArgFiles.new(:args(@ARGS));

    my %ENV;
    my Mu $env := pir::new__Ps('Env');
    my Mu $enviter := nqp::iterator($env);
    my $key;
    while $enviter {
        $key = nqp::p6box_s(pir::shift__SP($enviter));
        %ENV{$key} = nqp::p6box_s(nqp::atkey($env, nqp::unbox_s($key)));
    }
    nqp::bindkey(pir::get_who__PP(PROCESS), '%ENV', %ENV);
}

