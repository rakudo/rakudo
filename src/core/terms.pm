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

    my @INC;
    my Mu $config := nqp::atpos(pir::getinterp, pir::const::IGLOBALS_CONFIG_HASH);
    # add PERL6LIB
    @INC.push(%ENV<PERL6LIB>.split(
        nqp::p6box_s(nqp::atkey($config, 'osname')) eq 'MSWin32' ?? ':' !! ';'
    )) if %ENV<PERL6LIB>;
    # add ~/.perl6/lib
    @INC.push((%ENV<HOME> // %ENV<HOMEDRIVE> ~ %ENV<HOMEPATH>) ~ '/.perl6/lib');
    # add the installed Parrot languages/perl6/lib directory
    @INC.push(
       nqp::p6box_s(nqp::atkey($config, 'libdir')) ~
       nqp::p6box_s(nqp::atkey($config, 'versiondir')) ~
       'languages/perl6/lib'
    );
    # add current directory
    @INC.push('.'); # remove this when 'use lib' works fine
    nqp::bindkey(pir::get_who__PP(PROCESS), '@INC', @INC);

    my $PID = nqp::p6box_i(pir::getinterp.getpid());
    nqp::bindkey(pir::get_who__PP(PROCESS), '$PID', $PID);
}
