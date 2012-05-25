sub term:<time>() { nqp::p6box_i(pir::time__I()) }

{
    my @ARGS;
    my Mu $argiter := pir::get_hll_global__Ps('$!ARGITER');
    @ARGS.push(nqp::p6box_s(nqp::shift($argiter))) while $argiter;
    nqp::bindkey(pir::get_who__PP(PROCESS), '@ARGS', @ARGS);
    $PROCESS::ARGFILES = IO::ArgFiles.new(:args(@ARGS));

    my %ENV;
    my Mu $env := pir::new__Ps('Env');
    my Mu $enviter := nqp::iterator($env);
    my $key;
    while $enviter {
        $key = nqp::p6box_s(pir::shift__SP($enviter));
        %ENV{$key} = nqp::p6box_s(nqp::atkey($env, nqp::unbox_s($key)));
    }
    nqp::bindkey(pir::get_who__PP(PROCESS), '%ENV', %ENV);

    my $VM = {
        name    => 'parrot', # XXX: should be made dynamical
        config  => pir::perl6ize_type__PP(
                        nqp::atpos(pir::getinterp, pir::const::IGLOBALS_CONFIG_HASH))
    }
    nqp::bindkey(pir::get_who__PP(PROCESS), '$VM', $VM);

    my $PERL          = {};
    my Mu $perl      := pir::find_caller_lex__PS('$COMPILER_CONFIG');
    my Mu $perl_iter := nqp::iterator($perl);
    while $perl_iter {
        $key        = nqp::p6box_s(pir::shift__SP($perl_iter));
        $PERL{$key} = nqp::p6box_s(nqp::atkey($perl, nqp::unbox_s($key)));
    }
    nqp::bindkey(pir::get_who__PP(PROCESS), '$PERL', $PERL);

    my $CWD = nqp::p6box_s(pir::new__PS('OS').cwd);
    nqp::bindkey(pir::get_who__PP(PROCESS), '$CWD', $CWD);

    my @INC;
    @INC.push('lib');
    @INC.push(%ENV<PERL6LIB>.split($VM<config><osname> eq 'MSWin32' ?? ';' !! ':')) if %ENV<PERL6LIB>;
    try {
        @INC.push((%ENV<HOME> // %ENV<HOMEDRIVE> ~ %ENV<HOMEPATH>) ~ '/.perl6/lib');
    }
    @INC.push($VM<config><libdir> ~ $VM<config><versiondir> ~ '/languages/perl6/lib');
    @INC.push('.'); # XXX: remove this when 'use lib' works fine
    nqp::bindkey(pir::get_who__PP(PROCESS), '@INC', @INC);

    my $PID = nqp::p6box_i(pir::getinterp.getpid());
    nqp::bindkey(pir::get_who__PP(PROCESS), '$PID', $PID);

    my $OS = $VM<config><osname>; # XXX: master gets this information with the sysinfo dynop
    nqp::bindkey(pir::get_who__PP(PROCESS), '$OS', $OS);

    my $OSVER = $VM<config><osvers>; # XXX: master gets this information with the sysinfo dynop
    nqp::bindkey(pir::get_who__PP(PROCESS), '$OSVER', $OSVER);

    my $EXECUTABLE_NAME = 
        nqp::p6box_s(pir::interpinfo__Si(pir::const::INTERPINFO_EXECUTABLE_FULLNAME));
    nqp::bindkey(pir::get_who__PP(PROCESS), '$EXECUTABLE_NAME', $EXECUTABLE_NAME);

    my $PROGRAM_NAME =
        nqp::p6box_s(
            nqp::atpos(
                nqp::atpos(
                    pir::getinterp, pir::const::IGLOBALS_ARGV_LIST
                ), 0
            )
        ) || 'interactive';
    nqp::bindkey(pir::get_who__PP(PROCESS), '$PROGRAM_NAME', $PROGRAM_NAME);

}
