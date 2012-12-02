sub term:<time>() { nqp::p6box_i(nqp::time_i()) }

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
        $key = nqp::p6box_s(nqp::shift_s($enviter));
        %ENV{$key} = nqp::p6box_s(nqp::atkey($env, nqp::unbox_s($key)));
    }
    %ENV does role {
        method at_key($k) {
            Proxy.new(
                    FETCH => {
                        if nqp::p6bool(nqp::existskey($env, nqp::unbox_s($k))) {
                            nqp::p6box_s(nqp::atkey($env, nqp::unbox_s($k)))
                        }
                        else {
                            Any
                        }
                    },
                    STORE => -> $, $v {
                        nqp::bindkey($env, nqp::unbox_s($k),
                            nqp::unbox_s(($v // '').Str))
                    }
            )
        }

        method delete($k) {
            my $ret = self.at_key($k);
            nqp::deletekey($env, nqp::unbox_s($k));
            return $ret;
        }
    }
    nqp::bindkey(pir::get_who__PP(PROCESS), '%ENV', %ENV);

    my $VM = {
        name    => 'parrot', # XXX: should be made dynamical
        config  => pir::perl6ize_type__PP(
                        nqp::atpos(pir::getinterp__P, pir::const::IGLOBALS_CONFIG_HASH))
    }
    nqp::bindkey(pir::get_who__PP(PROCESS), '$VM', $VM);

    my Mu $compiler := pir::find_caller_lex__PS('$COMPILER_CONFIG');
    my $PERL = {
        name => 'rakudo',
        compiler => {
            name => 'rakudo',
            ver => nqp::p6box_s(nqp::atkey($compiler, 'version')),
            release-number => nqp::p6box_s(nqp::atkey($compiler, 'release-number')),
            build-date => nqp::p6box_s(nqp::atkey($compiler, 'build-date')),
            codename => nqp::p6box_s(nqp::atkey($compiler, 'codename')),
        }
    };
    nqp::bindkey(pir::get_who__PP(PROCESS), '$PERL', $PERL);

    my $CWD = nqp::p6box_s(pir::new__PS('OS').cwd);
    nqp::bindkey(pir::get_who__PP(PROCESS), '$CWD', $CWD);

    my @INC;
    @INC.push(%ENV<RAKUDOLIB>.split($VM<config><osname> eq 'MSWin32' ?? ';' !! ':')) if %ENV<RAKUDOLIB>;
    @INC.push(%ENV<PERL6LIB>.split($VM<config><osname> eq 'MSWin32' ?? ';' !! ':')) if %ENV<PERL6LIB>;
    my $prefix := $VM<config><libdir> ~ $VM<config><versiondir> ~ '/languages/perl6';
    my %CUSTOM_LIB;

    %CUSTOM_LIB<perl>   = $prefix;
    %CUSTOM_LIB<vendor> = $prefix ~ '/vendor';
    %CUSTOM_LIB<site>   = $prefix ~ '/site';
    @INC.push(%CUSTOM_LIB<perl>   ~ '/lib');
    @INC.push(%CUSTOM_LIB<vendor> ~ '/lib');
    @INC.push(%CUSTOM_LIB<site>   ~ '/lib');
    try {
        my $home := %ENV<HOME> // %ENV<HOMEDRIVE> ~ %ENV<HOMEPATH>;
        my $ver  := nqp::p6box_s(nqp::atkey($compiler, 'version'));
        %CUSTOM_LIB<home> = "$home/.perl6/$ver";
        @INC.push(%CUSTOM_LIB<home> ~ '/lib');
    }
    nqp::bindkey(pir::get_who__PP(PROCESS), '%CUSTOM_LIB', %CUSTOM_LIB);

    my $I := nqp::atkey(nqp::atkey(%*COMPILING, '%?OPTIONS'), 'I');
    if nqp::defined($I) {
        if pir::does__IPs($I, 'array') {
            my Mu $iter := nqp::iterator($I);
            @INC.unshift: nqp::p6box_s(nqp::shift($iter)) while $iter;
        }
        else {
            @INC.unshift: nqp::p6box_s($I);
        }
    }

    nqp::bindkey(pir::get_who__PP(PROCESS), '@INC', @INC);

    my $PID = nqp::p6box_i(pir::getinterp__P().getpid());
    nqp::bindkey(pir::get_who__PP(PROCESS), '$PID', $PID);

    my $OS = $VM<config><osname>; # XXX: master gets this information with the sysinfo dynop
    nqp::bindkey(pir::get_who__PP(PROCESS), '$OS', $OS);

    my $OSVER = $VM<config><osvers>; # XXX: master gets this information with the sysinfo dynop
    nqp::bindkey(pir::get_who__PP(PROCESS), '$OSVER', $OSVER);

    my $EXECUTABLE_NAME = 
        nqp::p6box_s(pir::interpinfo__Si(pir::const::INTERPINFO_EXECUTABLE_FULLNAME));
    nqp::bindkey(pir::get_who__PP(PROCESS), '$EXECUTABLE_NAME', $EXECUTABLE_NAME);
    my Mu $comp := pir::compreg__Ps('perl6');

    my $PROGRAM_NAME = $comp.user-progname();
    nqp::bindkey(pir::get_who__PP(PROCESS), '$PROGRAM_NAME', $PROGRAM_NAME);

}
