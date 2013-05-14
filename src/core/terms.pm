sub term:<time>() { nqp::p6box_i(nqp::time_i()) }

{
    my @ARGS;
    my Mu $argiter := nqp::getcurhllsym('$!ARGITER');
    @ARGS.push(nqp::p6box_s(nqp::shift($argiter))) while $argiter;
    nqp::bindkey(nqp::who(PROCESS), '@ARGS', @ARGS);
    $PROCESS::ARGFILES = IO::ArgFiles.new(:args(@ARGS));

    my %ENV;
    my Mu $env := nqp::getenvhash();
    my Mu $enviter := nqp::iterator($env);
    my $envelem;
    my $key;
    while $enviter {
        $envelem := nqp::shift($enviter);
        $key = nqp::p6box_s(nqp::iterkey_s($envelem));
        %ENV{$key} = nqp::p6box_s(nqp::iterval($envelem));
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
    nqp::bindkey(nqp::who(PROCESS), '%ENV', %ENV);

    my $VM = {
#?if parrot
        name    => 'parrot', # XXX: should be made dynamical
        config  => nqp::hllize(
                        nqp::atpos(pir::getinterp__P, pir::const::IGLOBALS_CONFIG_HASH))
#?endif
#?if jvm
        name    => 'jvm'
#?endif
    }
    nqp::bindkey(nqp::who(PROCESS), '$VM', $VM);

    my Mu $compiler := nqp::getlexcaller('$COMPILER_CONFIG');
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
    nqp::bindkey(nqp::who(PROCESS), '$PERL', $PERL);

#?if parrot
    my $CWD = nqp::p6box_s(pir::new__PS('OS').cwd);
    nqp::bindkey(nqp::who(PROCESS), '$CWD', $CWD);
#?endif
    
    my @INC;
    @INC.push(%ENV<RAKUDOLIB>.split($VM<config><osname> eq 'MSWin32' ?? ';' !! ':')) if %ENV<RAKUDOLIB>;
    @INC.push(%ENV<PERL6LIB>.split($VM<config><osname> eq 'MSWin32' ?? ';' !! ':')) if %ENV<PERL6LIB>;
    my $prefix := $VM<config><libdir> ~ $VM<config><versiondir> ~ '/languages/perl6';
    my %CUSTOM_LIB;

    %CUSTOM_LIB<perl>   = $prefix;
    %CUSTOM_LIB<vendor> = $prefix ~ '/vendor';
    %CUSTOM_LIB<site>   = $prefix ~ '/site';
    @INC.push(%CUSTOM_LIB<site>   ~ '/lib');
    @INC.push(%CUSTOM_LIB<vendor> ~ '/lib');
    @INC.push(%CUSTOM_LIB<perl>   ~ '/lib');
    try {
        my $home := %ENV<HOME> // %ENV<HOMEDRIVE> ~ %ENV<HOMEPATH>;
        my $ver  := nqp::p6box_s(nqp::atkey($compiler, 'version'));
        %CUSTOM_LIB<home> = "$home/.perl6/$ver";
        @INC.push(%CUSTOM_LIB<home> ~ '/lib');
    }
    nqp::bindkey(nqp::who(PROCESS), '%CUSTOM_LIB', %CUSTOM_LIB);

    my $I := nqp::atkey(nqp::atkey(%*COMPILING, '%?OPTIONS'), 'I');
    if nqp::defined($I) {
        if nqp::islist($I) {
            my Mu $iter := nqp::iterator($I);
            @INC.unshift: nqp::p6box_s(nqp::shift($iter)) while $iter;
        }
        else {
            @INC.unshift: nqp::p6box_s($I);
        }
    }

    nqp::bindkey(nqp::who(PROCESS), '@INC', @INC);

#?if parrot
    my $PID = nqp::p6box_i(pir::getinterp__P().getpid());
    nqp::bindkey(nqp::who(PROCESS), '$PID', $PID);
#?endif

    my $OS = $VM<config><osname>; # XXX: master gets this information with the sysinfo dynop
    nqp::bindkey(nqp::who(PROCESS), '$OS', $OS);

    my $OSVER = $VM<config><osvers>; # XXX: master gets this information with the sysinfo dynop
    nqp::bindkey(nqp::who(PROCESS), '$OSVER', $OSVER);

    my $EXECUTABLE_NAME = 
#?if parrot
        nqp::p6box_s(pir::interpinfo__Si(pir::const::INTERPINFO_EXECUTABLE_FULLNAME));
#?endif
#?if jvm
        'java';
#?endif
    nqp::bindkey(nqp::who(PROCESS), '$EXECUTABLE_NAME', $EXECUTABLE_NAME);
    my Mu $comp := nqp::getcomp('perl6');

    my $PROGRAM_NAME = $comp.user-progname();
    nqp::bindkey(nqp::who(PROCESS), '$PROGRAM_NAME', $PROGRAM_NAME);

    $PROCESS::TMPDIR = IO::Spec.tmpdir();


}
