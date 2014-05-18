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
    nqp::bindkey(nqp::who(PROCESS), '%ENV', %ENV);

#?if parrot
    my $VM = {
        name    => 'parrot', # XXX: should be made dynamical
        config  => nqp::hllize(
                        nqp::atpos(pir::getinterp__P, pir::const::IGLOBALS_CONFIG_HASH))
    }
#?endif
#?if jvm
    my %PROPS;
    my $jenv := nqp::jvmgetproperties();
    $enviter := nqp::iterator($jenv);
    while $enviter {
        $envelem := nqp::shift($enviter);
        $key = nqp::p6box_s(nqp::iterkey_s($envelem));
        %PROPS{$key} = nqp::p6box_s(nqp::iterval($envelem));
    }
    my %CONFIG;
    $jenv := nqp::backendconfig();
    $enviter := nqp::iterator($jenv);
    while $enviter {
        $envelem := nqp::shift($enviter);
        $key = nqp::p6box_s(nqp::iterkey_s($envelem));
        %CONFIG{$key} = nqp::p6box_s(nqp::iterval($envelem));
    }
    my $VM = {
        name    => 'jvm',
        properties => %PROPS,
        config => %CONFIG,
    }
#?endif
#?if moar
    my $VM = {
        name    => 'moar',
        config  => nqp::backendconfig
    }
#?endif
    nqp::bindkey(nqp::who(PROCESS), '$VM', $VM);

    my @INC;
#?if jvm
    my $pathsep := $VM<properties><path.separator>;
#?endif
#?if !jvm
    my $pathsep := $VM<config><osname> eq 'MSWin32' ?? ';' !! ':';
#?endif
    @INC.push(%ENV<RAKUDOLIB>.split($pathsep)) if %ENV<RAKUDOLIB>;
    @INC.push(%ENV<PERL6LIB>.split($pathsep)) if %ENV<PERL6LIB>;
    
#?if jvm
    for nqp::jvmclasspaths() -> $path {
        @INC.push($path) if nqp::stat($path, nqp::const::STAT_ISDIR);
    }
#?endif    

    my $prefix :=
#?if jvm
        $VM<properties><perl6.prefix>
#?endif
#?if parrot
        $VM<config><libdir> ~ $VM<config><versiondir>
#?endif
#?if moar
        $VM<config><prefix>
#?endif
         ~ '/languages/perl6';

# XXX Various issues with this stuff on JVM
    my Mu $compiler := nqp::getcurhllsym('$COMPILER_CONFIG');  # TEMPORARY
    my %CUSTOM_LIB;
    try {
        my $home := %ENV<HOME> // %ENV<HOMEDRIVE> ~ %ENV<HOMEPATH>;
        my $ver  := nqp::p6box_s(nqp::atkey($compiler, 'version'));
        %CUSTOM_LIB<home> = "$home/.perl6/$ver";
        @INC.push(%CUSTOM_LIB<home> ~ '/lib');
    }
    %CUSTOM_LIB<perl>   = $prefix;
    %CUSTOM_LIB<vendor> = $prefix ~ '/vendor';
    %CUSTOM_LIB<site>   = $prefix ~ '/site';
    @INC.push(%CUSTOM_LIB<site>   ~ '/lib');
    @INC.push(%CUSTOM_LIB<vendor> ~ '/lib');
    @INC.push(%CUSTOM_LIB<perl>   ~ '/lib');

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

    ## duplicate src/core/IO.pm::cwd
    my $CWD = IO::Path.new(nqp::p6box_s(
#?if parrot
        pir::trans_encoding__Ssi(
            nqp::cwd(),
            pir::find_encoding__Is('utf8'))
#?endif
#?if !parrot
            nqp::cwd(),
#?endif
    ));

    nqp::bindkey(nqp::who(PROCESS), '$CWD', $CWD);

#?if jvm
    my $OS = $VM<properties><os.name>;
    nqp::bindkey(nqp::who(PROCESS), '$OS', $OS);

    my $OSVER = $VM<properties><os.version>;
    nqp::bindkey(nqp::who(PROCESS), '$OSVER', $OSVER);
#?endif
#?if !jvm
    my $OS = $VM<config><osname>; # XXX: master gets this information with the sysinfo dynop on parrot
    nqp::bindkey(nqp::who(PROCESS), '$OS', $OS);

    my $OSVER = $VM<config><osname>; # XXX: master gets this information with the sysinfo dynop on parrot
    nqp::bindkey(nqp::who(PROCESS), '$OSVER', $OSVER);
#?endif

    my $PID = nqp::p6box_i(nqp::getpid());
    nqp::bindkey(nqp::who(PROCESS), '$PID', $PID);

    my $EXECUTABLE =
#?if parrot
        nqp::p6box_s(pir::interpinfo__Si(pir::const::INTERPINFO_EXECUTABLE_FULLNAME));
#?endif
#?if jvm
        $VM<properties><perl6.execname> or $VM<properties><perl6.prefix> ~ '/bin/perl6-j';
#?endif
#?if moar
        nqp::execname()
        or ($VM<config><prefix> ~ '/bin/' ~ ($VM<config><osname> eq 'MSWin32' ?? 'perl6-m.bat' !! 'perl6-m'));
#?endif
    $EXECUTABLE := $EXECUTABLE.path.absolute;
    PROCESS.WHO<$EXECUTABLE>      = $EXECUTABLE;
    PROCESS.WHO<$EXECUTABLE_NAME> = $EXECUTABLE.basename;

    my Mu $comp := nqp::getcomp('perl6');

    my $PROGRAM_NAME = $comp.user-progname();
    nqp::bindkey(nqp::who(PROCESS), '$PROGRAM_NAME', $PROGRAM_NAME);

    $PROCESS::TMPDIR = IO::Spec.tmpdir().path;
}
