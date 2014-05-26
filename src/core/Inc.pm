{
    my @INC;
#?if jvm
    my $pathsep := $*VM.properties<path.separator>;
#?endif
#?if !jvm
    my $pathsep := $*VM.config<osname> eq 'MSWin32' ?? ';' !! ':';
#?endif
    @INC.push(%*ENV<RAKUDOLIB>.split($pathsep)) if %*ENV<RAKUDOLIB>;
    @INC.push(%*ENV<PERL6LIB>.split($pathsep)) if %*ENV<PERL6LIB>;
    
#?if jvm
    for nqp::jvmclasspaths() -> $path {
        @INC.push($path) if nqp::stat($path, nqp::const::STAT_ISDIR);
    }
#?endif    

    my $prefix :=
#?if jvm
        $*VM.properties<perl6.prefix>
#?endif
#?if parrot
        $*VM.config<libdir> ~ $*VM.config<versiondir>
#?endif
#?if moar
        $*VM.config<prefix>
#?endif
         ~ '/languages/perl6';

# XXX Various issues with this stuff on JVM
    my Mu $compiler := nqp::getcurhllsym('$COMPILER_CONFIG');  # TEMPORARY
    my %CUSTOM_LIB;
    try {
        my $home := %*ENV<HOME> // %*ENV<HOMEDRIVE> ~ %*ENV<HOMEPATH>;
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
    PROCESS::<%CUSTOM_LIB> := %CUSTOM_LIB;

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
    PROCESS::<@INC> := @INC;
}
