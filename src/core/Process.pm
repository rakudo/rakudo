{
    PROCESS::<$PID> := nqp::p6box_i(nqp::getpid());

    my $EXECUTABLE =
#?if parrot
        nqp::p6box_s(pir::interpinfo__Si(pir::const::INTERPINFO_EXECUTABLE_FULLNAME));
#?endif
#?if jvm
        $*VM.properties<perl6.execname>
        or $*VM.properties<perl6.prefix> ~ '/bin/perl6-j';
#?endif
#?if moar
        nqp::execname()
        or ($*VM.config<prefix> ~ '/bin/' ~ ($*VM.config<osname> eq 'MSWin32' ?? 'perl6-m.bat' !! 'perl6-m'));
#?endif
    $EXECUTABLE := $EXECUTABLE.path.absolute;
    PROCESS::<$EXECUTABLE>      = $EXECUTABLE;
    PROCESS::<$EXECUTABLE_NAME> = $EXECUTABLE.basename;

    my Mu $comp := nqp::getcomp('perl6');
    my $PROGRAM_NAME = $comp.user-progname();
    PROCESS::<$PROGRAM_NAME> = $PROGRAM_NAME;

    PROCESS::<$TMPDIR> = IO::Spec.tmpdir().path;
}

# vim: ft=perl6 expandtab sw=4
