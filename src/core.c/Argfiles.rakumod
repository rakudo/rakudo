Rakudo::Internals.REGISTER-DYNAMIC: '@*ARGS', {
    my @ARGS;
    my Mu $argiter := nqp::getcurhllsym('$!ARGITER');
    @ARGS.push(nqp::p6box_s(nqp::shift($argiter))) while $argiter;
    PROCESS::<@ARGS> := @ARGS;
}
Rakudo::Internals.REGISTER-DYNAMIC: '$*ARGFILES', {
    PROCESS::<$ARGFILES> = do if @*ARGS -> @ARGS {
        if %*SUB-MAIN-OPTS<dash-as-STDIN> {
            $_ = $*IN if $_ eq '-' for @ARGS;
        }
        IO::ArgFiles.new(@ARGS)
    }
    else {
        $*IN
    }
}

# vim: expandtab shiftwidth=4
