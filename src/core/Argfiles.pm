multi sub INITIALIZE_DYNAMIC('@*ARGS') {
    my @ARGS;
    my Mu $argiter := nqp::getcurhllsym('$!ARGITER');
    @ARGS.push(nqp::p6box_s(nqp::shift($argiter))) while $argiter;
    PROCESS::<@ARGS> := @ARGS;
}
multi sub INITIALIZE_DYNAMIC('$*ARGFILES') {
    PROCESS::<$ARGFILES> = IO::ArgFiles.new(:args(@*ARGS));
}

# vim: ft=perl6 expandtab sw=4
