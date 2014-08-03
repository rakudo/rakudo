#!nqp
# Copyright (C) 2014, The Perl Foundation.


sub MAIN(*@ARGS) {
    @ARGS.shift;        # Remove program name
    my $existing := @ARGS.shift;
    $existing := "$existing.bat" unless nqp::stat($existing, nqp::const::STAT_EXISTS);
    nqp::die("Could not find $existing\n") unless nqp::stat($existing, nqp::const::STAT_EXISTS);

    my $fh := open($existing, :r);
    my $runner;
    while my $_ := nqp::readlinefh($fh) {
        $runner := $_;
    }
    close($fh);

    $runner := subst($runner, /'nqp-runtime.jar;'/, 'nqp-runtime.jar;rakudo-runtimer.jar;');
    $runner := subst($runner, /'nqp-runtime.jar:'/, 'nqp-runtime.jar:rakudo-runtimer.jar;');

    my $args := join(' ', @ARGS);

    $runner := subst($runner, /'"$@"'/, $args);
    $runner := subst($runner, /'%*'/, $args);

    nqp::shell($runner, nqp::cwd(), nqp::getenvhash());
}
