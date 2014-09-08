{
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
    PROCESS::<%ENV> := %ENV;
}

multi sub INITIALIZE('$*CWD') {
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
    PROCESS::<$CWD> := $CWD;
}

# vim: ft=perl6 expandtab sw=4
