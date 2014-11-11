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

multi sub INITIALIZE_DYNAMIC('$*CWD') {
#    PROCESS::<$CWD> = nqp::p6box_s(nqp::cwd());
    my $CWD := nqp::p6box_s(nqp::cwd()) ~ '/';
    PROCESS::<$CWD> = IO::Dir.new(:abspath($CWD));
}

# vim: ft=perl6 expandtab sw=4
