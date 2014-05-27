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

# vim: ft=perl6 expandtab sw=4
