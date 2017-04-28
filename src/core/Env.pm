{
    my %ENV;
    my Mu $env := nqp::getenvhash();
    my Mu $enviter := nqp::iterator($env);
    my $key;
    while $enviter {
        $key = nqp::p6box_s(nqp::iterkey_s(nqp::shift($enviter)));
        %ENV{$key} = val(nqp::p6box_s(nqp::iterval($enviter)));
    }
    PROCESS::<%ENV> := %ENV;
}

Rakudo::Internals.REGISTER-DYNAMIC: '$*CWD', {
#    PROCESS::<$CWD> = nqp::p6box_s(nqp::cwd());
    my $CWD := nqp::p6box_s(nqp::cwd());
    PROCESS::<$CWD> = IO::Path.new($CWD, :$CWD); # need :CWD to prevent looping
}

# vim: ft=perl6 expandtab sw=4
