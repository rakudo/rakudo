{
    my $CWD := nqp::p6box_s(nqp::cwd());
    PROCESS::<$CWD> = IO::Path.new($CWD, :$CWD); # need :CWD to prevent looping
}

# vim: expandtab shiftwidth=4
