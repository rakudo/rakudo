Rakudo::Internals.REGISTER-DYNAMIC: '$*CWD', {
    my $CWD := nqp::p6box_s(nqp::cwd());
    PROCESS::<$CWD> = IO::Path.new($CWD, :$CWD); # need :CWD to prevent looping
}

Rakudo::Internals.REGISTER-DYNAMIC: '%*ENV', {
    my $env := nqp::hash;
    my $iter := nqp::iterator(nqp::getenvhash);
    nqp::while(
      $iter,
      nqp::bindkey(
        $env,
        nqp::iterkey_s(nqp::shift($iter)),
        nqp::assign(
          nqp::p6scalarfromdesc(nqp::null),
          val(nqp::box_s(nqp::iterval($iter),Str))
        )
      )
    );
    PROCESS::<%ENV> := nqp::p6bindattrinvres(nqp::create(Hash),Map,'$!storage',$env)
}

# vim: expandtab shiftwidth=4
