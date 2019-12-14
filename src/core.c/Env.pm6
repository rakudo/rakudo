class Env is Hash {
    multi method new(Env:) {
        nqp::p6bindattrinvres(
          nqp::create(self),Map,'$!storage',nqp::clone(nqp::getenvhash)
        )
    }
    method AT-KEY(Env:D: Str:D $key) {
        nqp::existskey(nqp::getattr(self,Map,'$!storage'),$key)
          ?? val(nqp::atkey(nqp::getattr(self,Map,'$!storage'),$key))
          !! nextsame
    }
    method ASSIGN-KEY(Env:D: Str:D $key, \value) {
        nqp::bindkey(
          nqp::getattr(self,Map,'$!storage'),
          $key,
          nqp::unbox_s(value.Str)
        )
    }
}

Rakudo::Internals.REGISTER-DYNAMIC: '$*CWD', {
#    PROCESS::<$CWD> = nqp::p6box_s(nqp::cwd());
    my $CWD := nqp::p6box_s(nqp::cwd());
    PROCESS::<$CWD> = IO::Path.new($CWD, :$CWD); # need :CWD to prevent looping
}

# vim: ft=perl6 expandtab sw=4
