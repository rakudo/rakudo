my class CallFrame {
    has $.annotations;
    has $.my;

    # cannot be a private method due to sub callframe
    method SET-SELF(
      \level, Mu \ctx is raw, Mu \bt is raw
    ) is implementation-detail {
        nqp::stmts(
          (my int $i = nqp::add_i(level,1)),
          ($!annotations := nqp::atkey(
            nqp::atpos(nqp::getattr(bt,List,'$!reified'),$i),
            'annotations'
          )),
          (my $ctx := ctx),
          nqp::while(
            nqp::isgt_i(($i = nqp::sub_i($i,1)),0),
            nqp::ifnull(
                ($ctx := nqp::ctxcallerskipthunks($ctx)),
                fail "No callframe at level {level}"
            )
          ),
          ($!my :=
            nqp::p6bindattrinvres(nqp::create(Stash),Map,'$!storage',$ctx)),
          self
        )
    }

    only method new(CallFrame: Int:D $level = 0) {  # MUST BE AN only
        nqp::create(CallFrame).SET-SELF(            # wrt to backtrace levels
          $level,
          nqp::ctxcallerskipthunks(nqp::ctx),
          nqp::backtrace(nqp::handle(nqp::die(''),'CATCH',nqp::exception))
        )
    }

    method line() { nqp::atkey($!annotations,'line') }
    method file() { nqp::atkey($!annotations,'file') }
    method code() {
        my \vm-code = nqp::ctxcode(nqp::getattr($!my,Map,'$!storage'));
        nqp::isnull(vm-code) ?? Nil !! nqp::getcodeobj(vm-code)
    }
    method callframe(Int:D $?) {
        X::NYI.new(feature => 'Callframe.callframe').throw;
    }

    multi method gist(CallFrame:D:) {
        nqp::atkey($!annotations,'file')
          ~ ' at line '
          ~ nqp::atkey($!annotations,'line')
    }

    method annotations() {
        nqp::p6bindattrinvres(nqp::create(Map),Map,'$!storage',$!annotations)
    }
}

only sub callframe(Int:D $level = 0) { # MUST BE an only wrt to backtrace levels
    nqp::create(CallFrame).SET-SELF(
      $level,
      nqp::ctxcallerskipthunks(nqp::ctx),
      nqp::backtrace(nqp::handle(nqp::die(''),'CATCH',nqp::exception))
    )
}

# vim: expandtab shiftwidth=4
