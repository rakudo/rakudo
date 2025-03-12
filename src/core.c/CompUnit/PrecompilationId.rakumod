class CompUnit::PrecompilationId {
    has Str $.id is built(False) handles <Str IO substr>;

    method new(Str $id --> CompUnit::PrecompilationId:D) {
        nqp::atpos(nqp::radix_I(16,$id,0,0,Int),2) == 40
          ?? nqp::p6bindattrinvres(nqp::create(self),
               CompUnit::PrecompilationId,'$!id',$id)
          !! die "Invalid precompilation id: '$id'"
    }

    method new-from-string(Str $id --> CompUnit::PrecompilationId:D) {
        nqp::p6bindattrinvres(nqp::create(self),
          CompUnit::PrecompilationId,'$!id',nqp::sha1($id))
    }

    method new-without-check(Str $id --> CompUnit::PrecompilationId:D) {
        nqp::p6bindattrinvres(nqp::create(self),
          CompUnit::PrecompilationId,'$!id',$id)
    }

    multi method WHICH(CompUnit::PrecompilationId:D: --> ValueObjAt:D) {
        nqp::box_s(
          nqp::concat('CompUnit::PrecompilationId|',$!id),
          ValueObjAt
        )
    }
}

# vim: expandtab shiftwidth=4
