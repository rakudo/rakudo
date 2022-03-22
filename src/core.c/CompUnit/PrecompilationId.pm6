class CompUnit::PrecompilationId {
    has $.id;

    method new(str $id --> CompUnit::PrecompilationId:D) {
        nqp::atpos(nqp::radix_I(16,$id,0,0,Int),2) == 40
          ?? nqp::p6bindattrinvres(nqp::create(self),
               CompUnit::PrecompilationId,'$!id',$id)
          !! die "Invalid precompilation id: '$id'"
    }

    method new-from-string(str $id --> CompUnit::PrecompilationId:D) {
        nqp::p6bindattrinvres(nqp::create(self),
          CompUnit::PrecompilationId,'$!id',nqp::sha1($id))
    }

    method new-without-check(str $id --> CompUnit::PrecompilationId:D) {
        nqp::p6bindattrinvres(nqp::create(self),
          CompUnit::PrecompilationId,'$!id',$id)
    }

    multi method WHICH(CompUnit::PrecompilationId:D: --> ValueObjAt:D) {
        nqp::box_s(
          nqp::concat('CompUnit::PrecompilationId|',$!id),
          ValueObjAt
        )
    }

    method Str()      { $!id }
    method IO()       { $!id.IO }
    method substr(|c) { $!id.substr(|c) }
}

# vim: expandtab shiftwidth=4
