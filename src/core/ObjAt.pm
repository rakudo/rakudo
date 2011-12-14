my class ObjAt {
    multi method Str(ObjAt:D:) {
        nqp::p6box_s(nqp::unbox_s(self));
    }
    multi method gist(ObjAt:D:) {
        nqp::p6box_s(nqp::unbox_s(self));
    }
}
