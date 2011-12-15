my class ObjAt {
    multi method WHICH(ObjAt:D:) {
        nqp::box_s(
            nqp::concat_s(
                nqp::concat_s(nqp::unbox_s(self.^name), '|'),
                $!value
            ),
            ObjAt
        );
    }
    multi method Str(ObjAt:D:) {
        nqp::p6box_s(nqp::unbox_s(self));
    }
    multi method gist(ObjAt:D:) {
        nqp::p6box_s(nqp::unbox_s(self));
    }
}
