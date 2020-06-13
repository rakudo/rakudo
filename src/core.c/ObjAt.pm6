my class ObjAt { # declared in BOOTSTRAP
    # class ObjAt is Any
    #     has str $!value;
    method new(str $s) {
        nqp::box_s($s, self.WHAT)
    }

    multi method WHICH(ObjAt:D: --> ObjAt:D) {
        nqp::box_s(
          nqp::concat(
            nqp::if(
              nqp::eqaddr(self.WHAT,ObjAt),
              'ObjAt|',
              nqp::concat(nqp::unbox_s(self.^name), '|')
            ),
            $!value
          ),
          self.WHAT
        )
    }
    multi method Str(ObjAt:D:) {
        nqp::p6box_s(nqp::unbox_s(self));
    }
    multi method gist(ObjAt:D:) {
        nqp::p6box_s(nqp::unbox_s(self));
    }
    multi method raku(ObjAt:D:) {
        self.^name ~ ".new(" ~ nqp::p6box_s(nqp::unbox_s(self)).raku ~ ")"
    }
}

multi sub infix:<eqv>(ObjAt:D \a, ObjAt:D \b --> Bool:D) {
    nqp::hllbool(
      nqp::eqaddr(nqp::decont(a),nqp::decont(b))
        || (nqp::eqaddr(a.WHAT,b.WHAT) && nqp::iseq_s(a,b))
    )
}

# vim: expandtab shiftwidth=4
