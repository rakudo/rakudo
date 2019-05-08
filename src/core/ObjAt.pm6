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
    multi method perl(ObjAt:D:) {
        self.^name ~ ".new(" ~ nqp::p6box_s(nqp::unbox_s(self)).perl ~ ")"
    }
}

multi sub infix:<eqv>(ObjAt:D $a, ObjAt:D $b) {
    nqp::hllbool(
      nqp::eqaddr($a.WHAT,$b.WHAT)
        && nqp::iseq_s(nqp::unbox_s($a),nqp::unbox_s($b))
    )
}

# vim: ft=perl6 expandtab sw=4
