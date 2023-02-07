augment class Int {
    multi method sqrt(Int:D:) is default {
        nqp::islt_I(self,0)
          ?? Complex.new(
               0,
               nqp::p6box_n(nqp::sqrt_n(nqp::abs_n(nqp::tonum_I(self))))
             )
          !! nqp::p6box_n(nqp::sqrt_n(nqp::tonum_I(self)))
    }
}

augment class Num {
    multi method log(Num:D:) is default {
        nqp::islt_n(self,0e0)
          ?? Complex.new(
               nqp::p6box_n(nqp::log_n(nqp::abs_n(nqp::unbox_n(self)))),
               pi
             )
          !! nqp::p6box_n(nqp::log_n(nqp::unbox_n(self)));
    }

    multi method sqrt(Num:D:) is default {
        nqp::islt_n(self,0e0)
          ?? Complex.new(
               0,
               nqp::p6box_n(nqp::sqrt_n(nqp::abs_n(nqp::unbox_n(self))))
             )
          !! nqp::p6box_n(nqp::sqrt_n(nqp::unbox_n(self)));
    }
}

augment class Complex {
    method sign(Complex:D: --> Complex:D) {
        $_ == 0 ?? 0i !! self / $_ given self.abs;
    }
}

# vim: expandtab shiftwidth=4
