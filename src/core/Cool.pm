my class Cool {
    method bytes() {
        nqp::p6box_i(pir::bytelength__IS(nqp::unbox_s(self.Str)));
    }

    method chars() {
        nqp::p6box_i(nqp::chars(nqp::unbox_s(self.Str)));
    }

    method substr($start as Int, $length?) {
        my Str $str := self.Str;
        my Int $len := ($length // $str.chars).Int;
        if ($len < 0) {
            if ($start >= 0) {
                $len := $len + $str.chars;
            }
            $len := $len - $start;
        }

        # XXX no prefix:<-> yet...
        #if ($start > self.chars || $start < -self.chars) {
        #    return Mu;
        #}

        nqp::p6box_s(pir::substr(
            nqp::unbox_s($str),
            nqp::unbox_i($start),
            nqp::unbox_i($len)));
    }

    method uc() {
        nqp::p6box_s(nqp::uc(nqp::unbox_s(self.Str)))
    }

    method lc() {
        nqp::p6box_s(nqp::lc(nqp::unbox_s(self.Str)))
    }

    method ucfirst() {
        my $self-str = self.Str;
        $self-str eq '' ?? '' !! $self-str.substr(0, 1).uc ~ $self-str.substr(1)
    }

    method lcfirst() {
        my $self-str = self.Str;
        $self-str eq '' ?? '' !! $self-str.substr(0, 1).lc ~ $self-str.substr(1)
    }

    method ord() {
        nqp::p6box_i(nqp::ord(nqp::unbox_s(self.Str)))
    }

    method flip() {
        nqp::p6box_s(pir::box__PS(nqp::unbox_s(self.Str)).reverse)
    }

    proto method index(|$) {*}
    multi method index(Cool \$needle, Cool $pos = 0) {
        my $result := nqp::p6box_i(nqp::index(
                nqp::unbox_s(self.Str),
                nqp::unbox_s($needle.Str),
                nqp::unbox_i($pos.Int)
        ));
        # TODO: fail() instead of returning Str
        $result < 0 ?? Str !! $result;
    }

    proto method rindex(|$) {*}
    multi method rindex(Cool \$needle, Cool $pos = self.chars) {
        my $result := nqp::p6box_i(
                pir::box__PS(nqp::unbox_s(self.Str)).reverse_index(
                    nqp::unbox_s($needle.Str),
                    nqp::unbox_i($pos.Int)
                )
        );
        # TODO: fail() instead of returning Str
        $result < 0 ?? Str !! $result;
    }
}
