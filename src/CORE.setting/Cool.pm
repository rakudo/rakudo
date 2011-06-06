my class Cool {
    method bytes() {
        pir::perl6_box_int__PI(
            pir::bytelength__IS(pir::repr_unbox_str__SP(self.Str)));
    }

    method chars() {
        pir::perl6_box_int__PI(
            pir::length__IS(pir::repr_unbox_str__SP(self.Str)));
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

        pir::perl6_box_str__PS(pir::substr(
            pir::repr_unbox_str__SP($str),
            pir::repr_unbox_int__IP($start),
            pir::repr_unbox_int__IP($len)));
    }

    method uc() {
        pir::perl6_box_str__PS(pir::upcase__SS(
             pir::repr_unbox_str__SP(self.Str)
         ));
    }

    method lc() {
        pir::perl6_box_str__PS(pir::downcase__SS(
             pir::repr_unbox_str__SP(self.Str)
         ));
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
        pir::perl6_box_int__PI(pir::ord__IS(pir::repr_unbox_str__SP(self.Str)));
    }

}
