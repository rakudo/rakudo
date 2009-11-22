augment class Any {
    our Int multi method bytes() is export {
        pir::box__PI(pir::bytelength__IS(self))
    }

    our Int multi method chars() is export {
        pir::length__IS(self);
    }

    our Str multi method substr($start, $given_len?) is export {
        my $len = $given_len || self.chars;
        if ($len < 0) {
            if ($start >= 0) {
                $len += self.chars;
            }
            $len -= $start;
        }

        pir::substr(self, $start, $len);
    }

    our Str multi method chop() is export {
        self.substr(0, -1)
    }

    our Str multi method fmt(Str $format = '%s') {
        sprintf($format, self)
    }

    our Str multi method lc() is export {
        ~(pir::downcase__SS(self))
    }

    our Str multi method lcfirst() is export {
        self gt '' ?? self.substr(0,1).lc ~ self.substr(1) !! ""
    }

    our Int multi method ord() is export {
        fail('Can not take ord of empty string') if self.chars == 0;
        pir::box__PI(pir::ord__IS(self))
    }

    # TODO: Return type should be a Char once that is supported.
    our Str multi method p5chop() is export {
        my $char = '';

        for @.list -> $str is rw {
            if $str gt '' {
                $char = $str.substr($str.chars - 1, 1);
                $str  = $str.chop;
            }
        }

        $char
    }

    multi method flip() is export {
        (~self).split('').reverse().join;
    }

    our Str multi method uc() is export {
        ~(pir::upcase__SS(self))
    }

    our Str multi method ucfirst() is export {
        self gt '' ?? self.substr(0,1).uc ~ self.substr(1) !! ""
    }
}

multi sub infix:<x>($str, $n) {
    $n > 0 ?? ~(pir::repeat__SSI($str, $n)) !!  ''
}

# vim: ft=perl6
