augment class Any {
    our Int multi method bytes() is export {
        pir::box__PI(pir::bytelength__IS(self))
    }

    our Int multi method chars() is export {
        pir::length__IS(self);
    }

    multi method comb(Regex $matcher, $limit = *, :$match) {
        my $c = 0;
        # XXX this is an ugly hack. I'd prefer $limit ~~ Whatever,
        # but the Whatever type object isn't available yet,
        # and trying to introduce it leads to strainge MDD-related
        # bugs
        my $l = $limit.WHAT eq 'Whatever()' ?? Inf !! $limit;
        gather while $l > 0 && (my $m = self.match($matcher, :c($c))) {
            take $match ?? $m !! ~$m;
            $c = $m.to == $c ?? $c + 1 !! $m.to;
            --$l;
        }
    }

    our Str multi method substr($start, $length?) is export {
        my $len = $length // self.chars;
        if ($len < 0) {
            if ($start >= 0) {
                $len += self.chars;
            }
            $len -= $start;
        }

        pir::substr(self, $start, $len);
    }

    our Int multi method index($substring, $pos = 0) is export {
        if ($substring.chars == 0) {
            my $string_length = self.chars;
            return $pos < $string_length ?? $pos !! $string_length;
        }

        my $result = pir::index__ISSi(self, $substring, $pos);
        fail("Substring '$substring' not found in '{self}'") if ($result < 0);
        return $result;

        # also used to be a the following error message, but the condition
        # was never checked:
        # .tailcall '!FAIL'("Attempt to index from negative position")
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

    our multi method match(Regex $pat, :$c = 0) {
        Regex::Cursor.parse(self, :rule($pat), :c($c));
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
