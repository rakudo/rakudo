augment class Any {
    our Int multi method bytes() is export {
        pir::box__PI(pir::bytelength__IS(self))
    }

    our Int multi method chars() is export {
        pir::length__IS(self);
    }

    our multi method chomp() is export {
        if self ~~ /\x0a$/ {
            self.substr(0, self.chars - 1);
        } else {
            self;
        }
    }

    multi method subst(Regex $matcher, Str $replacement) {
        self.split($matcher).join($replacement);
    }

    multi method comb(Regex $matcher = /./, $limit = *, :$match) {
        my $c = 0;
        my $l = $limit ~~ ::Whatever ?? Inf !! $limit;
        gather while $l > 0 && (my $m = self.match($matcher, :c($c))) {
            take $match ?? $m !! ~$m;
            $c = $m.to == $c ?? $c + 1 !! $m.to;
            --$l;
        }
    }

    multi method split(Regex $matcher, $limit = *, :$all) {
        my $c = 0;
        my $l = $limit ~~ ::Whatever ?? Inf !! $limit - 1;
        if $l >= 0 {
            gather {
                while $l-- > 0 && (my $m = self.match($matcher, :c($c))) {
                    take self.substr($c, $m.from - $c);
                    take $m if $all;
                    $c = $m.to == $c ?? $c + 1 !! $m.to;
                }
                take self.substr($c);
            }
        } else {
            Nil;
        }
    }

    multi method split($delimiter, $limit = *) {
        my Str $match-string = $delimiter ~~ Str ?? $delimiter !! $delimiter.Str;
        my $c = 0;
        my $l = $limit ~~ ::Whatever ?? Inf !! $limit - 1;
        if $l >= 0 {
            gather {
                while $l-- > 0 {
                    if ($match-string eq "") {
                        last unless $c + 1 < self.chars;
                        take self.substr($c, 1);
                        $c++;
                    } else {
                        my $m = self.index($match-string, $c);
                        last if $m.notdef; # CHEAT, but the best I can do for now
                        take self.substr($c, $m - $c);
                        $c = $m + $match-string.chars;
                    }
                }
                take self.substr($c);
            }
        } else {
            Nil;
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

    # S32/Str says that this should always return a StrPos object
    our Int multi method index($substring, $pos = 0) is export {
        if ($substring.chars == 0) {
            my $string_length = self.chars;
            return $pos < $string_length ?? $pos !! $string_length;
        }

        my $result = pir::index__ISSi(self, $substring, $pos);
        fail("Substring '$substring' not found in '{self}'") if $result < 0;
        return $result;

        # also used to be a the following error message, but the condition
        # was never checked:
        # .tailcall '!FAIL'("Attempt to index from negative position")
    }

    # S32/Str says that this should always return a StrPos object
    # our Int multi method rindex($substring, $pos?) is export {
    #     if ($substring.chars == 0) {
    #         my $string_length = self.chars;
    #         return $pos.defined && $pos < $string_length ?? $pos !! $string_length;
    #     }
    #
    #     my $result = pir::reverse_index__ISSi(self, $substring, $pos);
    #     fail("Substring '$substring' not found in '{self}'") if $result < 0;
    #     return $result;
    #
    #     # also used to be a the following error message, but the condition
    #     # was never checked:
    #     # .tailcall '!FAIL'("Attempt to index from negative position")
    # }

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

    our multi method ord() {
        given self.chars {
            when 0  { fail('Can not take ord of empty string'); }
            when 1  { pir::box__PI(pir::ord__IS(self)); }
            default {
                        gather for self.comb {
                            take pir::box__PI(pir::ord__IS($_))
                        }
                    }
        }
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

    multi method eval() {
        eval(~self);
    }

    multi method flip() is export {
        (~self).split('').reverse().join;
    }

    # TODO: signature not fully specced in S32 yet
    multi method trim() is export {
        # (~self).subst(/(^\s+)|(\s+$)/, "", :g)
        if self ~~ /^\s*(.*?)\s*$/ {
            ~$/[0];
        } else {
            self;
        }
    }

    multi method words(Int $limit = *) {
        self.comb( / \S+ /, $limit );
    }

    our Str multi method uc() is export {
        ~(pir::upcase__SS(self))
    }

    our Str multi method ucfirst() is export {
        self gt '' ?? self.substr(0,1).uc ~ self.substr(1) !! ""
    }
}

our multi sub ord($string) {
    $string.ord;
}

our proto ord($string) {
    $string.ord;
}

our Str proto sub infix:<x>($str, $n) {
    $n > 0 ?? ~(pir::repeat__SSI($str, $n)) !!  ''
}

our multi sub infix:<cmp>($a, $b) {
    if $a eq $b {
        0;
    } else {
        $a lt $b ?? -1 !! 1;
    }
}

our multi sub infix:<leg>($a, $b) {
    ~$a cmp ~$b
}


# vim: ft=perl6
