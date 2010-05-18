augment class Cool {

    our Int multi method bytes() is export {
        pir::box__PI(pir::bytelength__IS(self))
    }

    # The spec has a more elegant approach for this,
    # but this one works now.
    our Str multi method capitalize() {
        self.lc.split(/\w+/, :all).map({ .Str.ucfirst }).join('');
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

    multi method subst($matcher, $replacement, :g(:$global), :$x) {
        die "Can't combine :g/:global and :x in subst"
            if defined($global) && defined($x);
        my $limit = defined($x) ?? $x +1 !! 2;
        my @chunks = self.split($matcher, :limit($global ?? * !! $limit), :all);
        if defined($x) && (@chunks < 2 * $x) {
            return self;
        }
        loop (my $i = 1; $i < @chunks; $i += 2) {
            @chunks[$i] = $replacement ~~ Callable ?? $replacement(@chunks[$i]) !! $replacement;
        }
        @chunks.join('');
    }


    multi method comb(Regex $matcher = /./, $limit = *, :$match) {
        my $c = 0;
        my $l = $limit ~~ ::Whatever ?? Inf !! $limit;
        gather while $l > 0 && (my $m = self.match($matcher, :c($c))) {
            if $match {
                my $m-clone = $m;
                take $m-clone;
            } else {
                take ~$m;
            }
            $c = $m.to == $c ?? $c + 1 !! $m.to;
            --$l;
        }
    }

    multi method samecase($pattern) is export {
        my $result = '';
        my $p = '';
        my @pattern = $pattern.comb;
        for self.comb -> $s {
            $p = @pattern.shift if @pattern;
            if $p ~~ /<.upper>/ {
                $result ~= $s.uc;
            } elsif $p ~~ /<.lower>/ {
                $result ~= $s.lc;
            } else {
                $result ~= $s;
            }
        }
        $result;
    }

    multi method split(Regex $matcher, $limit = *, :$all) {
        my $c = 0;
        my $l = $limit ~~ ::Whatever ?? Inf !! $limit - 1;
        if $l >= 0 {
            gather {
                while $l-- > 0 && (my $m = self.match($matcher, :c($c))) {
                    take self.substr($c, $m.from - $c);
                    my $m-clone = $m;
                    take $m-clone if $all;
                    $c = $m.to == $c ?? $c + 1 !! $m.to;
                }
                take self.substr($c);
            }
        } else {
            Nil;
        }
    }

    multi method split($delimiter, $limit = *, :$all) {
        my $match-string = $delimiter.Str;
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
                        take $match-string if $all;
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

        if ($start > self.chars || $start < -self.chars) {
            return Mu;
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

    our Str multi method lc() {
        ~(pir::downcase__SS(self))
    }

    our Str multi method lcfirst() {
        self gt '' ?? self.substr(0,1).lc ~ self.substr(1) !! ""
    }

    our multi method match(Regex $pat,
                           :c(:$continue),
                           :g(:$global),
                           :pos(:$p),
                           :$x,
                           Mu :$nth,
                           :ov(:$overlap)) {
        if $continue ~~ Bool {
            note ":c / :continue requires a position in the string";
            fail ":c / :continue requires a position in the string";
        }
        my %opts;
        %opts<p> = $p        if defined $p;
        %opts<c> = $continue // 0 unless defined $p;
        my $x_upper = -1;
        if defined($x) {
            if $x ~~ Range {
                $x_upper = $x.excludes_max ?? $x.max - 1 !! $x.max;
            } else {
                $x_upper = $x;
            }
        }

        if $global || $nth.defined || $overlap || ($x.defined && $x_upper > 1) {
            my $taken = 0;
            my $i = 1;
            my @r = gather while my $m = Regex::Cursor.parse(self, :rule($pat), |%opts) {
                my $m-copy = $m;
                unless $nth.defined && ($i !~~ any |$nth) {
                    take $m-copy;
                    $taken++;
                }
                last if $taken == $x_upper;

                if ($overlap) {
                    %opts<c> = $m.from + 1;
                } else {
                    if $m.to == $m.from {
                        %opts<c> = $m.to + 1;
                    } else {
                        %opts<c> = $m.to;
                    }
                }

                $i++;
            }
            if $x.defined && $taken !~~ $x {
                return;
            }
            return |@r;
        } else {
            Regex::Cursor.parse(self, :rule($pat), |%opts);
        }
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

    # Not yet spec'd, I expect it will be renamed
    multi method trim-leading() is export {
        if self ~~ /^\s*:(.*)$/ {
            ~$/[0];
        } else {
            self;
        }
    }

    # Not yet spec'd, I expect it will be renamed
    multi method trim-trailing() is export {
        if self ~~ /^(.*\S)\s*$/ {
            ~$/[0];
        } elsif self ~~ /^\s*$/ {
            "";
        }
        else {
            self;
        }
    }

    # TODO: signature not fully specced in S32 yet
    multi method trim() is export {
        self.trim-leading.trim-trailing;
    }

    multi method words(Int $limit = *) {
        self.comb( / \S+ /, $limit );
    }

    our Str multi method uc() {
        ~(pir::upcase__SS(self))
    }

    our Str multi method ucfirst() {
        self gt '' ?? self.substr(0,1).uc ~ self.substr(1) !! ""
    }

    our Str multi method sprintf(*@args) {
        my $result;
        try {
            $result = pir::sprintf__SSP(~self, (|@args)!PARROT_POSITIONALS);
        }
        $! ?? fail( "Insufficient arguments supplied to sprintf") !! $result
    }
}

multi sub ord($string) {
    $string.ord;
}

proto ord($string) {
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

multi split ( Str $delimiter, Str $input, Int $limit = * ) {
    $input.split($delimiter, $limit);
}

multi split ( Regex $delimiter, Str $input, Int $limit = * ) {
    $input.split($delimiter, $limit);
}

multi sub sprintf($str as Str, *@args) {
    $str.sprintf(|@args)
}

proto sub uc($string) { $string.uc; }
proto sub ucfirst($string) { $string.ucfirst; }
proto sub lc($string) { $string.lc; }
proto sub lcfirst($string) { $string.lcfirst; }
proto sub capitalize($string) { $string.capitalize; }

# vim: ft=perl6
