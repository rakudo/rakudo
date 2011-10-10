my class Cursor {... }
my class Range  {... }
my class Match  {... }
my class Buf    {... }

my class Str does Stringy {
    submethod BUILD(:$value as Str = '') {
        nqp::bindattr_s(self, Str, '$!value', nqp::unbox_s($value))
    }

    multi method Bool(Str:D:) { self ne '' && self ne '0' }
    
    multi method Str(Str:D:) { self }
    
    method Int(Str:D:) { self.Numeric.Int; }
    method Num(Str:D:) { self.Numeric.Num; }

    multi method ACCEPTS(Str:D: $other) { $other eq self }

    method chomp(Str:D:) {
        my int $chars = self.chars;
        return '' if $chars == 0;
        my str $last = nqp::substr(nqp::unbox_s(self), $chars - 1);
        my int $to_remove = 0;
        $to_remove = 1 if $last eq "\n" || $last eq "\r";
        $to_remove = 2 if $chars > 1
            && nqp::p6box_s(nqp::substr(nqp::unbox_s(self), $chars - 2)) eq "\r\n";
        nqp::p6box_s(pir::chopn__Ssi(nqp::unbox_s(self), $to_remove))
    }

    method chop(Str:D:) {
        nqp::p6box_s(
            nqp::p6box_s(pir::chopn__Ssi(nqp::unbox_s(self), 1))
        );
    }

    method substr(Str:D: $start, $length? is copy) {
        fail "Negative start argument ($start) to .substr" if $start < 0;
        fail "Start of substr ($start) beyond end of string" if $start > self.chars;
        $length = $length.defined ?? $length.Int !! self.chars - $start.Int;
        fail "Negative length argument ($length) to .substr" if $length < 0;

        nqp::p6box_s(nqp::substr(
            nqp::unbox_s(self),
            nqp::unbox_i($start.Int),
            nqp::unbox_i($length)));
    } 

    # chars used to handle ranges for pred/succ
    my $RANGECHAR = 
        "01234567890"                                # arabic digits
        ~ "ABCDEFGHIJKLMNOPQRSTUVWXYZA"              # latin uppercase
        ~ "abcdefghijklmnopqrstuvwxyza"              # latin lowercase
        ~ "\x[2160,2161,2162,2163,2164,2165,2166,2167,2168,2169,216a,216b,2160]" # clock roman uc
        ~ "\x[2170,2171,2172,2173,2174,2175,2176,2177,2178,2179,217a,217b,2170]" # clock roman lc
        ~ "\x[2680,2681,2682,2683,2684,2685,2680]";  # die faces

    # calculate the beginning and ending positions of <!after '.'><rangechar+>
    my sub RANGEPOS($str) {
        my $pos = $str.chars;
        while $pos > 0 {
            $pos--;
            my str $ch = nqp::substr(nqp::unbox_s($str), nqp::unbox_i($pos), 1);
            if nqp::isge_i(nqp::index(nqp::unbox_s($RANGECHAR), $ch, 0), 0) {
                my $end = $pos;
                while $pos > 0 {
                    $pos--;
                    $ch = nqp::substr(nqp::unbox_s($str), nqp::unbox_i($pos), 1);
                    last if nqp::iseq_s($ch, '.');
                    return ($pos+1, $end)
                        unless nqp::isge_i(nqp::index(nqp::unbox_s($RANGECHAR), $ch, 0), 0);
                }
                return ($pos, $end) unless nqp::iseq_s($ch, '.');
            }
        }
        return (0, -1);
    }

    method pred(Str:D:) {
        my $str = self;
        my ($r0, $r1) = RANGEPOS($str);
        while $r1 >= $r0 {
            my $ch0  = $str.substr($r1, 1);
            my $ipos = $RANGECHAR.index($ch0);
            $ipos = $RANGECHAR.index($ch0, $ipos+1) // $ipos;
            my $ch1 = $RANGECHAR.substr($ipos-1, 1);
            $str = nqp::p6box_s(
                       pir::replace__Ssiis(
                           nqp::unbox_s($str), 
                           $r1, 1, 
                           nqp::unbox_s($ch1)));
            # return if no carry
            return $str if $ch0 gt $ch1;
            # carry to previous position
            $r1--;
        }
        # cannot carry beyond first rangechar position
        fail('Decrement out of range');
    }

    method succ(Str:D:) {
        my $str = self;
        my ($r0, $r1) = RANGEPOS($str);
        while $r1 >= $r0 {
            my $ch0  = $str.substr($r1, 1);
            my $ipos = $RANGECHAR.index($ch0);
            my $ch1  = $RANGECHAR.substr($ipos+1, 1);
            $str = nqp::p6box_s(
                       pir::replace__Ssiis(
                           nqp::unbox_s($str), 
                           $r1, 1, 
                           nqp::unbox_s($ch1)));
            return $str if $ch1 gt $ch0;
            # carry to previous position
            $r1--;
            # extend string if carried past first rangechar position
            $str = nqp::p6box_s(
                       pir::replace__Ssiis(
                           nqp::unbox_s($str),
                           $r0, 0,
                       $ch1 eq '0' ?? '1' !! nqp::unbox_s($ch1)))  # XXX other digits?
                if $r1 < $r0;
        }
        $str;
    }

    multi method Numeric(Str:D:) {
        return nqp::p6box_n(pir::set__Ns('NaN')) if self eq 'NaN';
        my str $str = nqp::unbox_s(self);
        my int $eos = nqp::chars($str);
        my num $int;
        my num $frac = 0;
        my num $base = 0;
        # skip leading whitespace
        my int $pos   = pir::find_not_cclass__Iisii(pir::const::CCLASS_WHITESPACE, $str, 0, $eos);

        my $tailfail = 
             -> { fail "trailing characters after number in conversion"
                      if nqp::islt_i(
                          pir::find_not_cclass__Iisii(pir::const::CCLASS_WHITESPACE, 
                                                      $str, $pos, $eos),
                          $eos);
                  0;
             };

        # objects for managing the parse and results
        my Mu $parse;
        my $result;
    
        # get any leading +/- sign
        my int $ch = nqp::islt_i($pos, $eos) && nqp::ord($str, $pos);
        my int $neg = nqp::iseq_i($ch, 45);
        $pos = nqp::add_i($pos, 1) if nqp::iseq_i($ch, 45) || nqp::iseq_i($ch, 43);

        # handle 0x, 0d, etc. prefixes, if present
        my str $rpref = nqp::substr($str, $pos, 2);
        my int $radix =
            nqp::iseq_s($rpref, '0x') ?? 16
              !! nqp::iseq_s($rpref, '0d') ?? 10
              !! nqp::iseq_s($rpref, '0o') ?? 8
              !! nqp::iseq_s($rpref, '0b') ?? 2
              !! 0;
        if $radix {
            $parse := nqp::radix($radix, $str, nqp::add_i($pos, 2), $neg);
            $pos = nqp::atpos($parse, 2);
            fail "missing digits after radix prefix" if nqp::islt_i($pos, 0);
            return nqp::p6bigint(nqp::atpos($parse, 0)) unless $tailfail();
        } elsif nqp::iseq_s(nqp::substr($str, $pos, 1), ':') {
            # a string of form :16<DEAD_BEEF>
            $pos = nqp::add_i($pos, 1);
            $parse := nqp::radix(10, $str, $pos, 0);
            $radix = nqp::atpos($parse, 0);
            $pos = nqp::atpos($parse, 2);
            fail "not a number" if nqp::iseq_i($pos, -1);
            fail "malformed radix number, expecting '<' after the base"
                unless nqp::iseq_s(nqp::substr($str, $pos, 1), '<');
            $pos = nqp::add_i($pos, 1);
            $parse := nqp::radix($radix, $str, $pos, $neg);
            $pos = nqp::atpos($parse, 2);
            fail "malformed radix number" if nqp::iseq_i($pos, -1);
            fail "malformed radix number, expecting '>' after the body"
                unless nqp::iseq_s(nqp::substr($str, $pos, 1), '>');
            $pos = nqp::add_i($pos, 1);
            return nqp::p6bigint(nqp::atpos($parse, 0)) unless $tailfail();
        }

        # handle 'Inf'
        if nqp::iseq_s(nqp::substr($str, $pos, 3), 'Inf') {
            $pos = nqp::add_n($pos, 3);
            return ($neg ?? -$Inf !! $Inf) unless $tailfail();
        }

        # We have some sort of number, get leading integer part
        my int $p = $pos;
        $parse := nqp::radix(10, $str, $pos, $neg);
        $pos = nqp::atpos($parse, 2);
        # XXX: return 0 if ...
        #     We should really fail here instead of returning 0,
        #     but we need to first need to figure out better ways
        #     to handle failure results.
        return 0 if nqp::iseq_i($p, 0) && nqp::islt_i($pos, 0);
        fail "malformed numeric string" if nqp::islt_i($pos, 0);
        $int = nqp::atpos($parse, 0);

        # if there's a slash, get a denominator and make a Rat
        $ch = nqp::islt_i($pos, $eos) && nqp::ord($str, $pos);
        if nqp::iseq_i($ch, 47) {
            $parse := nqp::radix(10, $str, nqp::add_i($pos, 1), 0);
            $pos = nqp::atpos($parse, 2);
            fail "Slash must be followed by denominator" if nqp::islt_i($pos, 0);
            return Rat.new(nqp::p6bigint($int), nqp::p6bigint(nqp::atpos($parse, 0)))
                unless $tailfail();
        }

        # check for decimal fraction or number
        # parse an optional decimal point and value
        if nqp::iseq_i($ch, 46) {
            $parse := nqp::radix(10, $str, nqp::add_i($pos, 1), nqp::add_i(4,$neg));
            $pos = nqp::atpos($parse, 2);
            fail "Decimal point must be followed by digit" if nqp::islt_i($pos, 0);
            $frac = nqp::atpos($parse, 0);
            $base = nqp::atpos($parse, 1);
            $ch = nqp::islt_i($pos, $eos) && nqp::ord($str, $pos);
        }
    
        # handle exponent if 'E' or 'e' are present
        if nqp::iseq_i($ch, 69) || nqp::iseq_i($ch, 101) {
            $parse := nqp::radix(10, $str, nqp::add_i($pos, 1), 2);
            $pos = nqp::atpos($parse, 2);
            fail "'E' or 'e' must be followed by integer" if nqp::islt_i($pos, 0);
            my num $exp = nqp::atpos($parse, 0);
            my num $coef = $frac ?? nqp::add_n($int, nqp::div_n($frac, $base)) !! $int;
            return nqp::p6box_n(nqp::mul_n($coef, nqp::pow_n(10, $exp)))
                unless $tailfail();
        }

        # if we got a decimal point above, it's a Rat
        if $base {
            my num $numerator = nqp::add_n(nqp::mul_n($int, $base), $frac);
            return Rat.new(nqp::p6bigint($numerator), nqp::p6bigint($base))
                unless $tailfail();
        }

        nqp::p6bigint($int) unless $tailfail();
    }

    my %esc = (
        '$' => '\$',  '@' => '\@',  '%' => '\%',  '&' => '\&',  '{' => '\{',
        "\b" => '\b', "\n" => '\n', "\r" => '\r', "\t" => '\t', '"' => '\"',
        '\\' => '\\\\' );

    multi method gist(Str:D:) { self }
    multi method perl(Str:D:) {
        my $result = '"';
        for ^self.chars -> $i {
            my $ch = self.substr($i, 1);
            $result ~= %esc{$ch} // (pir::is_cclass__Iisi(
                                            pir::const::CCLASS_PRINTING,
                                            nqp::unbox_s($ch), 0)
                                      ?? $ch 
                                      !! $ch.ord.fmt('\x[%x]'));
        }
        $result ~ '"'
    }

    multi method comb(Str:D:) {
        (^self.chars).map({self.substr($_, 1) });
    }
    multi method comb(Str:D: Regex $pat, $limit = $Inf, :$match) {
        $match
            ?? self.match(:g, :x(1..$limit), $pat)
            !! self.match(:g, :x(1..$limit), $pat).map: { .Str }
    }

    # TODO: should be private
    proto method ll-match(Str:D: $, *%) {*}
    multi method ll-match(Str:D: Regex:D $pat, *%opts) {
        $pat(Cursor.'!cursor_init'(self, |%opts)).MATCH
    }
    multi method ll-match(Str:D: Cool:D $pat, *%opts) {
        my Int $from = %opts<p> // %opts<c> // 0;
        my $idx = self.index($pat, $from);
        defined $idx
          ?? Match.new(orig => self, from => $idx, to => ($idx + $pat.chars))
          !! Match.new(orig => self, from => 0,    to => -3);
    }

    multi method match(Str:D: $pat, :continue(:$c), :pos(:$p), :global(:$g), :ov(:$overlap), :$x) {
        # XXX initialization is a workaround for a nom bug
        my %opts := {};
        if $c.defined {
            %opts<c> = $c
        } elsif !$p.defined {
            %opts<c> = 0;
        }
        %opts<p> = $p if $p.defined;
        my $x_upper = -1;
        if $x.defined {
            return Nil if $x == 0;
            $x_upper = $x ~~ Range
                 ?? ( $x.excludes_max ?? $x.max - 1 !! $x )
                 !! $x
        }
        if $g  || $overlap || $x.defined {
            my @r;
            while my $m = self.ll-match($pat, |%opts) {
                # XXX a bug in the regex engine means that we can
                # match a zero-width match past the end of the string.
                # This is the workaround:
                last if $m.to > self.chars;

                @r.push: $m;
                last if @r.elems == $x_upper;

                %opts.delete('d');
                %opts<c> = $overlap
                        ?? $m.from +1
                        !!  ($m.to == $m.from ?? $m.to + 1 !! $m.to);
            }
            return if $x.defined && @r.elems !~~ $x;
            return @r;
        } else {
            self.ll-match($pat, |%opts)
        }
    }

    multi method subst($matcher, $replacement,
                       :ii(:$samecase), :ss(:$samespace),
                       :$SET_CALLER_DOLLAR_SLASH, *%options) {
        my @matches = self.match($matcher, |%options);
        return self unless @matches;
        return self if @matches == 1 && !@matches[0];
        my $caller_dollar_slash := pir::find_caller_lex__Ps('$/');
        my $prev = 0;
        my $result = '';
        for @matches -> $m {
            $result ~= self.substr($prev, $m.from - $prev);

            $caller_dollar_slash = $m if $SET_CALLER_DOLLAR_SLASH;
            my $real_replacement = ~($replacement ~~ Callable ?? $replacement($m) !! $replacement);
            $real_replacement    = $real_replacement.samecase(~$m) if $samecase;
            $real_replacement    = $real_replacement.samespace(~$m) if $samespace;
            $result ~= $real_replacement;
            $prev = $m.to;
        }
        my $last = @matches.pop;
        $result ~= self.substr($last.to);
        $result;
    }

    method ords(Str:D:) {
        my Int $c  = self.chars;
        my str $ns = nqp::unbox_s(self);
        (^$c).map: { nqp::p6box_i(nqp::ord(nqp::substr($ns, $_, 1))) }
    }

    method lines(Str:D: $limit = $Inf) {
        my $prev_pos = -1;
        my $l = 0;
        gather {
            while defined(my $current_pos = self.index("\n", $prev_pos + 1)) && $l++ < $limit {
                take self.substr($prev_pos + 1, $current_pos - $prev_pos - 1);
                $prev_pos = $current_pos;
            }
            take self.substr($prev_pos + 1) if $prev_pos + 1 < self.chars && $l <= $limit;
        }
    }

    multi method split(Str:D: Regex $pat, $limit = *, :$all) {
        my $l = $limit ~~ Whatever ?? $Inf !! $limit - 1;
        return ().list if $l < 0;
        my @matches := self.match($pat, :x(1..$l), :g);
        gather {
            my $prev-pos = 0;
            for @matches {
                take self.substr($prev-pos, .from - $prev-pos);
                take $_ if $all;
                $prev-pos = .to;
            }
            take self.substr($prev-pos);
        }
    }
    multi method split(Str:D: Cool $delimiter, $limit = *, :$all) {
        my $match-string = $delimiter.Str;
        return if self eq '' && $delimiter eq '';
        my $c = 0;
        my $l = $limit ~~ Whatever ?? $Inf !! $limit - 1;
        return ().list if $l < 0;
        if $l >= 0 {
            gather {
                while $l-- > 0 {
                    if ($match-string eq "") {
                        last unless $c + 1 < self.chars;
                        take self.substr($c, 1);
                        $c++;
                    } else {
                        my $m = self.index($match-string, $c);
                        last unless $m.defined;
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

    method samecase(Str:D: Str $pattern) {
        my @chars;
        my @pat = $pattern.comb;
        my $p = '';
        for self.comb -> $s {
            $p = @pat.shift if @pat;
            push @chars, $p ~~ /<.upper>/  ?? $s.uc
                      !! $p ~~ /<.lower>/  ?? $s.lc
                      !! $s;
        }
        @chars.join('');
    }

    method samespace(Str:D: Str:D $pat) {
        my @self-chunks  = self.split(rx/\s+/, :all);
        my @pat-chunks  := $pat.split(rx/\s+/, :all);
        loop (my $i = 1; $i < @pat-chunks && $i < @self-chunks; $i += 2) {
            @self-chunks[$i] = @pat-chunks[$i];
        }
        @self-chunks.join;
    }

    method trim-leading(Str:D:) {
        my Int $pos = nqp::p6box_i(
            pir::find_not_cclass__IiSii(
                pir::const::CCLASS_WHITESPACE,
                nqp::unbox_s(self),
                0,
                nqp::unbox_i(self.chars)
            )
        );
        self.substr($pos);
    }


    method trim-trailing(Str:D:) {
        my Int $pos = self.chars - 1;
        --$pos while $pos >= 0 && nqp::p6bool(
                nqp::iscclass(
                    pir::const::CCLASS_WHITESPACE,
                    nqp::unbox_s(self),
                    nqp::unbox_i($pos)
                )
            );
        $pos < 0 ?? '' !!  self.substr(0, $pos + 1);
    }

    method trim(Str:D:) {
        self.trim-leading.trim-trailing;
    }

    method words(Str:D: $limit = *) {
        self.comb( / \S+ /, $limit );
    }

    method encode(Str:D $encoding = 'utf8') {
        my $buf := Buf.new;
        pir::set__vPs(nqp::getattr($buf, Buf, '$!buffer'),
            pir::trans_encoding__ssi(
                nqp::unbox_s(self),
                pir::find_encoding__is(nqp::unbox_s(pir::perl6_decontainerize__PP($encoding.lc)))
            )
        );
        $buf;
    }

    method capitalize(Str:D:) {
        self.subst(:g, rx/\w+/, -> $_ { .Str.lc.ucfirst });
    }
}


multi prefix:<~>(Str:D \$a) { $a }

multi infix:<~>(Str:D \$a, Str:D \$b) {
    nqp::p6box_s(nqp::concat_s(nqp::unbox_s($a), nqp::unbox_s($b)))
}

multi infix:<x>(Str:D $s, Int:D $repetition) {
    $repetition <= 0
        ?? ''
        !!  nqp::p6box_s(nqp::x(nqp::unbox_s($s), nqp::unbox_i($repetition)))
}

multi infix:<cmp>(Str:D \$a, Str:D \$b) {
    nqp::p6box_i(nqp::cmp_s(nqp::unbox_s($a), nqp::unbox_s($b)))
}

multi infix:<===>(Str:D \$a, Str:D \$b) {
    nqp::p6bool(nqp::iseq_s(nqp::unbox_s($a), nqp::unbox_s($b)))
}

multi infix:<leg>(Str:D \$a, Str:D \$b) {
    nqp::p6box_i(nqp::cmp_s(nqp::unbox_s($a), nqp::unbox_s($b)))
}

multi infix:<eq>(Str:D \$a, Str:D \$b) {
    nqp::p6bool(nqp::iseq_s(nqp::unbox_s($a), nqp::unbox_s($b)))
}

multi infix:<lt>(Str:D \$a, Str:D \$b) {
    nqp::p6bool(nqp::islt_s(nqp::unbox_s($a), nqp::unbox_s($b)))
}

multi infix:<le>(Str:D \$a, Str:D \$b) {
    nqp::p6bool(nqp::isle_s(nqp::unbox_s($a), nqp::unbox_s($b)))
}

multi infix:<gt>(Str:D \$a, Str:D \$b) {
    nqp::p6bool(nqp::isgt_s(nqp::unbox_s($a), nqp::unbox_s($b)))
}

multi infix:<ge>(Str:D \$a, Str:D \$b) {
    nqp::p6bool(nqp::isge_s(nqp::unbox_s($a), nqp::unbox_s($b)))
}


multi infix:<~|>(Str:D \$a, Str:D \$b) {
    nqp::p6box_s(pir::bors__SSS(nqp::unbox_s($a), nqp::unbox_s($b)))
}

multi infix:<~&>(Str:D \$a, Str:D \$b) {
    nqp::p6box_s(pir::bands__SSS(nqp::unbox_s($a), nqp::unbox_s($b)))
}

multi infix:<~^>(Str:D \$a, Str:D \$b) {
    nqp::p6box_s(pir::bxors__SSS(nqp::unbox_s($a), nqp::unbox_s($b)))
}

multi prefix:<~^>(Str \$a) {
    fail "prefix:<~^> NYI";   # XXX
}

multi sub ords(Str $s) {
    my Int $c  = $s.chars;
    my str $ns = nqp::unbox_s($s);
    (^$c).map: { nqp::p6box_i(nqp::ord(nqp::substr($ns, $_, 1))) }
}

# TODO: Cool  variants
sub trim         (Str:D $s) { $s.trim }
sub trim-leading (Str:D $s) { $s.trim-leading }
sub trim-trailing(Str:D $s) { $s.trim-trailing }

# the opposite of Real.base, used for :16($hex_str)
sub unbase(Int:D $base, Cool:D $str) {
    if $str.substr(0, 2) eq any(<0x 0d 0o 0b>) {
        $str.Numeric;
    } else {
        ":{$base}<$str>".Numeric;
    }
}

sub chrs(*@c) {
    @c.map({.chr}).join('');
}
