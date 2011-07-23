my class Cursor {... }

my class Str does Stringy {
    method Bool() { self ne '' && self ne '0' }
    
    multi method Str(Str:D:) { self }
    
    method Int() { self.Numeric.Int; }
    method Num() { self.Numeric.Num; }

    multi method ACCEPTS(Str:D: $other) { $other eq self }

    method chomp() {
        my Int $chars = self.chars;
        return '' if $chars == 0;
        my Str $last = nqp::p6box_s(nqp::substr(nqp::unbox_s(self), nqp::unbox_i($chars - 1)));
        my Int $to_remove = 0;
        $to_remove = 1 if $last eq "\n" || $last eq "\r";
        $to_remove = 2 if $chars > 1
            && nqp::p6box_s(nqp::substr(nqp::unbox_s(self), nqp::unbox_i($chars - 2))) eq "\r\n";
        nqp::p6box_s(pir::chopn__Ssi(nqp::unbox_s(self),nqp::unbox_i($to_remove)))
    }

    method chop() {
        nqp::p6box_s(
            nqp::p6box_s(pir::chopn__Ssi(nqp::unbox_s(self), 1))
        );
    }

    method substr($start, $length? is copy) {
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
            my $ch = $str.substr($pos, 1);
            if $RANGECHAR.index($ch).defined {
                my $end = $pos;
                while $pos > 0 {
                    $pos--;
                    $ch = $str.substr($pos, 1);
                    last if $ch eq '.';
                    return ($pos+1, $end) unless $RANGECHAR.index($ch).defined;
                }
                return ($pos, $end) unless $ch eq '.';
            }
        }
        return (0, -1);
    }

    method pred() {
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

    method succ() {
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

    method Numeric(Str:D:) {
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

    multi method comb() {
        (^self.chars).map({self.substr($_, 1) });
    }
    multi method comb(Regex $pat, :$match) {
        $match
            ?? self.match(:g, $pat)
            !! self.match(:g, $pat).map: { .Str }
    }


    multi method match(Regex $pat, :continue(:$c), :pos(:$p), :global(:$g), :ov(:$overlap)) {
        # XXX initialization is a workaround for a nom bug
        my %opts := {};
        if $c.defined {
            %opts<c> = $c
        } elsif !$p.defined {
            %opts<c> = 0;
        }
        %opts<p> = $p if $p.defined;
        if $g  || $overlap {
            gather while my $m = $pat(Cursor.'!cursor_init'(self, |%opts)).MATCH {
                # XXX a bug in the regex engine means that we can
                # match a zero-width match past the end of the string.
                # This is the workaround:
                last if $m.to > self.chars;

                take $m;

                # XXX should be %opts.delete('d'), but Hash.delete is NYI
                %opts<d> = Any if %opts<d>;
                %opts<c> = $overlap
                        ?? $m.from +1
                        !!  ($m.to == $m.from ?? $m.to + 1 !! $m.to);
            }
        } else {
            $pat(Cursor.'!cursor_init'(self, |%opts)).MATCH;
        }
    }

    method ords(Str:D:) {
        my Int $c  = self.chars;
        my str $ns = nqp::unbox_s(self);
        (^$c).map: { nqp::p6box_i(nqp::ord(nqp::substr($ns, $_, 1))) }
    }

    method lines(Str:D:) {
        my $prev_pos = -1;
        gather {
            while defined(my $current_pos = self.index("\n", $prev_pos + 1)) {
                take self.substr($prev_pos + 1, $current_pos - $prev_pos - 1);
                $prev_pos = $current_pos;
            }
            take self.substr($prev_pos + 1) if $prev_pos + 1 < self.chars;
        }
    }

    multi method split(Regex $pat, :$all) {
        my @matches := self.match($pat, :g);
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
    multi method split(Cool $delimiter, $limit = *, :$all) {
        my $match-string = $delimiter.Str;
        return if self eq '' && $delimiter eq '';
        my $c = 0;
        my $l = $limit ~~ Whatever ?? $Inf !! $limit - 1;
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
}


multi prefix:<~>(Str \$a) { $a }

multi infix:<~>(Str \$a, Str \$b) {
    nqp::p6box_s(nqp::concat(nqp::unbox_s($a), nqp::unbox_s($b)))
}

multi infix:<x>(Str $s, Int $repetition) {
    nqp::p6box_s(nqp::x(nqp::unbox_s($s), nqp::unbox_i($repetition)))
}

multi infix:<cmp>(Str \$a, Str \$b) {
    nqp::p6box_i(nqp::cmp_s(nqp::unbox_s($a), nqp::unbox_s($b)))
}

multi infix:<===>(Str \$a, Str \$b) {
    nqp::p6bool(nqp::iseq_s(nqp::unbox_s($a), nqp::unbox_s($b)))
}

multi infix:<leg>(Str \$a, Str \$b) {
    nqp::p6box_i(nqp::cmp_s(nqp::unbox_s($a), nqp::unbox_s($b)))
}

multi infix:<eq>(Str \$a, Str \$b) {
    nqp::p6bool(nqp::iseq_s(nqp::unbox_s($a), nqp::unbox_s($b)))
}

multi infix:<ne>(Str \$a, Str \$b) {
    nqp::p6bool(nqp::isne_s(nqp::unbox_s($a), nqp::unbox_s($b)))
}

multi infix:<lt>(Str \$a, Str \$b) {
    nqp::p6bool(nqp::islt_s(nqp::unbox_s($a), nqp::unbox_s($b)))
}

multi infix:<le>(Str \$a, Str \$b) {
    nqp::p6bool(nqp::isle_s(nqp::unbox_s($a), nqp::unbox_s($b)))
}

multi infix:<gt>(Str \$a, Str \$b) {
    nqp::p6bool(nqp::isgt_s(nqp::unbox_s($a), nqp::unbox_s($b)))
}

multi infix:<ge>(Str \$a, Str \$b) {
    nqp::p6bool(nqp::isge_s(nqp::unbox_s($a), nqp::unbox_s($b)))
}


multi infix:<~|>(Str \$a, Str \$b) {
    nqp::p6box_s(pir::bors__SSS(nqp::unbox_s($a), nqp::unbox_s($b)))
}

multi infix:<~&>(Str \$a, Str \$b) {
    nqp::p6box_s(pir::bands__SSS(nqp::unbox_s($a), nqp::unbox_s($b)))
}

multi infix:<~^>(Str \$a, Str \$b) {
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
