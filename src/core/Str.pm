my class Cursor {... }
my class Range  {... }
my class Match  {... }
my class Buf    {... }
my class X::Str::Numeric { ... }

my $?TABSTOP = 8;

sub PARROT_ENCODING(Str:D $s) {
    my %map = (
        'utf-8'             => 'utf8',
        # according to http://de.wikipedia.org/wiki/ISO-8859-1
        'iso_8859-1:1987'   => 'iso-8859-1',
        'iso_8859-1'        => 'iso-8859-1',
        'iso-ir-100'        => 'iso-8859-1',
        'latin1'            => 'iso-8859-1',
        'latin-1'           => 'iso-8859-1',
        'csisolatin1'       => 'iso-8859-1',
        'l1'                => 'iso-8859-1',
        'ibm819'            => 'iso-8859-1',
        'cp819'             => 'iso-8859-1',
    );
    my Str $lc = lc($s);
    %map{$lc} // $lc;
}

my class Str does Stringy {
    multi method WHICH(Str:D:) {
        nqp::box_s(
            nqp::concat_s(
                nqp::concat_s(nqp::unbox_s(self.^name), '|'),
                $!value
            ),
            ObjAt
        );
    }
    submethod BUILD(:$value as Str = '') {
        nqp::bindattr_s(self, Str, '$!value', nqp::unbox_s($value))
    }

    multi method Bool(Str:D:) { self ne '' && self ne '0' }

    multi method Str(Str:D:) { self }

    method Int(Str:D:) { self.Numeric.Int; }
    method Num(Str:D:) { self.Numeric.Num; }

    multi method ACCEPTS(Str:D: $other) { $other eq self }

    method chomp(Str:D:) {
        my str $sself = nqp::unbox_s(self);
        my int $chars = nqp::chars($sself);
        return '' if $chars == 0;
        my str $last = nqp::substr($sself, $chars - 1);
        my int $to_remove = 0;
        $to_remove = 1 if $last eq "\n" || $last eq "\r";
        $to_remove = 2 if $chars > 1
            && nqp::p6box_s(nqp::substr($sself, $chars - 2)) eq "\r\n";
        nqp::p6box_s(pir::chopn__Ssi($sself, $to_remove))
    }

    method chop(Str:D:) {
        nqp::p6box_s(
            nqp::p6box_s(pir::chopn__Ssi(nqp::unbox_s(self), 1))
        );
    }

    method substr(Str:D: $start, $length? is copy) {
        my str $sself  = nqp::unbox_s(self);
        my int $istart = nqp::unbox_i(
            nqp::istype($start, Callable)
                ?? $start(nqp::p6box_i(nqp::chars($sself)))
                !! $start.Int
            );
        my int $ichars = nqp::chars($sself);
        fail "Negative start argument ($start) to .substr" if $istart < 0;
        fail "Start of substr ($start) beyond end of string" if $istart > $ichars;
        $length = $length($ichars - $istart) if nqp::istype($length, Callable);
        my int $ilength = $length.defined ?? $length.Int !! $ichars - $istart;
        fail "Negative length argument ($length) to .substr" if $ilength < 0;

        nqp::p6box_s(nqp::substr($sself, $istart, $ilength));
    }

    # chars used to handle ranges for pred/succ
    my str $RANGECHAR =
        "01234567890"                                # arabic digits
        ~ "ABCDEFGHIJKLMNOPQRSTUVWXYZA"              # latin uppercase
        ~ "abcdefghijklmnopqrstuvwxyza"              # latin lowercase
        ~ "\x[2160,2161,2162,2163,2164,2165,2166,2167,2168,2169,216a,216b,2160]" # clock roman uc
        ~ "\x[2170,2171,2172,2173,2174,2175,2176,2177,2178,2179,217a,217b,2170]" # clock roman lc
        ~ "\x[2680,2681,2682,2683,2684,2685,2680]";  # die faces

    # calculate the beginning and ending positions of <!after '.'><rangechar+>
    my sub RANGEPOS(str $str) {
        my int $pos = nqp::chars($str);
        while $pos > 0 {
            $pos = $pos - 1;
            my str $ch = nqp::substr($str, $pos, 1);
            if nqp::isge_i(nqp::index($RANGECHAR, $ch, 0), 0) {
                my int $end = $pos;
                while $pos > 0 {
                    $pos = $pos - 1;
                    $ch = nqp::substr($str, $pos, 1);
                    last if nqp::iseq_s($ch, '.');
                    return ($pos+1, $end)
                        unless nqp::isge_i(nqp::index($RANGECHAR, $ch, 0), 0);
                }
                return ($pos, $end) unless nqp::iseq_s($ch, '.');
            }
        }
        return (0, -1);
    }

    method pred(Str:D:) {
        my str $str = self;
        my Int ($Ir0, $Ir1) = RANGEPOS($str);
        my int $r0 = $Ir0;
        my int $r1 = $Ir1;
        while $r1 >= $r0 {
            my str $ch0  = nqp::substr($str, $r1, 1);
            my int $ipos = nqp::index($RANGECHAR, $ch0);
            $ipos = $RANGECHAR.index($ch0, $ipos+1) // $ipos;
            my str $ch1 = nqp::substr($RANGECHAR, $ipos-1, 1);
            $str = pir::replace__Ssiis($str, $r1, 1, $ch1);
            # return if no carry
            return $str if $ch0 gt $ch1;
            # carry to previous position
            $r1 = $r1 - 1;
        }
        # cannot carry beyond first rangechar position
        fail('Decrement out of range');
    }

    method succ(Str:D:) {
        my str $str = self;
        my Int ($Ir0, $Ir1) = RANGEPOS($str);
        my int $r0 = $Ir0;
        my int $r1 = $Ir1;
        while $r1 >= $r0 {
            my str $ch0  = nqp::substr($str, $r1, 1);
            my int $ipos = nqp::index($RANGECHAR, $ch0);
            my str $ch1  = nqp::substr($RANGECHAR, $ipos+1, 1);
            $str = pir::replace__Ssiis($str, $r1, 1, $ch1);
            return $str if $ch1 gt $ch0;
            # carry to previous position
            $r1 = $r1 - 1;
            # extend string if carried past first rangechar position
            $str = pir::replace__Ssiis($str, $r0, 0,
                       $ch1 eq '0' ?? '1' !! $ch1)  # XXX other digits?
                if $r1 < $r0;
        }
        $str;
    }

    multi method Numeric(Str:D: :$strict) {
        return nqp::p6box_n(pir::set__Ns('NaN')) if self eq 'NaN';
        my str $str = nqp::unbox_s(self);
        my int $eos = nqp::chars($str);
        my Int $int;
        my Int $frac = 0;
        my Int $base = 0;
        # skip leading whitespace
        my int $pos   = pir::find_not_cclass__Iisii(pir::const::CCLASS_WHITESPACE, $str, 0, $eos);

        my $tailfail =
             -> { fail(X::Str::Numeric.new(
                     source => self,
                     :$pos,
                     reason => 'trailing characters after number',
                 )) if nqp::islt_i(
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
            $parse := nqp::radix_I($radix, $str, nqp::add_i($pos, 2), $neg, Int);
            $pos = nqp::atpos($parse, 2);
            fail "missing digits after radix prefix" if nqp::islt_i($pos, 0);
            return nqp::atpos($parse, 0) unless $tailfail();
        } elsif nqp::iseq_s(nqp::substr($str, $pos, 1), ':') {
            # a string of form :16<DEAD_BEEF>
            $pos = nqp::add_i($pos, 1);
            $parse := nqp::radix_I(10, $str, $pos, 0, Int);
            $radix = nqp::atpos($parse, 0);
            $pos = nqp::atpos($parse, 2);
            fail "not a number" if nqp::iseq_i($pos, -1);
            fail "malformed radix number, expecting '<' after the base"
                unless nqp::iseq_s(nqp::substr($str, $pos, 1), '<');
            $pos = nqp::add_i($pos, 1);
            $parse := nqp::radix_I($radix, $str, $pos, $neg, Int);
            $pos = nqp::atpos($parse, 2);
            fail "malformed radix number" if nqp::iseq_i($pos, -1);
            fail "malformed radix number, expecting '>' after the body"
                unless nqp::iseq_s(nqp::substr($str, $pos, 1), '>');
            $pos = nqp::add_i($pos, 1);
            return nqp::atpos($parse, 0) unless $tailfail();
        }

        # handle 'Inf'
        if nqp::iseq_s(nqp::substr($str, $pos, 3), 'Inf') {
            $pos = nqp::add_n($pos, 3);
            return ($neg ?? -$Inf !! $Inf) unless $tailfail();
        }

        # We have some sort of number, get leading integer part
        # First check if leading character is '.' ...
        $ch = nqp::islt_i($pos, $eos) && nqp::ord($str, $pos);
        if nqp::iseq_i($ch, 46) {
            $int = 0;
        }
        else {
            my int $p = $pos;
            $parse := nqp::radix_I(10, $str, $pos, $neg, Int);
            $pos = nqp::atpos($parse, 2);
            # XXX: return 0 if ...
            #     We should really fail here instead of returning 0,
            #     but we need to first need to figure out better ways
            #     to handle failure results.
            fail X::Str::Numeric.new(
                     source => self,
                     pos    => $p,
                     reason => 'does not look like a number',
                ) if $strict && nqp::iseq_i($p, 0) && nqp::islt_i($pos, 0);
            return 0 if nqp::iseq_i($p, 0) && nqp::islt_i($pos, 0);
            fail "malformed numeric string" if nqp::islt_i($pos, 0);
            $int = nqp::atpos($parse, 0);
        }

        # if there's a slash, get a denominator and make a Rat
        $ch = nqp::islt_i($pos, $eos) && nqp::ord($str, $pos);
        if nqp::iseq_i($ch, 47) {
            $parse := nqp::radix_I(10, $str, nqp::add_i($pos, 1), 0, Int);
            $pos = nqp::atpos($parse, 2);
            fail "Slash must be followed by denominator" if nqp::islt_i($pos, 0);
            return Rat.new($int, nqp::atpos($parse, 0))
                unless $tailfail();
        }

        # check for decimal fraction or number
        # parse an optional decimal point and value
        if nqp::iseq_i($ch, 46) {
            $parse := nqp::radix_I(10, $str, nqp::add_i($pos, 1), nqp::add_i(4,$neg), Int);
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
            my Int $numerator = $int * $base + $frac;
            return Rat.new($numerator, $base)
                unless $tailfail();
        }

        $int unless $tailfail();
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
        my $x;
        $x = (1..$limit) unless nqp::istype($limit, Whatever) || $limit == $Inf;
        $match
            ?? self.match(:g, :$x, $pat)
            !! self.match(:g, :$x, $pat).map: { .Str }
    }

    # TODO: should be private
    proto method ll-match(Str:D: $, *%) {*}
    multi method ll-match(Str:D: Regex:D $pat, *%opts) {
        my $match := $pat(Cursor.'!cursor_init'(self, |%opts)).MATCH;
        # next line written this way for reasons of circularity sawing
        Cursor.HOW.find_private_method(Cursor, 'set_last_match')(Cursor, $match) if $match;
        $match
    }
    multi method ll-match(Str:D: Cool:D $pat, *%opts) {
        my Int $from = %opts<p> // %opts<c> // 0;
        my $idx = self.index($pat, $from);
        $idx.defined
          ?? Match.new(orig => self, from => $idx, to => ($idx + $pat.chars))
          !! Match.new(orig => self, from => 0,    to => -3);
    }
    method match-list(Str:D: $pat, :$g, :$ov, :$ex, *%opts) {
        if $ex && nqp::istype($pat, Callable) {
            gather {
                my $m := self.ll-match($pat, |%opts);
                if $m {
                    take $m;
                    while $m := $m.CURSOR.'!cursor_next'().MATCH {
                        # next line written this way for reasons of circularity sawing
                        Cursor.HOW.find_private_method(Cursor, 'set_last_match')(Cursor, $m);
                        take $m;
                    }
                }
            }
        }
        elsif $ov || $ex {
            gather {
                my $m := self.ll-match($pat, |%opts);
                while $m {
                    last if $m.to > self.chars;
                    take $m;
                    $m := self.ll-match($pat, :c($m.from + 1));
                }
            }
        }
        elsif $g {
            gather {
                my $m := self.ll-match($pat, |%opts);
                if $m {
                    take $m;
                    while $m := self.ll-match($pat, :c($m.to == $m.from ?? $m.to + 1 !! $m.to)) {
                        last if $m.to > self.chars;
                        take $m;
                    }
                }
            }
        }
        else {
            (self.ll-match($pat, |%opts),).list;
        }
    }
    multi method match(Str:D: $pat, :continue(:$c), :pos(:$p), :global(:$g), :overlap(:$ov), :exhaustive(:$ex), :$x, :st(:nd(:rd(:$nth)))) {
        my %opts;
        if $c.defined {
            %opts<c> = $c
        }
        elsif !$p.defined {
            %opts<c> = 0;
        }
        %opts<p> = $p if $p.defined;
        my @matches := self.match-list($pat, :g($g || $x || $nth), :$ov, :$ex, |%opts);
        if $nth.defined {
            if nqp::istype($nth, Positional) {
                my @nth-monotonic := gather {
                    my $max = 0;
                    for $nth.list {
                        if $_ > $max {
                            # note that $nth is 1-based, but our array
                            # indexes are 0-based. Hence the - 1
                            take $_ - 1;
                            $max = $_;
                        }
                    }
                }
                @matches := @matches[@nth-monotonic].list;
            }
            else {
                return () if $nth < 1;
                @matches := @matches[$nth - 1].defined
                    ?? (@matches[$nth - 1], ).list
                    !! ().list;
            }
        }
        if $x.defined {
            if nqp::istype($x, Int) {
                @matches.gimme($x) == $x
                    ?? @matches[^$x]
                    !! ().list;
            }
            elsif nqp::istype($x, Range) {
                my $real-max := $x.excludes_max ?? $x.max - 1 !! $x.max;
                @matches.gimme($real-max) ~~ $x
                    ?? @matches[^$real-max].list
                    !! ().list;
            }
            else {
                die "Invalid argument to :x, must be Int or Range, got type {$x.^name}";

            }
        }
        else {
            @matches.gimme(2) == 1 ?? @matches[0] !! @matches;
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
        return ().list if $limit ~~ Numeric && $limit <= 0;
        my @matches = nqp::istype($limit, Whatever)
                        ?? self.match($pat, :g)
                        !! self.match($pat, :x(1..$limit-1), :g);
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
        my str $str = nqp::unbox_s(self);
        my int $pos = pir::find_not_cclass__IiSii(
                          pir::const::CCLASS_WHITESPACE,
                          $str, 0, nqp::chars($str));
        nqp::p6box_s(nqp::substr($str, $pos));
    }

    method trim-trailing(Str:D:) {
        my str $str = nqp::unbox_s(self);
        my int $pos = nqp::chars($str) - 1;
        $pos = $pos - 1
            while nqp::isge_i($pos, 0)
               && nqp::iscclass(pir::const::CCLASS_WHITESPACE, $str, $pos);
        nqp::islt_i($pos, 0) ?? '' !! nqp::p6box_s(nqp::substr($str, 0, $pos + 1));
    }

    method trim(Str:D:) {
        my str $str  = nqp::unbox_s(self);
        my int $pos  = nqp::chars($str) - 1;
        my int $left = pir::find_not_cclass__IiSii(
                           pir::const::CCLASS_WHITESPACE, $str, 0, $pos + 1);
        $pos = $pos - 1
            while nqp::isge_i($pos, $left)
               && nqp::iscclass(pir::const::CCLASS_WHITESPACE, $str, $pos);
        nqp::islt_i($pos, $left) ?? '' !! nqp::p6box_s(nqp::substr($str, $left, $pos + 1 - $left));
    }

    method words(Str:D: $limit = *) {
        self.comb( / \S+ /, $limit );
    }

    method encode(Str:D $encoding = 'utf8') {
        my $buf := Buf.new;
        pir::set__vPs(nqp::getattr($buf, Buf, '$!buffer'),
            pir::trans_encoding__ssi(
                nqp::unbox_s(self),
                pir::find_encoding__is(nqp::unbox_s(PARROT_ENCODING($encoding)))
            )
        );
        $buf;
    }

    method capitalize(Str:D:) {
        self.subst(:g, rx/\w+/, -> $_ { .Str.lc.ucfirst });
    }


    my class LSM {
        has Str $!source;
        has @!substitutions;

        has int $!index;
        has int $!next_match;
        has $!next_substitution;
        has $!substitution_length;

        has str $.unsubstituted_text;
        has str $.substituted_text;
        
        submethod BUILD(:$!source) { }

        method add_substitution($key, $value) {
            push @!substitutions, $key => $value;
        }

        submethod compare_substitution($substitution, Int $pos, Int $length) {
            if $!next_match > $pos
               || $!next_match == $pos && $!substitution_length < $length {

                $!next_match = $pos;
                $!substitution_length = $length;
                $!next_substitution = $substitution;
            }
        }

        proto method triage_substitution(|$) {*}
        multi method triage_substitution($_ where { .key ~~ Regex }) {
            my $key = .key;
            return unless $!source.substr($!index) ~~ $key;
            self.compare_substitution($_, $!index + $/.from, $/.to - $/.from);
        }

        multi method triage_substitution($_ where { .key ~~ Cool }) {
            return unless defined index($!source, .key, $!index);
            self.compare_substitution($_,
                                      index($!source, .key, $!index),
                                      .key.chars);
        }

        multi method triage_substitution($_) {
            die "Don't know how to handle a {.WHAT.gist} as a substitution key";
        }

        proto method increment_index(|$) {*}
        multi method increment_index(Regex $s) {
            $!source.substr($!index) ~~ $s;
            $!index = $!next_match + $/.chars;
        }

        multi method increment_index(Cool $s) {
            $!index = $!next_match + nqp::chars($s.Str);
        }

        method next_substitution() {
            $!next_match = $!source.chars;

            for @!substitutions {
                self.triage_substitution($_);
            }

            $!unsubstituted_text # = nqp::substr(nqp::unbox_s($!source), $!index, 
                = $!source.substr($!index, $!next_match - $!index);
            if defined $!next_substitution {
                my $result = $!next_substitution.value;
                $!substituted_text
                    = nqp::unbox_s(($result ~~ Callable ?? $result() !! $result).Str);
                self.increment_index($!next_substitution.key);
            }

            return $!next_match < $!source.chars;
        }
    }

    method trans(Str:D: *@changes) {
        my sub expand($s) {
            return $s.list if $s ~~ Iterable|Positional;
            gather for $s.comb(/ (\w) '..' (\w) | . /, :match) {
                if .[0] {
                    take $_ for ~.[0] .. ~.[1];
                } else {
                    take ~$_;
                }
            }
        }

        my $lsm = LSM.new(:source(self));
        for (@changes) -> $p {
            die "$p.perl() is not a Pair" unless $p ~~ Pair;
            if $p.key ~~ Regex {
                $lsm.add_substitution($p.key, $p.value);
            }
            elsif $p.value ~~ Callable {
                my @from = expand $p.key;
                for @from -> $f {
                    $lsm.add_substitution($f, $p.value);
                }
            }
            else {
                my @from = expand $p.key;
                my @to = expand $p.value;
                for @from Z (@to ?? @to xx ceiling(@from / @to) !! '' xx @from) -> $f, $t {
                    $lsm.add_substitution($f, $t);
                }
            }
        }

        my str $r;
        while $lsm.next_substitution {
            $r = $r ~ nqp::unbox_s($lsm.unsubstituted_text)
                    ~ nqp::unbox_s($lsm.substituted_text);
        }
        $r = $r ~ nqp::unbox_s($lsm.unsubstituted_text);

        return $r;
    }
    proto method indent($) {*}
    # Zero indent does nothing
    multi method indent(Int $steps where { $_ == 0 }) {
        self;
    }

    # Positive indent does indent
    multi method indent(Int $steps where { $_ > 0 }) {
    # We want to keep trailing \n so we have to .comb explicitly instead of .lines
        return self.comb(/:r ^^ \N* \n?/).map({
            given $_.Str {
                # Use the existing space character if they're all the same
                # (but tabs are done slightly differently)
                when /^(\t+) ([ \S .* | $ ])/ {
                    $0 ~ "\t" x ($steps div $?TABSTOP) ~
                         ' '  x ($steps mod $?TABSTOP) ~ $1
                }
                when /^(\h) $0* [ \S | $ ]/ {
                    $0 x $steps ~ $_
                }

                # Otherwise we just insert spaces after the existing leading space
                default {
                    $_ ~~ /^(\h*) (.*)$/;
                    $0 ~ (' ' x $steps) ~ $1
                }
            }
        }).join;
    }

    # Negative values and Whatever-* do outdent
    multi method indent($steps where { nqp::istype($_, Whatever) || nqp::istype($_, Int) && $_ < 0 }) {
        # Loop through all lines to get as much info out of them as possible
        my @lines = self.comb(/:r ^^ \N* \n?/).map({
            # Split the line into indent and content
            my ($indent, $rest) = @($_ ~~ /^(\h*) (.*)$/);

            # Split the indent into characters and annotate them
            # with their visual size
            my $indent-size = 0;
            my @indent-chars = $indent.comb.map(-> $char {
                my $width = $char eq "\t"
                    ?? $?TABSTOP - ($indent-size mod $?TABSTOP)
                    !! 1;
                $indent-size += $width;
                $char => $width;
            });

            { :$indent-size, :@indent-chars, :$rest };
        });

        # Figure out the amount * should outdent by, we also use this for warnings
        my $common-prefix = [min] @lines.map({ $_<indent-size> });

        # Set the actual outdent amount here
        my Int $outdent = $steps ~~ Whatever ?? $common-prefix
                                             !! -$steps;

        warn sprintf('Asked to remove %d spaces, ' ~
                     'but the shortest indent is %d spaces',
                     $outdent, $common-prefix) if $outdent > $common-prefix;

        # Work backwards from the right end of the indent whitespace, removing
        # array elements up to # (or over, in the case of tab-explosion)
        # the specified outdent amount.
        @lines.map({
            my $pos = 0;
            while $_<indent-chars> and $pos < $outdent {
                $pos += $_<indent-chars>.pop.value;
            }
            $_<indent-chars>».key.join ~ ' ' x ($pos - $outdent) ~ $_<rest>;
        }).join;
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
    Order.(nqp::p6box_i(nqp::cmp_s(nqp::unbox_s($a), nqp::unbox_s($b))))
}

multi infix:<===>(Str:D \$a, Str:D \$b) {
    nqp::p6bool(nqp::iseq_s(nqp::unbox_s($a), nqp::unbox_s($b)))
}

multi infix:<leg>(Str:D \$a, Str:D \$b) {
    Order.(nqp::p6box_i(nqp::cmp_s(nqp::unbox_s($a), nqp::unbox_s($b))))
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
sub unbase(Int:D $base, Str:D $str) {
    my Str $prefix = $str.substr(0, 2);
    if    $base <= 10 && $prefix eq any(<0x 0d 0o 0b>)
       or $base <= 24 && $prefix eq any <0o 0x>
       or $base <= 33 && $prefix eq '0x' {
        $str.Numeric;

    } else {
        ":{$base}<$str>".Numeric;
    }
}

sub chrs(*@c) {
    @c.map({.chr}).join('');
}
