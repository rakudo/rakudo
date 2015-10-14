my class Cursor {... }
my class Range  {... }
my class Match  {... }
my class X::Str::Numeric  { ... }
my class X::Str::Match::x { ... }
my class X::Str::Trans::IllegalKey { ... }
my class X::Str::Trans::InvalidArg { ... }
my class X::Numeric::Confused { ... }

my constant $?TABSTOP = 8;

sub NORMALIZE_ENCODING(Str:D $s) {
    state %map = (
        # fast mapping for identicals
        'utf8'              => 'utf8',
        'utf16'             => 'utf16',
        'utf32'             => 'utf32',
        'ascii'             => 'ascii',
        'iso-8859-1'        => 'iso-8859-1',
        'windows-1252'      => 'windows-1252',
        # with dash
        'utf-8'             => 'utf8',
        'utf-16'            => 'utf16',
        'utf-32'            => 'utf32',
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
    %map{$s} // %map{lc $s} // lc $s;
}

my class Str does Stringy { # declared in BOOTSTRAP
    # class Str is Cool {
    #     has str $!value is box_target;

    multi method WHY('Life, the Universe, and Everything':) { 42 }

    multi method WHICH(Str:D:) {
        nqp::box_s(
            nqp::concat(
                nqp::concat(nqp::unbox_s(self.^name), '|'),
                $!value
            ),
            ObjAt
        );
    }
    submethod BUILD(Str() :$value = '') {
        nqp::bindattr_s(self, Str, '$!value', nqp::unbox_s($value))
    }

    multi method Bool(Str:D:) {
        nqp::p6bool(nqp::chars($!value));
    }

    multi method Str(Str:D:)     { self }
    multi method Stringy(Str:D:) { self }
    multi method DUMP(Str:D:) { self.perl }

    method Int(Str:D:) { self.Numeric.Int; }
    method Num(Str:D:) { self.Numeric.Num; }

    multi method ACCEPTS(Str:D: Str:D \other) {
        nqp::p6bool(nqp::iseq_s(nqp::unbox_s(other),$!value));
    }
    multi method ACCEPTS(Str:D: Any:U \other) {
        False;
    }
    multi method ACCEPTS(Str:D: Any:D \other) {
        nqp::p6bool(nqp::iseq_s(nqp::unbox_s(other.Str),$!value));
    }

    method chomp(Str:D:) {
        my str $sself = nqp::unbox_s(self);
        my int $chars = nqp::chars($sself);
        return '' if $chars == 0;

        my int $last = nqp::ordat($sself, $chars - 1);
        if $last == 10 {
            if $chars > 1 && nqp::iseq_i(nqp::ordat($sself, $chars - 2),  13) {
                nqp::p6box_s(nqp::substr($sself, 0, $chars - 2));
            }
            else {
                nqp::p6box_s(nqp::substr($sself, 0, $chars - 1));
            }
        }
        elsif $last == 13 {
            nqp::p6box_s(nqp::substr($sself, 0, $chars - 1));
        }
        else {
            self;
        }
    }

    method chop(Str:D: Int() $chopping = 1) {
        my str $str   = nqp::unbox_s(self);
        my int $chars = nqp::chars($str) - $chopping;
        $chars > 0 ?? nqp::p6box_s(nqp::substr($str,0,$chars)) !! '';
    }

    # chars used to handle ranges for pred/succ
    my str $RANGECHAR =
        "01234567890"                                # arabic digits
        ~ "ABCDEFGHIJKLMNOPQRSTUVWXYZA"              # latin uppercase
        ~ "abcdefghijklmnopqrstuvwxyza"              # latin lowercase
        ~ "\x[391,392,393,394,395,396,397,398,399,39A,39B,39C,39D,39E,39F,3A0,3A1,3A3,3A4,3A5,3A6,3A7,3A8,3A9,391]" # greek uppercase
        ~ "\x[3B1,3B2,3B3,3B4,3B5,3B6,3B7,3B8,3B9,3BA,3BB,3BC,3BD,3BE,3BF,3C0,3C1,3C3,3C4,3C5,3C6,3C7,3C8,3C9,3B1]" # greek lowercase
        ~ "\x[5D0,5D1,5D2,5D3,5D4,5D5,5D6,5D7,5D8,5D9,5DA,5DB,5DC,5DD,5DE,5DF,5E0,5E1,5E2,5E3,5E4,5E5,5E6,5E7,5E8,5E9,5EA,5D0]" # hebrew
        ~ "\x[410,411,412,413,414,415,416,417,418,419,41A,41B,41C,41D,41E,41F,420,421,422,423,424,425,426,427,428,429,42A,42B,42C,42D,42E,42F,410]" # cyrillic uppercase
        ~ "\x[430,431,432,433,434,435,436,437,438,439,43A,43B,43C,43D,43E,43F,440,441,442,443,444,445,446,447,448,449,44A,44B,44C,44D,44E,44F,430]" # cyrillic lowercase
        ~ "\x[660,661,662,663,664,665,666,667,668,669,660]" # arabic-indic digits
        ~ "\x[966,967,968,969,96A,96B,96C,96D,96E,96F,966]" # devanagari digits
        ~ "\x[9E6,9E7,9E8,9E9,9EA,9EB,9EC,9ED,9EE,9EF,9E6]" # bengali digits
        ~ "\x[A66,A67,A68,A69,A6A,A6B,A6C,A6D,A6E,A6F,A66]" # gurmukhi digits
        ~ "\x[AE6,AE7,AE8,AE9,AEA,AEB,AEC,AED,AEE,AEF,AE6]" # gujarati digits
        ~ "\x[B66,B67,B68,B69,B6A,B6B,B6C,B6D,B6E,B6F,B66]" # oriya digits
        ~ "\x[FF10,FF11,FF12,FF13,FF14,FF15,FF16,FF17,FF18,FF19,FF10]" # fullwidth digits
        ~ "\x[2070,2071,00B2,00B3,2074,2075,2076,2077,2078,2079]" # superscripts
        ~ "\x[2080,2081,2082,2083,2084,2085,2086,2087,2088,2089]" # subscripts
        ~ "\x[2160,2161,2162,2163,2164,2165,2166,2167,2168,2169,216a,216b,2160]" # clock roman uc
        ~ "\x[2170,2171,2172,2173,2174,2175,2176,2177,2178,2179,217a,217b,2170]" # clock roman lc
        ~ "\x[2460,2461,2462,2463,2464,2465,2466,2467,2468,2469,246A,246B,246C,246D,246E,246F,2470,2471,2472,2473,2460]" # circled digits 1..20
        ~ "\x[2474,2475,2476,2477,2478,2479,247A,247B,247C,247D,247E,247F,2480,2481,2482,2483,2484,2485,2486,2487,2474]" # parenthesized digits 1..20
        ~ "\x[249C,249D,249E,249F,24A0,24A1,24A2,24A3,24A4,24A5,24A6,24A7,24A8,24A9,24AA,24AB,24AC,24AD,24AE,24AF,24B0,24B1,24B2,24B3,24B4,24B5,249C]" # parenthesized latin lc
        ~ "\x[2581,2582,2583,2584,2585,2586,2587,2588]" # lower blocks
        ~ "\x[2680,2681,2682,2683,2684,2685,2680]" # die faces
        ~ "\x[2776,2777,2778,2779,277A,277B,277C,277D,277E,277F,2776]" # dingbat negative circled 1..10
        ~ "\x[1F37A,1F37B]"  # beer mugs
        ~ "\x[1F42A,1F42B]"; # camels

    # digit to extend the string with if carried past first rangechar position
    my $carrydigit := nqp::hash(
       '0',      '1',      # arabic
       "\x0660", "\x0661", # arabic-indic
       "\x0966", "\x0967", # devanagari
       "\x09E6", "\x09E7", # bengali
       "\x0A66", "\x0A67", # gurmukhi
       "\x0AE6", "\x0AE7", # gujarati
       "\x0B66", "\x0B67", # oriya
       "\xFF10", "\xFF11", # fullwidth XXX: should be treated as digit?
       "\x2070", "\x2071", # superscripts XXX: should be treated as digit?
       "\x2080", "\x2081", # subscripts XXX: should be treated as digit?
       "\x1F37A","\x1F37B",# beer mugs
       "\x1F42A","\x1F42B",# camels
    );
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
        (0, -1);
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
            $str = nqp::replace($str, $r1, 1, $ch1);
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
            $str = nqp::replace($str, $r1, 1, $ch1);
            return $str if $ch1 gt $ch0;
            # carry to previous position
            $r1 = $r1 - 1;
            # extend string if carried past first rangechar position
            $str = nqp::replace($str, $r0, 0,
                       nqp::existskey($carrydigit, $ch1)
                           ?? nqp::atkey($carrydigit, $ch1)
                           !! $ch1)
                if $r1 < $r0;
        }
        $str;
    }

    multi method Numeric(Str:D: :$strict = True) {
        # Handle special empty string
        return 0 if self.trim eq "";

        val(self, :val-or-fail);
    }

    my %esc = (
        '$' => '\$',  '@' => '\@',  '%' => '\%',  '&' => '\&',  '{' => '\{',
        "\b" => '\b', "\n" => '\n', "\r" => '\r', "\t" => '\t', '"' => '\"',
        '\\' => '\\\\' );

    multi method gist(Str:D:) { self }
    multi method perl(Str:D:) {
        sub char-to-escapes($ch) {
#?if moar
            $ch.NFC.list.map(*.fmt('\x[%x]')).join
#?endif
#?if !moar
            $ch.ord.fmt('\x[%x]')
#?endif
        }

        # Under NFG-supporting implementations, must be sure that any leading
        # combiners are escaped, otherwise they will be combined onto the "
        # under concatenation closure, which ruins round-tripping.
        my $result = '"';
        my $to-encode = self;
        if nqp::chars($to-encode) {
            my int $opener = ord(self);
#?if moar
            if $opener >= 256 && +uniprop($opener, 'Canonical_Combining_Class') {
                $result ~= char-to-escapes(self.substr(0, 1));
                $to-encode = self.substr(1);
            }
#?endif
        }

        for ^$to-encode.chars -> $i {
            my $ch = substr($to-encode, $i, 1);
            $result ~= %esc{$ch}
                       //  (nqp::iscclass( nqp::const::CCLASS_PRINTING,
                                                  nqp::unbox_s($ch), 0)
                           ?? $ch
                           !! char-to-escapes($ch)
                           );
        }
        $result ~ '"'
    }

    role ProcessStr does Iterator {
        has str $!str;
        has int $!chars;
        submethod BUILD(\string) {
            $!str   = nqp::unbox_s(string);
            $!chars = nqp::chars($!str);
            self
        }
        method new(\string) { nqp::create(self).BUILD(string) }
    }

    multi method comb(Str:D:) {
        Seq.new(class :: does ProcessStr {
            has int $!pos;
            method pull-one() {
                $!pos < $!chars
                  ?? nqp::p6box_s(nqp::substr($!str, $!pos++, 1))
                  !! IterationEnd
            }
            method count-only() { nqp::p6box_i($!pos = $!chars) }
        }.new(self));
    }
    multi method comb(Str:D: Str $pat) {
        Seq.new(class :: does Iterator {
            has str $!str;
            has str $!pat;
            has int $!pos;
            submethod BUILD(\string, \pat) {
                $!str = nqp::unbox_s(string);
                $!pat = nqp::unbox_s(pat);
                self
            }
            method new(\string, \pat) { nqp::create(self).BUILD(string, pat) }
            method pull-one() {
                my int $found = nqp::index($!str, $!pat, $!pos);
                if $found < 0 {
                    IterationEnd
                }
                else {
                    $!pos = $found + 1;
                    nqp::p6box_s($!pat)
                }
            }
            method count-only() {
                my int $seen;
                my int $found;
                until ($found = nqp::index($!str, $!pat, $!pos)) < 0 {
                    $seen = $seen + 1;
                    $!pos = $found + 1;
                }
                nqp::p6box_i($seen)
            }
        }.new(self, $pat));
    }
    multi method comb(Str:D: Str $pat, $limit) {
        return self.comb($pat)
          if nqp::istype($limit,Whatever) || $limit == Inf;

        Seq.new(class :: does Iterator {
            has str $!str;
            has str $!pat;
            has int $!pos;
            has int $!todo;
            submethod BUILD(\string, \pat, \limit) {
                $!str  = nqp::unbox_s(string);
                $!pat  = nqp::unbox_s(pat);
                $!todo = nqp::unbox_i(limit.Int);
                self
            }
            method new(\string, \pat, \limit) {
                nqp::create(self).BUILD(string, pat, limit)
            }
            method pull-one() {
                my int $found = nqp::index($!str, $!pat, $!pos);
                if $found < 0 || $!todo == 0 {
                    IterationEnd
                }
                else {
                    $!pos  = $found + 1;
                    $!todo = $!todo - 1;
                    nqp::p6box_s($!pat)
                }
            }
            method count-only() {
                my int $seen;
                my int $found;
                until ($found = nqp::index($!str, $!pat, $!pos)) < 0
                  || $!todo == 0 {
                    $seen  = $seen + 1;
                    $!pos  = $found + 1;
                    $!todo = $!todo - 1;
                }
                nqp::p6box_i($seen)
            }
        }.new(self, $pat, $limit));
    }
    multi method comb(Str:D: Regex $pat, $limit = Inf, :$match) {
        my $x;
        $x = (1..$limit) unless nqp::istype($limit, Whatever) || $limit == Inf;
        $match
            ?? self.match(:g, :$x, $pat)
            !! self.match(:g, :$x, $pat).map: { .Str }
    }

    # A temporary routine for differential testing of .match overhead.
    # This can only be used for a single non-multi match.
    method simplematch($pat) {
        my $caller_dollar_slash := nqp::getlexcaller('$/');
        my %opts;
        %opts<c> = 0;
        my $patrx := nqp::istype($pat,Code) ?? $pat !! / "$pat": /;
        my $cur := $patrx(Cursor.'!cursor_init'(self, |%opts));

        my \result = $cur.pos >= 0
            ?? $cur.MATCH_SAVE
            !! Nil;
        $caller_dollar_slash = result;
        result;
    }

    method match($pat,
                 :continue(:$c), :pos(:$p),
                 :global(:$g), :overlap(:$ov), :exhaustive(:$ex),
                 # :st(:nd(:rd(:th($nth)))) is cute, but slow
                 :st(:$nd), :rd(:$th), :$nth = $nd // $th, :$x) {
        my $caller_dollar_slash := nqp::getlexcaller('$/');
        my %opts;
        if $p.defined { %opts<p> = $p }
        else { %opts<c> = $c // 0; }
        my $patrx := nqp::istype($pat,Code) ?? $pat !! / "$pat": /;
        my $cur := $patrx(Cursor.'!cursor_init'(self, |%opts));

        %opts<ov> = $ov if $ov;
        %opts<ex> = $ex if $ex;

        my $matches := gather {
            while $cur.pos >= 0 {
                take $cur.MATCH_SAVE;
                $cur := $cur.'!cursor_more'(|%opts);
            }
        }

        my $multi = $g || $ov || $ex;

        my @matches;

        if $nth.defined or $x.defined {
            my $clip;
            my $idxs;

            # Translate :nth lists to monotonic 0-based indices
            my sub nthidx($n is copy) {

                if nqp::istype($n, Callable) or nqp::istype($n, Whatever) {
                    # WhateverCode forces us to remember early
                    once @matches := $matches.list;
                    once $matches := Nil;
                    # WhateverCode is 1-based
                    $n = nqp::istype($n, Whatever) ?? +@matches !! $n(+@matches);
                }

                state $max = -Inf;

                if ($n > $max or once $n == -Inf) {
                    $max = $n;
                    # After first positive, <= 0 are "ignored" per spec
                    once die "Attempt to retrieve before :1st match -- :nth($n)"
                       if $max < 1;
                    $n - 1;
                }
                else {
                    Slip.new();
                }
            }
            if $nth.defined and not $x.defined {
                $multi = Positional.ACCEPTS($nth);
                $idxs := $nth.map(&nthidx).Array;
                $clip := $idxs.elems..Inf;
            }
            if $x.defined {
                $multi = True;
                if nqp::istype($x, Int) {
                    $clip := $x..$x;
                }
                elsif nqp::istype($x, Range) {
                    my $mx = $x.max.floor;
                    $mx = $mx - 1 unless $mx ~~ $x;
                    my $mn = $x.min.ceiling;
                    $mn = $mn + 1 unless $mn ~~ $x;
                    $clip := $mn..$mx;
                }
                elsif nqp::istype($x, Whatever) {
                    $clip := 0..Inf;
                }
                else {
                    X::Str::Match::x.new(:got($x)).fail;
                }
                $clip := 0..($clip.max) if $clip.min < 0;
                return Slip.new() if $clip.max < 1 or $clip.max < $clip.min;

                if $nth.defined {
                    $idxs := $nth.map(&nthidx).Array;
                    return Slip.new()
                        if $clip.min and not $idxs.EXISTS-POS($clip.min - 1);
                }
                else {
                    $idxs := (0..Inf).Array;
                }
            }

            unless $matches.defined {
                # Whatever, we have an extra layer of memoization.
                $matches := @matches.values;
                @matches := ();
            }

            # Just "list $matches.grep", once we have True.last
            @matches := (gather do for $matches -> $m {
                state $i = 0;
                state $took = 0;
                state $n = $idxs.EXISTS-POS(0) ?? $idxs.shift !! Nil;
                last unless $n.defined;

                if $i == $n {
                    $n = $idxs.EXISTS-POS(0) ?? $idxs.shift !! Nil;
                    take $m;
                    $took++;
                    last if $took >= $clip.max;
                };
                $i++;

                last unless $n.defined;
            }).cache;
            @matches := () unless not $clip.min or @matches.EXISTS-POS($clip.min - 1);
        }
        else {
            @matches := $matches.list;
        }
        if $multi {
            if nqp::istype($pat, Regex) {
                try $caller_dollar_slash = +@matches
                    ?? @matches[ +@matches - 1 ]
                    !! Cursor.'!cursor_init'(nqp::unbox_s('self')).'!cursor_start_cur'().MATCH;
            }
            $caller_dollar_slash = @matches;
            @matches
        }
        else {
            try $caller_dollar_slash = (@matches[0] // $cur.MATCH_SAVE);
            (@matches[0] // $cur.MATCH_SAVE)
        }
    }

    multi method subst-mutate($self is rw: $matcher, $replacement,
                       :ii(:$samecase), :ss(:$samespace),
                       :$SET_CALLER_DOLLAR_SLASH, *%options) {
        my $global = %options<g> || %options<global>;
        my $caller_dollar_slash := nqp::getlexcaller('$/');
        my $SET_DOLLAR_SLASH     = $SET_CALLER_DOLLAR_SLASH || nqp::istype($matcher, Regex);

        try $caller_dollar_slash = $/ if $SET_DOLLAR_SLASH;
        my @matches              = self.match($matcher, |%options);

        if !@matches || (@matches == 1 && !@matches[0]) {
            Nil;
        }
        else {
            $self = $self!APPLY-MATCHES(
              @matches,
              $replacement,
              $caller_dollar_slash,
              $SET_DOLLAR_SLASH,
              $samecase,
              $samespace,
            );
            if $global {
                my \result := List.CREATE;
                nqp::bindattr(result, List, '$!reified', nqp::getattr(@matches, List, '$!reified'));
                result
            }
            else {
                @matches[0]
            }
        }
    }

    multi method subst(Str:D: Str \from, Str \to, :$global!, *%adverbs) {
        if $global && !%adverbs {
            TRANSPOSE(self,from,to);
        }
        else {
            $/ := nqp::getlexdyn('$/');
            self.subst(from, to, |%adverbs);
        }
    }
    multi method subst(Str:D: $matcher, $replacement,
                       :ii(:$samecase), :ss(:$samespace),
                       :$SET_CALLER_DOLLAR_SLASH, *%options) {

        my $caller_dollar_slash := nqp::getlexcaller('$/');
        my $SET_DOLLAR_SLASH     = $SET_CALLER_DOLLAR_SLASH || nqp::istype($matcher, Regex);

        # nothing to do
        try $caller_dollar_slash = $/ if $SET_DOLLAR_SLASH;
        my @matches = self.match($matcher, |%options);

        !@matches || (@matches == 1 && !@matches[0])
          ?? self
          !! self!APPLY-MATCHES(
               @matches,
               $replacement,
               $caller_dollar_slash,
               $SET_DOLLAR_SLASH,
               $samecase,
               $samespace,
             );
    }

    method !APPLY-MATCHES(\matches,$replacement,\cds,\SDS,\case,\space) {
        my \callable       := nqp::istype($replacement,Callable);

        my int $prev;
        my str $str    = nqp::unbox_s(self);
        my Mu $result := nqp::list_s();
        try cds = $/ if SDS;

        # need to do something special
        if SDS || case || space || callable {
            my \noargs         := callable ?? $replacement.count == 0 !! False;
            my \case-or-space  := case || space;
            my \case-and-space := case && space;

            for flat matches -> $m {
                try cds = $m if SDS;
                nqp::push_s(
                  $result,nqp::substr($str,$prev,nqp::unbox_i($m.from) - $prev)
                );

                if case-or-space {
                    my $it := ~(callable
                      ?? (noargs ?? $replacement() !! $replacement($m))
                      !! $replacement
                    );
                    if case-and-space {
                        my $mstr := $m.Str;
                        nqp::push_s($result,nqp::unbox_s(
                          $it.samecase($mstr).samespace($mstr)
                        ) );
                    }
                    elsif case {
                        nqp::push_s($result,nqp::unbox_s($it.samecase(~$m)));
                    }
                    else { # space
                        nqp::push_s($result,nqp::unbox_s($it.samespace(~$m)));
                    }
                }
                else {
                    nqp::push_s($result,nqp::unbox_s( ~(callable
                      ?? (noargs ?? $replacement() !! $replacement($m))
                      !! $replacement
                    ) ) );
                }
                $prev = nqp::unbox_i($m.to);
            }
            nqp::push_s($result,nqp::substr($str,$prev));
            nqp::p6box_s(nqp::join('',$result));
        }

        # simple string replacement
        else {
            for flat matches -> $m {
                nqp::push_s(
                  $result,nqp::substr($str,$prev,nqp::unbox_i($m.from) - $prev)
                );
                $prev = nqp::unbox_i($m.to);
            }
            nqp::push_s($result,nqp::substr($str,$prev));
            nqp::p6box_s(nqp::join(nqp::unbox_s(~$replacement),$result));
        }
    }

    method ords(Str:D:) {
        Seq.new(class :: does ProcessStr {
            has int $!pos;
            method pull-one() {
                $!pos < $!chars
                  ?? nqp::p6box_i(nqp::ordat($!str, $!pos++))
                  !! IterationEnd
            }
        }.new(self));
    }

    # constant ???
    my str $CRLF = nqp::unbox_s("\r\n");

    multi method lines(Str:D: :$count!) {
        # we should probably deprecate this feature
        $count ?? self.lines.elems !! self.lines;
    }
    multi method lines(Str:D: $limit) {
        # we should probably deprecate this feature
        nqp::istype($limit,Whatever) || $limit == Inf
          ?? self.lines
          !! self.lines[ 0 .. $limit.Int - 1 ]
    }
    multi method lines(Str:D:) {
        Seq.new(class :: does ProcessStr {
            has int $!pos;
            method pull-one() {
                my int $left;
                my int $nextpos;
                my int $length;

                if ($left = $!chars - $!pos) > 0 {
                    $nextpos = nqp::findcclass(
                      nqp::const::CCLASS_NEWLINE, $!str, $!pos, $left);

                    my str $found = ($length = $nextpos - $!pos)
                      ?? nqp::substr($!str, $!pos, $length )
                      !! '';
                    $!pos = $nextpos + 1 + nqp::eqat($!str, $CRLF, $nextpos);

                    return nqp::p6box_s($found);
                }
                IterationEnd
            }
            method push-exactly($target, int $n) {
                my int $found;
                my int $left;
                my int $nextpos;
                my int $length;

                while ($left = $!chars - $!pos) > 0 {
                    $nextpos = nqp::findcclass(
                      nqp::const::CCLASS_NEWLINE, $!str, $!pos, $left);

                    $target.push(nqp::p6box_s( ($length = $nextpos - $!pos)
                      ?? nqp::substr($!str, $!pos, $length)
                      !! ''
                    ));
                    $!pos = $nextpos + 1 + nqp::eqat($!str, $CRLF, $nextpos);

                    $found = $found + 1;
                    return nqp::p6box_i($found) if $found == $n;
                }
                nqp::p6box_i($found)
            }
            method push-all($target) {
                my int $left;
                my int $nextpos;
                my int $length;

                while ($left = $!chars - $!pos) > 0 {
                    $nextpos = nqp::findcclass(
                      nqp::const::CCLASS_NEWLINE, $!str, $!pos, $left);

                    $target.push(nqp::p6box_s( ($length = $nextpos - $!pos)
                      ?? nqp::substr($!str, $!pos, $length)
                      !! ''
                    ));
                    $!pos = $nextpos + 1 + nqp::eqat($!str, $CRLF, $nextpos);
                }
                IterationEnd
            }
            method count-only() {
                my int $found;
                my int $left;
                my int $nextpos;

                while ($left = $!chars - $!pos) > 0 {
                    $nextpos = nqp::findcclass(
                      nqp::const::CCLASS_NEWLINE, $!str, $!pos, $left);

                    $found = $found + 1;
                    $!pos = $nextpos + 1 + nqp::eqat($!str, $CRLF, $nextpos);
                }
                nqp::p6box_i($found)
            }
        }.new(self));
    }

    multi method split(Str:D: Regex $pat, $limit = *;; :$all) {
        return ().list
          if nqp::istype($limit,Numeric) && $limit <= 0;
        my \matches = nqp::istype($limit, Whatever)
          ?? self.match($pat, :g)
          !! self.match($pat, :x(1..$limit-1), :g);

        my $prev-pos = 0;

        if ($all) {
            my $elems = +matches;
            map {
                my $value = substr(self,$prev-pos, .from - $prev-pos);
                $prev-pos = .to;
                # we don't want the dummy object
                $elems-- ?? Slip.new($value, $_) !! $value;
            }, flat matches, Match.new( :from(self.chars) );
            #                ^-- add dummy for last
        }
        else {
            map {
                my $value = substr(self, $prev-pos, .from - $prev-pos);
                $prev-pos = .to;
                $value;
            }, flat matches, Match.new( :from(self.chars) );
            #                ^-- add dummy for last
        }
    }

    multi method split(Str:D: Cool $delimiter, $limit = *;; :$all) {
        my $delim-str        = $delimiter.Str;
        my str $self-string  = self;
        my str $match-string = $delim-str;
        return ().list unless nqp::chars($self-string) || nqp::chars($match-string);

        my int $l = nqp::istype($limit, Whatever) || $limit == Inf
            ?? nqp::chars($self-string) + 1
            !! $limit.Int;
        return ().list     if $l <= 0;
        return (self).list if $l == 1;

        my int $c = 0;
        my int $done = 0;
        if nqp::chars($match-string) {
            my int $width = nqp::chars($match-string);
            map {
                last if $done;

                my int $m = nqp::index($self-string, $match-string, $c);
                if $m >= 0 and ($l = $l - 1) {
                    my \value = nqp::p6box_s(nqp::substr($self-string, $c, $m - $c));
                    $c = $m + $width;
                    $all ?? Slip.new(value, $match-string) !! value;
                }
                else {
                    $done = 1;
                    nqp::p6box_s(nqp::substr($self-string, $c));
                }
            }, 1 .. $l;
        } else {
            # Add one more to the limit to get the trailing empty string
            $l = $l + 1 if nqp::istype($limit, Whatever) || $limit == Inf;
            my int $chars = nqp::chars($self-string);
            map {
                last if $done;

                if $_ == 1 {
                    $l = $l - 1;
                    ""
                } elsif $chars > 0 && $l > 1 {
                    $chars = $chars - 1; $l = $l - 1;
                    my \value = nqp::p6box_s(nqp::substr($self-string, $c, 1));
                    $c = $c + 1;
                    value
                }
                else {
                    $done = 1;
                    nqp::p6box_s(nqp::substr($self-string, $c));
                }
            }, 1 .. $l;
        }
    }

    method samecase(Str:D: Str $pattern) {
        my str $str = nqp::unbox_s(self);
        my str $pat = nqp::unbox_s($pattern);
        my int $min = min(nqp::chars($str),nqp::chars($pattern));
        my int $i = 0;
        my int $j = 0;
        my int $case = 0;
        my int $last-case;
        my Mu $ret := nqp::list_s();
        my str $substr;
        while $i < $min {
            repeat {
                $last-case = $case;
                $case = nqp::iscclass(nqp::const::CCLASS_LOWERCASE, $pat, $j) ?? 1 !!
                        nqp::iscclass(nqp::const::CCLASS_UPPERCASE, $pat, $j) ?? 2 !! 0;
                last if $case != $last-case;
                $j = $j + 1;
            } while $j < $min;
            $substr = nqp::substr($str, $i, $j - $i);
            nqp::push_s($ret, $last-case == 1 ?? nqp::lc($substr) !!
                              $last-case == 2 ?? nqp::uc($substr) !! $substr);
            $i = $j
        }
        $substr = nqp::substr($str,$i);
        nqp::push_s($ret, $case == 1 ?? nqp::lc($substr) !!
                          $case == 2 ?? nqp::uc($substr) !! $substr);
        nqp::join("",$ret);
    }


    method samespace(Str:D: Str:D $pat) {
        my @self-chunks = self.split(rx/\s+/, :all).flat;
        my @pat-chunks  = $pat.split(rx/\s+/, :all).flat;
        loop (my $i = 1; $i < @pat-chunks && $i < @self-chunks; $i += 2) {
            @self-chunks[$i] = @pat-chunks[$i];
        }
        @self-chunks.join;
    }

    method trim-leading(Str:D:) {
        my str $str = nqp::unbox_s(self);
        my int $pos = nqp::findnotcclass(
                          nqp::const::CCLASS_WHITESPACE,
                          $str, 0, nqp::chars($str));
        $pos ?? nqp::p6box_s(nqp::substr($str, $pos)) !! self;
    }

    method trim-trailing(Str:D:) {
        my str $str = nqp::unbox_s(self);
        my int $pos = nqp::chars($str) - 1;
        $pos = $pos - 1
            while nqp::isge_i($pos, 0)
               && nqp::iscclass(nqp::const::CCLASS_WHITESPACE, $str, $pos);
        nqp::islt_i($pos, 0) ?? '' !! nqp::p6box_s(nqp::substr($str, 0, $pos + 1));
    }

    method trim(Str:D:) {
        my str $str  = nqp::unbox_s(self);
        my int $pos  = nqp::chars($str) - 1;
        my int $left = nqp::findnotcclass(
                           nqp::const::CCLASS_WHITESPACE, $str, 0, $pos + 1);
        $pos = $pos - 1
            while nqp::isge_i($pos, $left)
               && nqp::iscclass(nqp::const::CCLASS_WHITESPACE, $str, $pos);
        nqp::islt_i($pos, $left) ?? '' !! nqp::p6box_s(nqp::substr($str, $left, $pos + 1 - $left));
    }

    multi method words(Str:D: :$autoderef!) { # in Actions.postprocess_words
        my @list = self.words;
        return @list == 1 ?? @list[0] !! @list;
    }
    multi method words(Str:D: $limit) {
        # we should probably deprecate this feature
        nqp::istype($limit,Whatever) || $limit == Inf
          ?? self.words
          !! self.words[ 0 .. $limit.Int - 1 ]
    }
    multi method words(Str:D:) {
        Seq.new(class :: does Iterator {
            has str $!str;
            has int $!chars;
            has int $!pos;

            submethod BUILD(\string) {
                $!str   = nqp::unbox_s(string);
                $!chars = nqp::chars($!str);
                $!pos   = nqp::findnotcclass(
                  nqp::const::CCLASS_WHITESPACE, $!str, 0, $!chars);
                self
            }
            method new(\string) { nqp::create(self).BUILD(string) }
            method pull-one() {
                my int $left;
                my int $nextpos;

                if ($left = $!chars - $!pos) > 0 {
                    $nextpos = nqp::findcclass(
                      nqp::const::CCLASS_WHITESPACE, $!str, $!pos, $left);

                    my str $found =
                      nqp::substr($!str, $!pos, $nextpos - $!pos);
                    $!pos = nqp::findnotcclass( nqp::const::CCLASS_WHITESPACE,
                      $!str, $nextpos, $!chars - $nextpos);

                    return nqp::p6box_s($found);
                }
                IterationEnd
            }
            method push-exactly($target, int $n) {
                my int $found;
                my int $left;
                my int $nextpos;

                while ($left = $!chars - $!pos) > 0 {
                    $nextpos = nqp::findcclass(
                      nqp::const::CCLASS_WHITESPACE, $!str, $!pos, $left);

                    $target.push(nqp::p6box_s(
                      nqp::substr($!str, $!pos, $nextpos - $!pos)
                    ));
                    $!pos = nqp::findnotcclass( nqp::const::CCLASS_WHITESPACE,
                      $!str, $nextpos, $!chars - $nextpos);

                    $found = $found + 1;
                    return nqp::p6box_i($found) if $found == $n;
                }
                nqp::p6box_i($found)
            }
            method push-all($target) {
                my int $left;
                my int $nextpos;

                while ($left = $!chars - $!pos) > 0 {
                    $nextpos = nqp::findcclass(
                      nqp::const::CCLASS_WHITESPACE, $!str, $!pos, $left);

                    $target.push(nqp::p6box_s(
                      nqp::substr($!str, $!pos, $nextpos - $!pos)
                    ));
                    $!pos = nqp::findnotcclass( nqp::const::CCLASS_WHITESPACE,
                      $!str, $nextpos, $!chars - $nextpos);
                }
                IterationEnd
            }
            method count-only() {
                my int $found;
                my int $left;
                my int $nextpos;

                while ($left = $!chars - $!pos) > 0 {
                    $nextpos = nqp::findcclass(
                      nqp::const::CCLASS_WHITESPACE, $!str, $!pos, $left);

                    $found = $found + 1;
                    $!pos = nqp::findnotcclass( nqp::const::CCLASS_WHITESPACE,
                      $!str, $nextpos, $!chars - $nextpos);
                }
                nqp::p6box_i($found)
            }
        }.new(self));
    }

    my %enc_type = utf8 => utf8, utf16 => utf16, utf32 => utf32;
    method encode(Str:D $encoding = 'utf8') {
        my $enc      := NORMALIZE_ENCODING($encoding);
        my $enc_type := %enc_type.EXISTS-KEY($enc) ?? %enc_type{$enc} !! blob8;
        nqp::encode(nqp::unbox_s(self), nqp::unbox_s($enc), nqp::decont($enc_type.new))
    }

#?if moar
    method NFC() {
        nqp::strtocodes(nqp::unbox_s(self), nqp::const::NORMALIZE_NFC, nqp::create(NFC))
    }
    method NFD() {
        nqp::strtocodes(nqp::unbox_s(self), nqp::const::NORMALIZE_NFD, nqp::create(NFD))
    }
    method NFKC() {
        nqp::strtocodes(nqp::unbox_s(self), nqp::const::NORMALIZE_NFKC, nqp::create(NFKC))
    }
    method NFKD() {
        nqp::strtocodes(nqp::unbox_s(self), nqp::const::NORMALIZE_NFKD, nqp::create(NFKD))
    }
#?endif

    method wordcase(Str:D: :&filter = &tclc, Mu :$where = True) {
        self.subst(:g, / [<:L> \w* ] +% <['\-]> /, -> $m {
            my Str $s = $m.Str;
            $s ~~ $where ?? filter($s) !! $s;
        });
    }

    my class LSM {
        has Str $!source;
        has @!substitutions;
        has $!squash;
        has $!complement;

        has int $!index;
        has int $!next_match;
        has $!first_substitution; # need this one for :c with arrays
        has $!next_substitution;
        has $!substitution_length;
        has $!prev_result;
        has $!match_obj;
        has $!last_match_obj;

        has str $.unsubstituted_text;
        has str $.substituted_text;

        submethod BUILD(:$!source, :$!squash, :$!complement) { }

        method add_substitution($key, $value) {
            $/ := CALLERS::('$/');
            push @!substitutions, $key => $value;
        }

        submethod compare_substitution($substitution, Int $pos, Int $length) {
            $/ := CALLERS::('$/');
            if $!next_match > $pos
               || $!next_match == $pos && $!substitution_length < $length {

                $!next_match = $pos;
                $!substitution_length = $length;
                $!next_substitution = $substitution;
                $!match_obj = $!last_match_obj;
            }
        }

        proto method triage_substitution(|) {*}
        multi method triage_substitution($_ where { nqp::istype(.key,Regex) }) {
            $/ := CALLERS::('$/');
            my $m := $!source.match(.key, :continue($!index));
            return unless $m;
            $!last_match_obj = $/;
            self.compare_substitution($_, $m.from, $m.to - $m.from);
            True
        }

        multi method triage_substitution($_ where { nqp::istype(.key,Cool) }) {
            $/ := CALLERS::('$/');
            my $pos := index($!source, .key, $!index);
            return unless defined $pos;
            self.compare_substitution($_, $pos, .key.chars);
            True
        }

        multi method triage_substitution($_) {
            X::Str::Trans::IllegalKey.new(key => $_).throw;
        }

        proto method increment_index(|) {*}
        multi method increment_index(Regex $s) {
            $/ := CALLERS::('$/');
            substr($!source,$!index) ~~ $s;
            $!last_match_obj = $/;
            $!index = $!next_match + $/.chars;
        }

        multi method increment_index(Cool $s) {
            $/ := CALLERS::('$/');
            $!index = $!next_match + nqp::chars($s.Str);
        }

        method get_next_substitution_result {
            my $result = $!complement ?? $!first_substitution.value !! $!next_substitution.value;
            my $cds := CALLERS::('$/');
            $/ := CALLERS::('$/');
            $cds = $!match_obj;
            my $orig-result = $result = ($result ~~ Callable ?? $result() !! $result).Str;
            if $!prev_result
                && $!prev_result eq $result
                && $!unsubstituted_text eq ''
                && $!squash {
                $result = '';
            }
            $!prev_result = $orig-result;
            nqp::unbox_s($result)
        }

        method next_substitution() {
            $/ := CALLERS::('$/');
            $!next_match = $!source.chars;
            $!first_substitution //= @!substitutions[0];

            # triage_substitution has a side effect!
            @!substitutions = @!substitutions.grep: { self.triage_substitution($_) }

            $!unsubstituted_text # = nqp::substr(nqp::unbox_s($!source), $!index,
                = substr($!source,$!index, $!next_match - $!index);
            if defined $!next_substitution {
                if $!complement {
                    my $oldidx = $!index;
                    if $!unsubstituted_text {
                        my $result = self.get_next_substitution_result;
                        self.increment_index($!next_substitution.key);
                        $!substituted_text = substr($!source,$oldidx + $!unsubstituted_text.chars,
                            $!index - $oldidx - $!unsubstituted_text.chars);
                        $!unsubstituted_text = $!squash ?? $result
                            !! $result x $!unsubstituted_text.chars;
                    }
                    else {
                        return if $!next_match == $!source.chars;
                        my $result = self.get_next_substitution_result;
                        self.increment_index($!next_substitution.key);
                        $!substituted_text = '';
                        $!unsubstituted_text = substr($!source,$oldidx, $!index - $oldidx);
                    }
                }
                else {
                    return if $!next_match == $!source.chars;
                    $!substituted_text = self.get_next_substitution_result;
                    self.increment_index($!next_substitution.key);
                }
            }

            return $!next_match < $!source.chars && @!substitutions;
        }
    }

    proto method trans(|) { $/ := nqp::getlexcaller('$/'); {*} }
    multi method trans(Pair:D \what, *%n) {
        my $from = what.key;
        my $to   = what.value;
        $/ := CALLERS::('$/');
        return self.trans(|%n, (what,))
          if !nqp::istype($from,Str)   # from not a string
          || !$from.defined            # or a type object
          || !nqp::istype($to,Str)     # or to not a string
          || !$to.defined              # or a type object
          || %n;                       # or any named params passed

        return TRANSPOSE-ONE(self, $from, substr($to,0,1))  # 1 char to 1 char
          if $from.chars == 1 && $to.chars;

        sub expand(Str:D \x) {
            my str $s     = nqp::unbox_s(x);
            my int $found = nqp::index($s,'..',1);
            return x       #  not found or at the end without trail
              if nqp::iseq_i($found,-1) || nqp::iseq_i($found,nqp::chars($s)-2);

            my int $from   = nqp::ordat($s,$found - 1);
            my int $to     = nqp::ordat($s,$found + 2);
            my Mu $result := nqp::list_s();

            nqp::push_s($result,nqp::substr($s,0,$found - 1));
            while nqp::isle_i($from,$to) {
                nqp::push_s($result,nqp::chr($from));
                $from = $from + 1;
            }
            nqp::push_s($result,nqp::substr($s,$found + 3));

            expand(nqp::p6box_s(nqp::join('',$result)));
        }

        my str $sfrom  = nqp::unbox_s(expand($from));
        my str $str    = nqp::unbox_s(self);
        my str $chars  = nqp::chars($str);
        my Mu $result := nqp::list_s();
        my str $check;
        my int $i;

        # something to convert to
        if $to.chars -> $tochars {
            nqp::setelems($result,$chars);

            # all convert to one char
            if $tochars == 1 {
                my str $sto = nqp::unbox_s($to);

                while nqp::islt_i($i,$chars) {
                    $check = nqp::substr($str,$i,1);
                    nqp::bindpos_s(
                      $result, $i, nqp::iseq_i(nqp::index($sfrom,$check),-1)
                        ?? $check
                        !! $sto
                    );
                    $i = $i + 1;
                }
            }

            # multiple chars to convert to
            else {
                my str $sto   = nqp::unbox_s(expand($to));
                my int $sfl   = nqp::chars($sfrom);
                my int $found;

                # repeat until mapping complete
                $sto = $sto ~ $sto while nqp::islt_i(nqp::chars($sto),$sfl);

                while nqp::islt_i($i,$chars) {
                    $check = nqp::substr($str,$i,1);
                    $found = nqp::index($sfrom,$check);
                    nqp::bindpos_s($result, $i, nqp::iseq_i($found,-1)
                      ?? $check
                      !! nqp::substr($sto,$found,1)
                    );
                    $i = $i + 1;
                }
            }
        }

        # just remove
        else {
            while nqp::islt_i($i,$chars) {
                $check = nqp::substr($str,$i,1);
                nqp::push_s($result, $check)
                  if nqp::iseq_i(nqp::index($sfrom,$check),-1);
                $i = $i + 1;
            }
        }

        nqp::p6box_s(nqp::join('',$result));
    }
    multi method trans(Str:D: *@changes, :complement(:$c), :squash(:$s), :delete(:$d)) {
        my sub myflat(*@s) { @s.map: { nqp::istype($_, Iterable) ?? .list.Slip !! $_ } }
        my sub expand($s) {
            return myflat($s.list).Slip
              if nqp::istype($s,Iterable) || nqp::istype($s,Positional);
            flat $s.comb(/ (\w) '..' (\w) | . /, :match).map: {
                flat(.[0] ?? ~.[0] .. ~.[1] !! ~$_).Slip
            };
        }

        $/ := CALLERS::('$/');
        my $lsm = LSM.new(:source(self), :squash($s), :complement($c));
        for @changes -> $p {
            X::Str::Trans::InvalidArg.new(got => $p).throw
              unless nqp::istype($p,Pair);
            if nqp::istype($p.key,Regex) {
                $lsm.add_substitution($p.key, $p.value);
            }
            elsif nqp::istype($p.value,Callable) {
                my @from = expand $p.key;
                for @from -> $f {
                    $lsm.add_substitution($f, $p.value);
                }
            }
            else {
                my @from = expand $p.key;
                my @to = expand $p.value;
                if @to {
                    my $padding = $d ?? '' !! @to[@to - 1];
                    @to = flat @to, $padding xx @from - @to;
                }
                else {
                    @to = '' xx @from
                }
                for flat @from Z @to -> $f, $t {
                    $lsm.add_substitution($f, $t);
                }
            }
        }

        my Mu $ret := nqp::list_s();
        while $lsm.next_substitution {
            nqp::push_s($ret, nqp::unbox_s($lsm.unsubstituted_text));
            nqp::push_s($ret, nqp::unbox_s($lsm.substituted_text));
        }
        nqp::push_s($ret, nqp::unbox_s($lsm.unsubstituted_text));
        return nqp::join('', $ret);
    }
    proto method indent($) {*}
    # Zero indent does nothing
    multi method indent(Int() $steps where { $_ == 0 }) {
        self;
    }

    # Positive indent does indent
    multi method indent(Int() $steps where { $_ > 0 }) {
    # We want to keep trailing \n so we have to .comb explicitly instead of .lines
        self.comb(/:r ^^ \N* \n?/).map({
            given $_.Str {
                when /^ \n? $ / {
                    $_;
                }
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

    # Negative indent (outdent)
    multi method indent(Int() $steps where { $_ < 0 }) {
        outdent(self, $steps);
    }

    # Whatever indent (outdent)
    multi method indent(Whatever $steps) {
        outdent(self, $steps);
    }

    sub outdent($obj, $steps) {
        # Loop through all lines to get as much info out of them as possible
        my @lines = $obj.comb(/:r ^^ \N* \n?/).map({
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
            }).eager;

            { :$indent-size, :@indent-chars, :rest(~$rest) };
        });

        # Figure out the amount * should outdent by, we also use this for warnings
        my $common-prefix = min @lines.grep({ .<indent-size> ||  .<rest> ~~ /\S/}).map({ $_<indent-size> });
        return $obj if $common-prefix === Inf;

        # Set the actual outdent amount here
        my Int $outdent = nqp::istype($steps,Whatever)
          ?? $common-prefix
          !! -$steps;

        warn "Asked to remove $outdent spaces, but the shortest indent is $common-prefix spaces"
            if $outdent > $common-prefix;

        # Work forwards from the left end of the indent whitespace, removing
        # array elements up to # (or over, in the case of tab-explosion)
        # the specified outdent amount.
        @lines.map(-> $l {
            my $pos = 0;
            while $l<indent-chars> and $pos < $outdent {
                if $l<indent-chars>.shift.key eq "\t" {
                    $pos -= $pos % $?TABSTOP;
                    $pos += $?TABSTOP;
                } else {
                    $pos++
                }
            }
            if $l<indent-chars> and $pos % $?TABSTOP {
                my $check = $?TABSTOP - $pos % $?TABSTOP;
                $check = $l<indent-chars>[0..^$check].first-index({$_.key eq "\t"});
                if $check.defined {
                    $l<indent-chars>.shift for 0..$check;
                    $pos -= $pos % $?TABSTOP;
                    $pos += $?TABSTOP;
                }
            }
            $l<indent-chars>».key.join ~ ' ' x ($pos - $outdent) ~ $l<rest>;
        }).join;
    }

    proto method codes(|) { * }
    multi method codes(Str:D:) returns Int:D {
#?if moar
        self.NFC.codes
#?endif
#?if jvm
        nqp::p6box_i(nqp::chars(nqp::unbox_s(self)))
#?endif
    }
    multi method codes(Str:U:) returns Int:D {
        self.Str;  # generate undefined warning
        0
    }

    proto method chars(|) { * }
    multi method chars(Str:D:) returns Int:D {
        nqp::p6box_i(nqp::chars($!value))
    }
    multi method chars(Str:U:) returns Int:D {
        self.Str;  # generate undefined warning
        0
    }

    proto method uc(|) { * }
    multi method uc(Str:D:) {
        nqp::p6box_s(nqp::uc($!value));
    }
    multi method uc(Str:U:) {
        self.Str;
    }

    proto method lc(|) { * }
    multi method lc(Str:D:) {
        nqp::p6box_s(nqp::lc($!value));
    }
    multi method lc(Str:U:) {
        self.Str;
    }

    proto method tc(|) { * }
    multi method tc(Str:D:) {
        nqp::p6box_s(nqp::tc(nqp::substr($!value,0,1)) ~ nqp::substr($!value,1));
    }
    multi method tc(Str:U:) {
        self.Str
    }

    proto method fc(|) { * }
    multi method fc(Str:D:) {
        nqp::p6box_s(nqp::fc($!value));
    }
    multi method fc(Str:U:) {
        self.Str;
    }

    proto method tclc(|) { * }
    multi method tclc(Str:D:) {
        nqp::p6box_s(nqp::tclc($!value))
    }
    multi method tclc(Str:U:) {
        self.Str
    }

    proto method flip(|) { * }
    multi method flip(Str:D:) {
        nqp::p6box_s(nqp::flip($!value))
    }
    multi method flip(Str:U:) {
        self.Str
    }

    proto method ord(|) { * }
    multi method ord(Str:D:) returns Int {
        nqp::chars($!value)
          ?? nqp::p6box_i(nqp::ord($!value))
          !! Int;
    }
    multi method ord(Str:U:) {
        self.Str;
        Int
    }

}


multi sub prefix:<~>(Str:D \a)  returns Str:D { a }
multi sub prefix:<~>(str $a)    returns str   { $a }

multi sub infix:<~>(Str:D \a, Str:D \b) returns Str:D {
    nqp::p6box_s(nqp::concat(nqp::unbox_s(a), nqp::unbox_s(b)))
}
multi sub infix:<~>(str $a, str $b) returns str { nqp::concat($a, $b) }
multi sub infix:<~>(*@args) returns Str:D { @args.join }

multi sub infix:<x>(Str:D $s, Int:D $repetition) returns Str:D {
    $repetition < 0
        ?? ''
        !!  nqp::p6box_s(nqp::x(nqp::unbox_s($s), nqp::unbox_i($repetition)))
}
multi sub infix:<x>(str $s, int $repetition) returns str {
    nqp::if(nqp::islt_i($repetition, 0), '', nqp::x($s, $repetition))
}

multi sub infix:<cmp>(Str:D \a, Str:D \b) returns Order:D {
    ORDER(nqp::cmp_s(nqp::unbox_s(a), nqp::unbox_s(b)))
}
multi sub infix:<cmp>(str $a, str $b) returns Order:D {
    ORDER(nqp::cmp_s($a, $b))
}

multi sub infix:<===>(Str:D \a, Str:D \b) returns Bool:D {
    nqp::p6bool(nqp::iseq_s(nqp::unbox_s(a), nqp::unbox_s(b)))
}
multi sub infix:<===>(str $a, str $b) returns Bool:D {
    nqp::p6bool(nqp::iseq_s($a, $b))
}

multi sub infix:<leg>(Str:D \a, Str:D \b) returns Order:D {
    ORDER(nqp::cmp_s(nqp::unbox_s(a), nqp::unbox_s(b)))
}
multi sub infix:<leg>(str $a, str $b) returns Order:D {
    ORDER(nqp::cmp_s($a, $b))
}

multi sub infix:<eq>(Str:D \a, Str:D \b) returns Bool:D {
    nqp::p6bool(nqp::iseq_s(nqp::unbox_s(a), nqp::unbox_s(b)))
}
multi sub infix:<eq>(str $a, str $b) returns Bool:D {
    nqp::p6bool(nqp::iseq_s($a, $b))
}

multi sub infix:<ne>(Str:D \a, Str:D \b) returns Bool:D {
    nqp::p6bool(nqp::isne_s(nqp::unbox_s(a), nqp::unbox_s(b)))
}
multi sub infix:<ne>(str $a, str $b) returns Bool:D {
    nqp::p6bool(nqp::isne_s($a, $b))
}

multi sub infix:<lt>(Str:D \a, Str:D \b) returns Bool:D {
    nqp::p6bool(nqp::islt_s(nqp::unbox_s(a), nqp::unbox_s(b)))
}
multi sub infix:<lt>(str $a, str $b) returns Bool:D {
    nqp::p6bool(nqp::islt_s($a, $b))
}

multi sub infix:<le>(Str:D \a, Str:D \b) returns Bool:D {
    nqp::p6bool(nqp::isle_s(nqp::unbox_s(a), nqp::unbox_s(b)))
}
multi sub infix:<le>(str $a, str $b) returns Bool:D {
    nqp::p6bool(nqp::isle_s($a, $b))
}

multi sub infix:<gt>(Str:D \a, Str:D \b) returns Bool:D {
    nqp::p6bool(nqp::isgt_s(nqp::unbox_s(a), nqp::unbox_s(b)))
}
multi sub infix:<gt>(str $a, str $b) returns Bool:D {
    nqp::p6bool(nqp::isgt_s($a, $b))
}

multi sub infix:<ge>(Str:D \a, Str:D \b) returns Bool:D {
    nqp::p6bool(nqp::isge_s(nqp::unbox_s(a), nqp::unbox_s(b)))
}
multi sub infix:<le>(str $a, str $b) returns Bool:D {
    nqp::p6bool(nqp::isle_s($a, $b))
}

multi sub infix:<~|>(Str:D \a, Str:D \b) returns Str:D {
    nqp::p6box_s(nqp::bitor_s(nqp::unbox_s(a), nqp::unbox_s(b)))
}
multi sub infix:<~|>(str $a, str $b) returns str { nqp::bitor_s($a, $b) }

multi sub infix:<~&>(Str:D \a, Str:D \b) returns Str:D {
    nqp::p6box_s(nqp::bitand_s(nqp::unbox_s(a), nqp::unbox_s(b)))
}
multi sub infix:<~&>(str $a, str $b) returns str { nqp::bitand_s($a, $b) }

multi sub infix:<~^>(Str:D \a, Str:D \b) returns Str:D {
    nqp::p6box_s(nqp::bitxor_s(nqp::unbox_s(a), nqp::unbox_s(b)))
}
multi sub infix:<~^>(str $a, str $b) returns str { nqp::bitxor_s($a, $b) }

multi sub prefix:<~^>(Str \a) {
    fail "prefix:<~^> NYI";   # XXX
}

# XXX: String-wise shifts NYI
multi sub infix:«~>»(Str:D \a, Int:D \b) returns Str:D {
    X::NYI.new(feature => "infix:«~>»").throw;
}
multi sub infix:«~>»(str $a, int $b) {
    X::NYI.new(feature => "infix:«~>»").throw;
}
multi sub infix:«~<»(Str:D \a, Int:D \b) returns Str:D {
    X::NYI.new(feature => "infix:«~<»").throw;
}
multi sub infix:«~<»(str $a, int $b) {
    X::NYI.new(feature => "infix:«~<»").throw;
}

multi sub ords(Str $s) returns Seq:D {
    my Int $c  = $s.chars;
    my str $ns = nqp::unbox_s($s);
    (^$c).map: { nqp::p6box_i(nqp::ord(nqp::substr($ns, $_, 1))) }
}

# TODO: Cool  variants
sub trim         (Str:D $s) returns Str:D { $s.trim }
sub trim-leading (Str:D $s) returns Str:D { $s.trim-leading }
sub trim-trailing(Str:D $s) returns Str:D { $s.trim-trailing }

# the opposite of Real.base, used for :16($hex_str)
proto sub UNBASE (|) { * }
multi sub UNBASE(Int:D $base, Any:D $num) {
    X::Numeric::Confused.new(:$num, :$base).throw;
}
multi sub UNBASE(Int:D $base, Str:D $str) {
    my Str $ch = substr($str, 0, 1);
    if $ch eq '0' {
        $ch = substr($str, 1, 1);
        if    $base <= 11 && $ch eq any(<x d o b>)
           or $base <= 24 && $ch eq any <o x>
           or $base <= 33 && $ch eq 'x' {
            $str.Numeric;
        } else {
            ":{$base}<$str>".Numeric;
        }
    } elsif $ch eq ':' && substr($str, 1, 1) ~~ ('1'..'9') {
        $str.Numeric;
    } else {
        ":{$base}<$str>".Numeric;
    }
}

# for :16[1, 2, 3]
sub UNBASE_BRACKET($base, @a) {
    my $v = 0;
    my $denom = 1;
    my Bool $seen-dot = False;
    for @a {
        if $seen-dot {
            die "Only one decimal dot allowed" if $_ eq '.';
            $denom *= $base;
            $v += $_ / $denom
        }
        elsif $_ eq '.' {
            $seen-dot = True;
        }
        else {
            $v = $v * $base + $_;
        }
    }
    $v;
}

sub chrs(*@c) returns Str:D {
    @c.map({.chr}).join;
}

sub SUBSTR-START-OOR(\from,\max) {
    X::OutOfRange.new(
      :what('Start argument to substr'),
      :got(from.gist),
      :range("0.." ~ max),
      :comment( nqp::istype(from, Callable) || -from > max
        ?? ''
        !! "use *{from} if you want to index relative to the end"),
    );
}
sub SUBSTR-CHARS-OOR(\chars) {
    X::OutOfRange.new(
      :what('Number of characters argument to substr'),
      :got(chars.gist),
      :range("0..Inf"),
      :comment("use *{chars} if you want to index relative to the end"),
    );
}
sub SUBSTR-SANITY(Str \what, $start, $want, \from, \chars) {
    my Int $max := what.chars;
    from = nqp::istype($start, Callable)
      ?? $start($max)
      !! nqp::istype($start, Range)
        ?? $start.min + $start.excludes-min
        !! $start.Int;
    SUBSTR-START-OOR(from,$max).fail
      if from < 0 || from > $max;

    chars = nqp::istype($start, Range)
      ?? $start == Inf
        ?? $max - from
        !! $start.max - $start.excludes-max - from + 1
      !! $want.defined
        ?? $want === Inf
          ?? $max - from
          !! nqp::istype($want, Callable)
            ?? $want($max - from)
            !! (nqp::istype($want,Int) ?? $want !! $want.Int)
        !! $max - from;
    chars < 0 ?? SUBSTR-CHARS-OOR(chars).fail !! 1;
}

proto sub substr(|) { * }
multi sub substr(Str:D \what, Int:D \start) {
    my str $str  = nqp::unbox_s(what);
    my int $max  = nqp::chars($str);
    my int $from = nqp::unbox_i(start);

    SUBSTR-START-OOR($from,$max).fail
      if nqp::islt_i($from,0) || nqp::isgt_i($from,$max);

    nqp::p6box_s(nqp::substr($str,$from));
}
multi sub substr(Str:D \what, Callable:D \start) {
    my str $str  = nqp::unbox_s(what);
    my int $max  = nqp::chars($str);
    my int $from = nqp::unbox_i((start)(nqp::p6box_i($max)));

    SUBSTR-START-OOR($from,$max).fail
      if nqp::islt_i($from,0) || nqp::isgt_i($from,$max);

    nqp::p6box_s(nqp::substr($str,$from));
}
multi sub substr(Str:D \what, Int:D \start, Int:D \want) {
    my str $str   = nqp::unbox_s(what);
    my int $max   = nqp::chars($str);
    my int $from  = nqp::unbox_i(start);

    SUBSTR-START-OOR($from,$max).fail
     if nqp::islt_i($from,0) || nqp::isgt_i($from,$max);

    my int $chars = nqp::unbox_i(want);
    SUBSTR-CHARS-OOR($chars).fail
      if nqp::islt_i($chars,0);

    nqp::p6box_s(nqp::substr($str,$from,$chars));
}
multi sub substr(Str() $what, \start, $want?) {

    # should really be int, but \ then doesn't work for rw access
    my $r := SUBSTR-SANITY($what, start, $want, my Int $from, my Int $chars);
    $r.defined
      ?? nqp::p6box_s(nqp::substr(
           nqp::unbox_s($what),nqp::unbox_i($from),nqp::unbox_i($chars)
         ))
      !! $r;
}

sub substr-rw(\what, \start, $want?) is rw {
    my $Str := nqp::istype(what,Str) ?? what !! what.Str;

    # should really be int, but \ then doesn't work for rw access
    my $r := SUBSTR-SANITY($Str, start, $want, my Int $from, my Int $chars);
    $r.defined
      ?? Proxy.new(
           FETCH => sub ($) {
               nqp::p6box_s(nqp::substr(
                 nqp::unbox_s($Str), nqp::unbox_i($from), nqp::unbox_i($chars)
               ));
           },
           STORE => sub ($, Str() $new) {
               my $str = nqp::unbox_s($Str);
               what = nqp::p6box_s(
                 nqp::concat(
                   nqp::substr($str,0,nqp::unbox_i($from)),
                   nqp::concat(
                     nqp::unbox_s($new),
                     nqp::substr($str,nqp::unbox_i($from + $chars))
                   )
                 )
               );
           },
         )
      !! $r;
}

sub TRANSPOSE(Str \string, Str \original, Str \final) {
    my str $str    = nqp::unbox_s(string);
    my int $chars  = nqp::chars($str);
    my str $needle = nqp::unbox_s(original);
    my int $skip   = nqp::chars($needle);
    my int $from;
    my int $to;
    my Mu  $parts := nqp::list_s();

    while $to < $chars {
        $to = nqp::index($str,$needle,$from);
        last if $to == -1;
        nqp::push_s($parts, $to > $from
              ?? nqp::substr($str,$from,$to - $from)
              !! ''
            );
        $to = $from = $to + $skip;
    }
    nqp::push_s( $parts, $from < $chars
      ?? nqp::substr($str,$from,$chars - $from)
      !! ''
    );

    nqp::elems($parts)
      ?? nqp::box_s(nqp::join(nqp::unbox_s(final),$parts),Str)
      !! string;
}

sub TRANSPOSE-ONE(Str \string, Str \original, Str \final) {
    my str $str     = nqp::unbox_s(string);
    my int $chars   = nqp::chars($str);
    my int $ordinal = ord(original);
    my int $from;
    my int $to;
    my $parts := nqp::list_s();

    while $to < $chars {
        if nqp::ordat($str,$to) == $ordinal {
            nqp::push_s($parts, $to > $from
              ?? nqp::substr($str,$from,$to - $from)
              !! ''
            );
            $from = $to + 1;
        }
        $to = $to + 1;
    }
    nqp::push_s( $parts, $from < $chars
      ?? nqp::substr($str,$from,$chars - $from)
      !! ''
    );

    nqp::elems($parts)
      ?? nqp::box_s(nqp::join(nqp::unbox_s(final),$parts),Str)
      !! string;
}

# These probably belong in a separate unicodey file

proto sub uniname(|) {*}
multi sub uniname(Str $str) { uniname($str.ord) }
multi sub uniname(Int $code) { nqp::getuniname($code) }

proto sub uninames(|) {*}
multi sub uninames(Str $str) { $str.comb.map:{uniname($_.ord)}; }

#?if jvm
multi sub unival(|)       { die 'unival NYI on jvm backend' }
multi sub univals(|)      { die 'univals NYI on jvm backend' }
multi sub uniprop(|)      { die 'uniprop NYI on jvm backend' }
multi sub uniprop-int(|)  { die 'uniprop-int NYI on jvm backend' }
multi sub uniprop-bool(|) { die 'uniprop-bool NYI on jvm backend' }
multi sub uniprop-str(|)  { die 'uniprop-str NYI on jvm backend' }
multi sub unimatch(|)     { die 'unimatch NYI on jvm backend' }
#?endif

#?if moar
sub PROPCODE($propname) {
    state %propcode;
    %propcode{$propname} //= nqp::unipropcode($propname);
}
sub PVALCODE($prop,$pvalname) {
    state %pvalcode;
    %pvalcode{$prop ~ $pvalname} //= nqp::unipvalcode($prop, $pvalname);
}

proto sub uniprop(|) {*}
multi sub uniprop(Str $str, |c) { uniprop($str.ord, |c) }
multi sub uniprop(Int $code, Stringy $propname = "GeneralCategory") {
    my $prop := PROPCODE($propname);
    state %prefs;  # could prepopulate this with various prefs
    given %prefs{$propname} // '' {
        when 'S' { nqp::getuniprop_str($code,$prop) }
        when 'I' { nqp::getuniprop_int($code,$prop) }
        when 'B' { nqp::getuniprop_bool($code,$prop) }
        # your ad here
        default {
            my $result = nqp::getuniprop_str($code,$prop);
            if $result ne '' { %prefs{$propname} = 'S'; $result }
            else             { %prefs{$propname} = 'I'; nqp::getuniprop_int($code,$prop) }
        }
    }
}

proto sub uniprop-int(|) {*}
multi sub uniprop-int(Str $str, Stringy $propname) { uniprop-int($str.ord, $propname) }
multi sub uniprop-int(Int $code, Stringy $propname) {
    nqp::getuniprop_int($code,PROPCODE($propname));
}

proto sub uniprop-bool(|) {*}
multi sub uniprop-bool(Str $str, Stringy $propname) { uniprop-bool($str.ord, $propname) }
multi sub uniprop-bool(Int $code, Stringy $propname) {
    so nqp::getuniprop_bool($code,PROPCODE($propname));
}

proto sub uniprop-str(|) {*}
multi sub uniprop-str(Str $str, Stringy $propname) { uniprop-str($str.ord, $propname) }
multi sub uniprop-str(Int $code, Stringy $propname) {
    nqp::getuniprop_str($code,PROPCODE($propname));
}

proto sub unival(|) {*}
multi sub unival(Str $str) { unival($str.ord) }
multi sub unival(Int $code) {
    state $nuprop = nqp::unipropcode("NumericValueNumerator");
    state $deprop = nqp::unipropcode("NumericValueDenominator");
    my $nu = nqp::getuniprop_str($code, $nuprop);
    my $de = nqp::getuniprop_str($code, $deprop);
    !$de || $de eq '1' ?? $nu.Int !! $nu / $de;
}

proto sub univals(|) {*}
multi sub univals(Str $str) { $str.ords.map: { unival($_) } }

proto sub unimatch(|) {*}
multi sub unimatch(Str $str, |c) { unimatch($str.ord, |c) }
multi sub unimatch(Int $code, Stringy $pvalname, Stringy $propname = $pvalname) {
    my $prop := PROPCODE($propname);
    so nqp::matchuniprop($code,$prop,PVALCODE($prop,$pvalname));
}
#?endif

# vim: ft=perl6 expandtab sw=4
