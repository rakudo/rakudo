my class Cursor {... }
my class Range  {... }
my class Match  {... }
my class X::Str::Numeric  { ... }
my class X::Str::Match::x { ... }
my class X::Str::Trans::IllegalKey { ... }
my class X::Str::Trans::InvalidArg { ... }
my class X::Numeric::Confused { ... }
my class X::NYI { ... }

my $?TABSTOP = 8;

sub NORMALIZE_ENCODING(Str:D $s) {
    state %map = (
        # fast mapping for identicals
        'utf8'              => 'utf8',
        'utf16'             => 'utf16',
        'utf32'             => 'utf32',
        'ascii'             => 'ascii',
        'iso-8859-1'        => 'iso-8859-1',
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

    method WHY(Str:D:) {
        nextsame if self ne "Life, the Universe, and Everything";
        42;
    }

    multi method WHICH(Str:D:) {
        nqp::box_s(
            nqp::concat(
                nqp::concat(nqp::unbox_s(self.^name), '|'),
                $!value
            ),
            ObjAt
        );
    }
    submethod BUILD(:$value as Str = '') {
        nqp::bindattr_s(self, Str, '$!value', nqp::unbox_s($value))
    }

    multi method Bool(Str:D:) {
        nqp::p6bool(nqp::chars($!value) && nqp::isne_s($!value,"0"));
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

    method chop(Str:D: $chars = 1) {
        my str $sself = nqp::unbox_s(self);
        nqp::p6box_s(nqp::substr($sself, 0, nqp::chars($sself) - $chars))
    }

    method substr(Str:D: $start, $length? is copy) {
        my str $sself  = nqp::unbox_s(self);
        my int $istart = nqp::unbox_i(
            nqp::istype($start, Callable)
                ?? $start(nqp::p6box_i(nqp::chars($sself)))
                !! $start.Int
            );
        my int $ichars = nqp::chars($sself);
        if $istart < 0 {
            if nqp::istype($start, Callable) || -$istart > $ichars {
                X::OutOfRange.new(
                    what    => 'Start argument to substr',
                    got     => $istart,
                    range   => (0..$ichars),
                ).fail
            }
            else {
                X::OutOfRange.new(
                    what    => 'Start argument to substr',
                    got     => $start,
                    range   => (0..*),
                    comment => "use *{$istart} if you want to index relative to the end"
                ).fail
            }
        }
        X::OutOfRange.new(
            what => 'Start of substr',
            got  => $istart,
            range => (0..$ichars),
        ).fail
            if $istart > $ichars;
        $length = $length($ichars - $istart) if nqp::istype($length, Callable);
        my int $ilength = !$length.defined || $length === Inf
                            ?? $ichars - $istart
                            !! $length.Int;
        X::OutOfRange.new(
            what    => 'Length argument to substr',
            got     => $length,
            range   => (0..*),
            comment => "use *{$ilength} if you want to index relative to the end"
        ).fail
            if $ilength < 0;
        nqp::p6box_s(nqp::substr($sself, $istart, $ilength));
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
        ~ "\x[2776,2777,2778,2779,277A,277B,277C,277D,277E,277F,2776]"; # dingbat negative circled 1..10

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


    # TODO:
    # * Additional numeric styles:
    #   + fractions in [] radix notation:  :100[10,'.',53]
    # * Performance tuning
    # * Fix remaining XXXX

    multi method Numeric(Str:D: :$strict = True) {
        my str $str = nqp::unbox_s(self);
        my int $eos = nqp::chars($str);

        # S02:3276-3277: Ignore leading and trailing whitespace
        my int $pos = nqp::findnotcclass(nqp::const::CCLASS_WHITESPACE,
                                                  $str, 0, $eos);
        my int $end = nqp::sub_i($eos, 1);

        $end = nqp::sub_i($end, 1)
            while nqp::isge_i($end, $pos)
               && nqp::iscclass(nqp::const::CCLASS_WHITESPACE, $str, $end);

        # Return 0 if no non-whitespace characters in string
        return 0 if nqp::islt_i($end, $pos);

        # Reset end-of-string after trimming
        $eos = nqp::add_i($end, 1);

        # Fail all the way out when parse failures occur
        my &parse_fail := -> $msg {
            fail X::Str::Numeric.new(
                    source => self,
                    reason => $msg,
                    :$pos,
            );
        };

        my sub parse-simple-number () {
            # Handle NaN here, to make later parsing simpler
            if nqp::iseq_s(nqp::substr($str, $pos, 3), 'NaN') {
                $pos = nqp::add_i($pos, 3);
                return nqp::p6box_n(nqp::nan());
            }

            # Handle any leading +/- sign
            my int $ch  = nqp::ord($str, $pos);
            my int $neg = nqp::iseq_i($ch, 45);                # '-'
            if nqp::iseq_i($ch, 45) || nqp::iseq_i($ch, 43) {  # '-', '+'
                $pos = nqp::add_i($pos, 1);
                $ch  = nqp::islt_i($pos, $eos) && nqp::ord($str, $pos);
            }

            # nqp::radix_I parse results, and helper values
            my Mu  $parse;
            my str $prefix;
            my int $radix;
            my int $p;

            my sub parse-int-frac-exp () {
                # Integer part, if any
                my Int:D $int := 0;
                if nqp::isne_i($ch, 46) {  # '.'
                    $parse := nqp::radix_I($radix, $str, $pos, $neg, Int);
                    $p      = nqp::atpos($parse, 2);
                    parse_fail "base-$radix number must begin with valid digits or '.'"
                        if nqp::iseq_i($p, -1);
                    $pos    = $p;

                    $int   := nqp::atpos($parse, 0);
                    if nqp::isge_i($pos, $eos) {
                        return $int;
                    }
                    else {
                        $ch = nqp::ord($str, $pos);
                    }
                }

                # Fraction, if any
                my Int:D $frac := 0;
                my Int:D $base := 0;
                if nqp::iseq_i($ch, 46) {  # '.'
                    $pos    = nqp::add_i($pos, 1);
                    $parse := nqp::radix_I($radix, $str, $pos,
                                           nqp::add_i($neg, 4), Int);
                    $p      = nqp::atpos($parse, 2);
                    parse_fail 'radix point must be followed by one or more valid digits'
                        if nqp::iseq_i($p, -1);
                    $pos    = $p;

                    $frac  := nqp::atpos($parse, 0);
                    $base  := nqp::atpos($parse, 1);
                    $ch     = nqp::islt_i($pos, $eos) && nqp::ord($str, $pos);
                }

                # Exponent, if 'E' or 'e' are present (forces return type Num)
                if nqp::iseq_i($ch, 69) || nqp::iseq_i($ch, 101) {  # 'E', 'e'
                    parse_fail "'E' or 'e' style exponent only allowed on decimal (base-10) numbers, not base-$radix"
                        unless nqp::iseq_i($radix, 10);

                    $pos    = nqp::add_i($pos, 1);
                    $parse := nqp::radix_I(10, $str, $pos, 2, Int);
                    $p      = nqp::atpos($parse, 2);
                    parse_fail "'E' or 'e' must be followed by decimal (base-10) integer"
                        if nqp::iseq_i($p, -1);
                    $pos    = $p;

                    my num $exp  = nqp::atpos($parse, 0).Num;
                    my num $coef = $frac ?? nqp::add_n($int.Num, nqp::div_n($frac.Num, $base.Num)) !! $int.Num;
                    return nqp::p6box_n(nqp::mul_n($coef, nqp::pow_n(10e0, $exp)));
                }

                # Multiplier with exponent, if single '*' is present
                # (but skip if current token is '**', as otherwise we
                # get recursive multiplier parsing stupidity)
                if nqp::iseq_i($ch, 42)
                && nqp::isne_s(substr($str, $pos, 2), '**') {  # '*'
                    $pos           = nqp::add_i($pos, 1);
                    my $mult_base := parse-simple-number();

                    parse_fail "'*' multiplier base must be an integer"
                        unless $mult_base.WHAT === Int;
                    parse_fail "'*' multiplier base must be followed by '**' and exponent"
                        unless nqp::iseq_s(nqp::substr($str, $pos, 2), '**');

                    $pos           = nqp::add_i($pos, 2);
                    my $mult_exp  := parse-simple-number();

                    parse_fail "'**' multiplier exponent must be an integer"
                        unless $mult_exp.WHAT === Int;

                    my $mult := $mult_base ** $mult_exp;
                    $int     := $int  * $mult;
                    $frac    := $frac * $mult;
                }

                # Return an Int if there was no radix point
                return $int unless $base;

                # Otherwise, return a Rat
                my Int:D $numerator := $int * $base + $frac;
                return Rat.new($numerator, $base);
            }

            # Look for radix specifiers
            if nqp::iseq_i($ch, 58) {  # ':'
                # A string of the form :16<FE_ED.F0_0D> or :60[12,34,56]
                $pos    = nqp::add_i($pos, 1);
                $parse := nqp::radix_I(10, $str, $pos, 0, Int);
                $p      = nqp::atpos($parse, 2);
                parse_fail "radix (in decimal) expected after ':'"
                    if nqp::iseq_i($p, -1);
                $pos    = $p;

                $radix  = nqp::atpos($parse, 0);
                $ch     = nqp::islt_i($pos, $eos) && nqp::ord($str, $pos);
                if    nqp::iseq_i($ch, 60) {  # '<'
                    $pos = nqp::add_i($pos, 1);

                    my $result := parse-int-frac-exp();

                    parse_fail "malformed ':$radix<>' style radix number, expecting '>' after the body"
                        unless nqp::islt_i($pos, $eos)
                            && nqp::iseq_i(nqp::ord($str, $pos), 62);  # '>'

                    $pos = nqp::add_i($pos, 1);
                    return $result;
                }
                elsif nqp::iseq_i($ch, 171) {  # '«'
                    $pos = nqp::add_i($pos, 1);

                    my $result := parse-int-frac-exp();

                    parse_fail "malformed ':$radix«»' style radix number, expecting '»' after the body"
                        unless nqp::islt_i($pos, $eos)
                            && nqp::iseq_i(nqp::ord($str, $pos), 187);  # '»'

                    $pos = nqp::add_i($pos, 1);
                    return $result;
                }
                elsif nqp::iseq_i($ch, 91) {  # '['
                    $pos = nqp::add_i($pos, 1);
                    my Int:D $result := 0;
                    my Int:D $digit  := 0;
                    while nqp::islt_i($pos, $eos)
                       && nqp::isne_i(nqp::ord($str, $pos), 93) {  # ']'
                        $parse := nqp::radix_I(10, $str, $pos, 0, Int);
                        $p      = nqp::atpos($parse, 2);
                        parse_fail "malformed ':$radix[]' style radix number, expecting comma separated decimal values after opening '['"
                            if nqp::iseq_i($p, -1);
                        $pos    = $p;

                        $digit := nqp::atpos($parse, 0);
                        parse_fail "digit is larger than {$radix - 1} in ':$radix[]' style radix number"
                            if $digit >= $radix;

                        $result := $result * $radix + $digit;
                        $pos     = nqp::add_i($pos, 1)
                            if nqp::islt_i($pos, $eos)
                            && nqp::iseq_i(nqp::ord($str, $pos), 44);  # ','
                    }
                    parse_fail "malformed ':$radix[]' style radix number, expecting ']' after the body"
                        unless nqp::islt_i($pos, $eos)
                            && nqp::iseq_i(nqp::ord($str, $pos), 93);  # ']'
                    $pos = nqp::add_i($pos, 1);

                    # XXXX: Handle fractions!
                    # XXXX: Handle exponents!
                    return $neg ?? -$result !! $result;
                }
                else {
                    parse_fail "malformed ':$radix' style radix number, expecting '<' or '[' after the base";
                }
            }
            elsif nqp::iseq_i($ch, 48)  # '0'
              and $radix = nqp::index('  b     o d     x',
                                      nqp::substr($str, nqp::add_i($pos, 1), 1))
              and nqp::isge_i($radix, 2) {
                # A string starting with 0x, 0d, 0o, or 0b,
                # followed by one optional '_'
                $pos   = nqp::add_i($pos, 2);
                $pos   = nqp::add_i($pos, 1)
                    if nqp::islt_i($pos, $eos)
                    && nqp::iseq_i(nqp::ord($str, $pos), 95);  # '_'

                return parse-int-frac-exp();
            }
            elsif nqp::iseq_s(nqp::substr($str, $pos, 3), 'Inf') {
                # 'Inf'
                $pos = nqp::add_i($pos, 3);
                return $neg ?? -Inf !! Inf;
            }
            else {
                # Last chance: a simple decimal number
                $radix = 10;
                return parse-int-frac-exp();
            }
        }

        my sub parse-real () {
            # Parse a simple number or a Rat numerator
            my $result := parse-simple-number();
            return $result if nqp::iseq_i($pos, $eos);

            # Check for '/' indicating Rat denominator
            if nqp::iseq_i(nqp::ord($str, $pos), 47) {  # '/'
                $pos = nqp::add_i($pos, 1);
                parse_fail "denominator expected after '/'"
                    unless nqp::islt_i($pos, $eos);

                my $denom := parse-simple-number();

                $result := nqp::istype($result, Int) && nqp::istype($denom, Int)
                        ?? Rat.new($result, $denom)
                        !! $result / $denom;
            }

            return $result;
        }

        # Parse a real number, magnitude of a pure imaginary number,
        # or real part of a complex number
        my $result := parse-real();
        return $result if nqp::iseq_i($pos, $eos);

        # Check for 'i' or '\\i' indicating first parsed number was
        # the magnitude of a pure imaginary number
        if nqp::iseq_i(nqp::ord($str, $pos), 105) {  # 'i'
            $pos = nqp::add_i($pos, 1);
            $result := Complex.new(0, $result);
        }
        elsif nqp::iseq_s(nqp::substr($str, $pos, 2), '\\i') {
            $pos = nqp::add_i($pos, 2);
            $result := Complex.new(0, $result);
        }
        # Check for '+' or '-' indicating first parsed number was
        # the real part of a complex number
        elsif nqp::iseq_i(nqp::ord($str, $pos), 45)    # '-'
           || nqp::iseq_i(nqp::ord($str, $pos), 43) {  # '+'
            # Don't move $pos -- we want parse-real() to see the sign
            my $im := parse-real();
            parse_fail "imaginary part of complex number must be followed by 'i' or '\\i'"
                unless nqp::islt_i($pos, $eos);

            if nqp::iseq_i(nqp::ord($str, $pos), 105) {  # 'i'
                $pos = nqp::add_i($pos, 1);
            }
            elsif nqp::iseq_s(nqp::substr($str, $pos, 2), '\\i') {
                $pos = nqp::add_i($pos, 2);
            }
            else {
                parse_fail "imaginary part of complex number must be followed by 'i' or '\\i'"
            }

            $result := Complex.new($result, $im);
        }

        # Check for trailing garbage
        parse_fail "trailing characters after number"
            if nqp::islt_i($pos, $eos);

        return $result;
    }

    my %esc = (
        '$' => '\$',  '@' => '\@',  '%' => '\%',  '&' => '\&',  '{' => '\{',
        "\b" => '\b', "\n" => '\n', "\r" => '\r', "\t" => '\t', '"' => '\"',
        '\\' => '\\\\' );

    multi method gist(Str:D:) { self }
    multi method perl(Str:D:) {
        my $result = '"';
#?if parrot
        my $icu = $*VM.config<has_icu>;
        for ^self.chars -> $i {
            my $ch = self.substr($i, 1);
            $result ~= %esc{$ch}
                       //  (   ((!$icu && $ch.ord >= 256)
                               || nqp::iscclass( nqp::const::CCLASS_PRINTING,
                                                  nqp::unbox_s($ch), 0))
#?endif
#?if !parrot
        for ^self.chars -> $i {
            my $ch = self.substr($i, 1);
            $result ~= %esc{$ch}
                       //  (nqp::iscclass( nqp::const::CCLASS_PRINTING,
                                                  nqp::unbox_s($ch), 0)
#?endif
                           ?? $ch
                           !! $ch.ord.fmt('\x[%x]')
                           );
        }
        $result ~ '"'
    }

    multi method comb(Str:D:) {
        my str $self = nqp::unbox_s(self);
        (^self.chars).map({ nqp::p6box_s(nqp::substr($self, $_, 1)) });
    }
    multi method comb(Str:D: Regex $pat, $limit = Inf, :$match) {
        my $x;
        $x = (1..$limit) unless nqp::istype($limit, Whatever) || $limit == Inf;
        $match
            ?? self.match(:g, :$x, $pat)
            !! self.match(:g, :$x, $pat).map: { .Str }
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

        my @matches := gather {
            while $cur.pos >= 0 {
                take $cur.MATCH_SAVE;
                $cur := $cur.'!cursor_more'(|%opts);
            }
        }
        my $multi = $g || $ov || $ex;

        if $nth.defined {
            $multi = Positional.ACCEPTS($nth);
            my @nlist := $nth.list;
            my @src   := @matches;
            @matches  := gather {
                my $max = 0;
                while @nlist {
                    my $n = shift @nlist;
                    $n = $n(+@src) + 1 if nqp::istype($n, Callable);
                    fail "Attempt to retrieve negative match :nth($n)" if $n < 1;
                    if $n > $max { take @src[$n-1]; $max = $n; }
                }
            }
        }

        if $x.defined {
            $multi = True;
            if nqp::istype($x, Int) {
                @matches := @matches.gimme($x) >= $x
                            ?? @matches[^$x]
                            !! ().list
            }
            elsif nqp::istype($x, Range) {
                my $min = $x.min.ceiling;
                my $max = $x.max;
                $min++ while $min <= $max && $min !~~ $x;
                if @matches.gimme($min) >= $min && $min ~~ $x {
                    my @src := @matches;
                    @matches := gather {
                        my $n = 0;
                        while @src && ($n < $min || $n+1 ~~ $x) {
                            take @src.shift;
                            $n++;
                        }
                    }
                }
                else { @matches := ().list }
            }
            elsif nqp::istype($x, Whatever) { }
            else {
                X::Str::Match::x.new(got => $x).fail;
            }
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
        my @matches              = self.match($matcher, |%options);
        try $caller_dollar_slash = $/ if $SET_DOLLAR_SLASH;

        return Nil unless @matches;
        return Nil if @matches == 1 && !@matches[0];

        my $prev = 0;
        my $result = '';
        for @matches -> $m {
            try $caller_dollar_slash = $m if $SET_DOLLAR_SLASH;
            $result ~= self.substr($prev, $m.from - $prev);

            my $real_replacement = ~(nqp::istype($replacement,Callable)
                ?? ($replacement.count == 0 ?? $replacement() !! $replacement($m))
                !! $replacement);
            $real_replacement    = $real_replacement.samecase(~$m) if $samecase;
            $real_replacement    = $real_replacement.samespace(~$m) if $samespace;
            $result ~= $real_replacement;
            $prev = $m.to;
        }
        my $last = @matches[@matches-1];
        $result ~= self.substr($last.to);
        $self = $result;
        $global ?? (@matches,).list !! @matches[0];
    }

    multi method subst($matcher, $replacement,
                       :ii(:$samecase), :ss(:$samespace),
                       :$SET_CALLER_DOLLAR_SLASH, *%options) {
        my $caller_dollar_slash := nqp::getlexcaller('$/');
        my $SET_DOLLAR_SLASH     = $SET_CALLER_DOLLAR_SLASH || nqp::istype($matcher, Regex);
        my @matches              = self.match($matcher, |%options);
        try $caller_dollar_slash = $/ if $SET_DOLLAR_SLASH;

        return self unless @matches;
        return self if @matches == 1 && !@matches[0];

        my $prev = 0;
        my $result = '';
        for @matches -> $m {
            try $caller_dollar_slash = $m if $SET_DOLLAR_SLASH;
            $result ~= self.substr($prev, $m.from - $prev);

            my $real_replacement = ~(nqp::istype($replacement,Callable)
                ?? ($replacement.count == 0 ?? $replacement() !! $replacement($m))
                !! $replacement);
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
        my str $ns = nqp::unbox_s(self);
        (^self.chars).map: { nqp::p6box_i(nqp::ord(nqp::substr($ns, $_, 1))) }
    }

    # constant ???
    my str $CRLF = nqp::unbox_s("\r\n");

    multi method lines(Str:D:) {
        my str $str   = nqp::unbox_s(self);
        my int $chars = nqp::chars($str);
        my int $left;
        my int $pos;
        my int $nextpos;
        my int $found;

        gather while ($left = $chars - $pos) > 0 {
            $nextpos =
              nqp::findcclass(nqp::const::CCLASS_NEWLINE, $str, $pos, $left);
            take ($found = $nextpos - $pos)
              ?? nqp::box_s(nqp::substr( $str, $pos, $found ), Str)
              !! '';
            $pos = $nextpos + 1 + nqp::eqat($str, $CRLF, $nextpos);
        }
    }
    multi method lines(Str:D: :$eager!) {  # can probably go after GLR
        return self.lines if !$eager;

        my str $str   = nqp::unbox_s(self);
        my int $chars = nqp::chars($str);
        my int $left;
        my int $pos;
        my int $nextpos;
        my int $found;
        my Mu $rpa := nqp::list();

        while ($left = $chars - $pos) > 0 {
            $nextpos =
              nqp::findcclass(nqp::const::CCLASS_NEWLINE, $str, $pos, $left);
            nqp::push($rpa, ($found = $nextpos - $pos)
              ?? nqp::box_s(nqp::substr( $str, $pos, $found ), Str)
              !! ''
            );
            $pos = $nextpos + 1 + nqp::eqat($str, $CRLF, $nextpos);
        }
        nqp::p6parcel($rpa, Nil);
    }
    multi method lines(Str:D: :$count!) {
        return self.lines if !$count;

        my str $str   = nqp::unbox_s(self);
        my int $chars = nqp::chars($str);
        my int $left;
        my int $pos;
        my int $nextpos;
        my int $lines;

        while ($left = $chars - $pos) > 0 {
            $nextpos =
              nqp::findcclass(nqp::const::CCLASS_NEWLINE, $str, $pos, $left);
            $lines = $lines + 1;
            $pos   = $nextpos + 1 + nqp::eqat($str, $CRLF, $nextpos);
        }
        nqp::box_i($lines, Int);
    }
    multi method lines(Str:D: Whatever $, :$eager) {
        self.lines(:$eager);
    }
    multi method lines(Str:D: $limit, :$eager ) {
        return self.lines(:$eager) if $limit == Inf;

        my str $str   = nqp::unbox_s(self);
        my int $chars = nqp::chars($str);
        my int $left;
        my int $pos;
        my int $nextpos;
        my int $found;
        my int $count = $limit + 1;
        my Mu $rpa := nqp::list();

        while ($count = $count - 1) and ($left = $chars - $pos) > 0 {
            $nextpos =
              nqp::findcclass(nqp::const::CCLASS_NEWLINE, $str, $pos, $left);
            nqp::push($rpa, ($found = $nextpos - $pos)
              ?? nqp::box_s(nqp::substr( $str, $pos, $found ), Str)
              !! ''
            );
            $pos = $nextpos + 1 + nqp::eqat($str, $CRLF, $nextpos);
        }
        nqp::p6parcel($rpa, Nil);
    }

    multi method split(Str:D: Regex $pat, $limit = *, :$all) {
        return ().list
          if nqp::istype($limit,Numeric) && $limit <= 0;
        my @matches = nqp::istype($limit, Whatever)
          ?? self.match($pat, :g)
          !! self.match($pat, :x(1..$limit-1), :g);

        # add dummy for last
        push @matches, Match.new( :from(self.chars) );
        my $prev-pos = 0;

        if ($all) {
            my $elems = +@matches;
            map {
                my $value = self.substr($prev-pos, .from - $prev-pos);
                $prev-pos = .to;
                # we don't want the dummy object
                --$elems ?? ($value, $_) !! $value;
            }, @matches;
        }
        else {
            map {
                my $value = self.substr($prev-pos, .from - $prev-pos);
                $prev-pos = .to;
                $value;
            }, @matches;
        }
    }

    multi method split(Str:D: Cool $delimiter, $limit = *, :$all) {
        my $delim-str        = $delimiter.Str;
        my str $self-string  = self;
        my str $match-string = $delim-str;
        return unless nqp::chars($self-string) || nqp::chars($match-string);

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
                    $all ?? (value, $match-string) !! value;
                }
                else {
                    $done = 1;
                    nqp::p6box_s(nqp::substr($self-string, $c));
                }
            }, 1 .. $l;
        } else {
            my int $chars = nqp::chars($self-string);
            map {
                last if $done;

                if ($chars = $chars - 1) and ($l = $l - 1) {
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
        my @self-chunks  = self.split(rx/\s+/, :all).flat;
        my @pat-chunks  := $pat.split(rx/\s+/, :all).flat;
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

    multi method words(Str:D:) {
        my str $str   = nqp::unbox_s(self);
        my int $chars = nqp::chars($str);
        my int $pos   = nqp::findnotcclass(
          nqp::const::CCLASS_WHITESPACE, $str, 0, $chars);
        my int $left;
        my int $nextpos;

        gather while ($left = $chars - $pos) > 0 {
            $nextpos = nqp::findcclass(
              nqp::const::CCLASS_WHITESPACE, $str, $pos, $left);
            take nqp::box_s(nqp::substr( $str, $pos, $nextpos - $pos ), Str);
            last unless $left = $chars - $nextpos;

            $pos = nqp::findnotcclass(
              nqp::const::CCLASS_WHITESPACE, $str, $nextpos, $left);
        }
    }
    multi method words(Str:D: :$eager!) {  # can probably go after GLR
        return self.words if !$eager;

        my str $str   = nqp::unbox_s(self);
        my int $chars = nqp::chars($str);
        my int $pos   = nqp::findnotcclass(
          nqp::const::CCLASS_WHITESPACE, $str, 0, $chars);
        my int $left;
        my int $nextpos;
        my Mu $rpa := nqp::list();

        while ($left = $chars - $pos) > 0 {
            $nextpos = nqp::findcclass(
              nqp::const::CCLASS_WHITESPACE, $str, $pos, $left);
            nqp::push($rpa,
              nqp::box_s(nqp::substr( $str, $pos, $nextpos - $pos ), Str));
            last unless $left = $chars - $nextpos;

            $pos = nqp::findnotcclass(
              nqp::const::CCLASS_WHITESPACE, $str, $nextpos, $left);
        }
        nqp::p6parcel($rpa, Nil);
    }
    multi method words(Str:D: :$autoderef!) { # in Actions.postprocess_words
        my @list := self.words(:eager);
        return @list == 1 ?? @list[0] !! @list;
    }
    multi method words(Str:D: :$count!) {
        return self.words if !$count;

        my str $str   = nqp::unbox_s(self);
        my int $chars = nqp::chars($str);
        my int $pos   = nqp::findnotcclass(
          nqp::const::CCLASS_WHITESPACE, $str, 0, $chars);
        my int $left;
        my int $nextpos;
        my int $words;

        while ($left = $chars - $pos) > 0 {
            $nextpos = nqp::findcclass(
              nqp::const::CCLASS_WHITESPACE, $str, $pos, $left);
            $words = $words + 1;
            last unless $left = $chars - $nextpos;

            $pos = nqp::findnotcclass(
              nqp::const::CCLASS_WHITESPACE, $str, $nextpos, $left);
        }
        nqp::box_i($words, Int);
    }
    multi method words(Str:D: Whatever $, :$eager) {
        self.words(:$eager);
    }
    multi method words(Str:D: $limit, :$eager) {
        return self.words(:$eager) if $limit == Inf;

        my str $str   = nqp::unbox_s(self);
        my int $chars = nqp::chars($str);
        my int $pos   = nqp::findnotcclass(
          nqp::const::CCLASS_WHITESPACE, $str, 0, $chars);
        my int $left;
        my int $nextpos;
        my int $count = $limit + 1;
        my Mu $rpa := nqp::list();

        while ($count = $count - 1) and ($left = $chars - $pos) > 0 {
            $nextpos = nqp::findcclass(
              nqp::const::CCLASS_WHITESPACE, $str, $pos, $left);
            nqp::push($rpa,
              nqp::box_s(nqp::substr( $str, $pos, $nextpos - $pos ), Str));
            last unless $left = $chars - $nextpos;

            $pos = nqp::findnotcclass(
              nqp::const::CCLASS_WHITESPACE, $str, $nextpos, $left);
        }
        nqp::p6parcel($rpa, Nil);
    }

    my %enc_type = utf8 => utf8, utf16 => utf16, utf32 => utf32;
    method encode(Str:D $encoding = 'utf8') {
        my $enc      := NORMALIZE_ENCODING($encoding);
        my $enc_type := %enc_type.exists_key($enc) ?? %enc_type{$enc} !! blob8;
        nqp::encode(nqp::unbox_s(self), nqp::unbox_s($enc), nqp::decont($enc_type.new))
    }

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
            push @!substitutions, $key => $value;
        }

        submethod compare_substitution($substitution, Int $pos, Int $length) {
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
            my $m := $!source.match(.key, :continue($!index));
            return unless $m;
            $!last_match_obj = $/;
            self.compare_substitution($_, $m.from, $m.to - $m.from);
            True
        }

        multi method triage_substitution($_ where { nqp::istype(.key,Cool) }) {
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
            $!source.substr($!index) ~~ $s;
            $!last_match_obj = $/;
            $!index = $!next_match + $/.chars;
        }

        multi method increment_index(Cool $s) {
            $!index = $!next_match + nqp::chars($s.Str);
        }

        method get_next_substitution_result {
            my $result = $!complement ?? $!first_substitution.value !! $!next_substitution.value;
            my $cds := nqp::getlexcaller('$CALLER_DOLLAR_SLASH');
            try $cds = $!match_obj;
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
            $!next_match = $!source.chars;
            $!first_substitution //= @!substitutions[0];

            # triage_substitution has a side effect!
            @!substitutions = @!substitutions.grep: { self.triage_substitution($_) }

            $!unsubstituted_text # = nqp::substr(nqp::unbox_s($!source), $!index,
                = $!source.substr($!index, $!next_match - $!index);
            if defined $!next_substitution {
                if $!complement {
                    my $oldidx = $!index;
                    my $result = self.get_next_substitution_result;
                    if $!unsubstituted_text {
                        self.increment_index($!next_substitution.key);
                        $!substituted_text = $!source.substr($oldidx + $!unsubstituted_text.chars, 
                            $!index - $oldidx - $!unsubstituted_text.chars);
                        $!unsubstituted_text = $!squash ?? $result
                            !! $result x $!unsubstituted_text.chars;
                    }
                    else {
                        self.increment_index($!next_substitution.key);
                        $!substituted_text = '';
                        $!unsubstituted_text = $!source.substr($oldidx, $!index - $oldidx);
                    }
                }
                else {
                    $!substituted_text = self.get_next_substitution_result;
                    self.increment_index($!next_substitution.key);
                }
            }

            return $!next_match < $!source.chars && @!substitutions;
        }
    }

    method trans(Str:D: *@changes, :complement(:$c), :squash(:$s), :delete(:$d)) {
        my sub expand($s) {
            return $s.list
              if nqp::istype($s,Iterable) || nqp::istype($s,Positional);
            $s.comb(/ (\w) '..' (\w) | . /, :match).map: {
                .[0] ?? ~.[0] .. ~.[1] !! ~$_
            };
        }

        my $CALLER_DOLLAR_SLASH := nqp::getlexcaller('$/');
        my $lsm = LSM.new(:source(self), :squash($s), :complement($c));
        for (@changes) -> $p {
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
                    @to = @to, $padding xx @from - @to;
                }
                else {
                    @to = '' xx @from
                }
                for @from Z @to -> $f, $t {
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
    multi method indent($steps as Int where { $_ == 0 }) {
        self;
    }

    # Positive indent does indent
    multi method indent($steps as Int where { $_ > 0 }) {
    # We want to keep trailing \n so we have to .comb explicitly instead of .lines
        return self.comb(/:r ^^ \N* \n?/).map({
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
    multi method indent($steps as Int where { $_ < 0 }) {
        return outdent(self, $steps);
    }

    # Whatever indent (outdent)
    multi method indent(Whatever $steps) {
        return outdent(self, $steps);
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

    method codes(Str:D:) returns Int:D {
        nqp::p6box_i(nqp::chars(nqp::unbox_s(self)))
    }

    method path(Str:D:) returns IO::Path:D {
        DEPRECATED('IO', |<2014.11 2015.11>);
        IO::Path.new(self)
    }

    method unival(Str:D:)  { unival(self.ord) };
    method univals(Str:D:) { univals(self) };
}


multi sub prefix:<~>(Str:D \a)  returns Str:D { a }
multi sub prefix:<~>(str $a)    returns str   { $a }

multi sub infix:<~>(Str:D \a, Str:D \b) returns Str:D {
    nqp::p6box_s(nqp::concat(nqp::unbox_s(a), nqp::unbox_s(b)))
}
multi sub infix:<~>(str $a, str $b) returns str { nqp::concat($a, $b) }

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

multi sub ords(Str $s) returns List:D {
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
multi sub UNBASE(Int:D $base, Cool:D $num) is hidden_from_backtrace {
    X::Numeric::Confused.new(:what($num)).throw;
}
multi sub UNBASE(Int:D $base, Str:D $str) is hidden_from_backtrace {
    my Str $prefix = $str.substr(0, 2);
    if    $base <= 10 && $prefix eq any(<0x 0d 0o 0b>)
       or $base <= 24 && $prefix eq any <0o 0x>
       or $base <= 33 && $prefix eq '0x' {
        $str.Numeric;

    } else {
        ":{$base}<$str>".Numeric;
    }
}

# for :16[1, 2, 3]
sub UNBASE_BRACKET($base, @a) is hidden_from_backtrace {
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

sub substr-rw($s is rw, $from = 0, $chars = $s.chars - $from) {
    my Str $substr = $s.substr($from, $chars);
    Proxy.new(
        FETCH   => sub ($) { $substr },
        STORE   => sub ($, $new) {
            $s = $s.substr(0, $from)
               ~ $new
               ~ $s.substr($from + $chars);
        }
    );
}

# These probably belong in a separate unicodey file

#?if parrot
multi sub uniname(|)  { die 'uniname NYI on parrot backend' }
multi sub uniprop(|)  { die 'uniprop NYI on parrot backend' }
multi sub unibool(|)  { die 'unibool NYI on parrot backend' }
multi sub unival(|)   { die 'unival NYI on parrot backend' }
multi sub univals(|)  { die 'univals NYI on parrot backend' }
multi sub unimatch(|) { die 'unimatch NYI on parrot backend' }
#?endif
#?if jvm
multi sub uniname(|)  { die 'uniname NYI on jvm backend' }
multi sub uniprop(|)  { die 'uniprop NYI on jvm backend' }
multi sub unibool(|)  { die 'unibool NYI on jvm backend' }
multi sub unival(|)   { die 'unival NYI on jvm backend' }
multi sub univals(|)  { die 'univals NYI on jvm backend' }
multi sub unimatch(|) { die 'unimatch NYI on jvm backend' }
#?endif
#?if moar
my %propcodecache;
my %pvalcodecache;
proto sub uniname(|) {*}
multi sub uniname(Str $str) { uniname($str.ord) }
multi sub uniname(Int $code) { nqp::getuniname($code) }

proto sub uniprop(|) {*}
multi sub uniprop(Str $str, |c) { uniprop($str.ord, |c) }
multi sub uniprop(Int $code, Stringy $propname = "GeneralCategory") {
    my $prop = %propcodecache{$propname} //= nqp::unipropcode($propname);
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
    my $prop = %propcodecache{$propname} //= nqp::unipropcode($propname);
    nqp::getuniprop_int($code,$prop);
}

proto sub uniprop-bool(|) {*}
multi sub uniprop-bool(Str $str, Stringy $propname) { uniprop-bool($str.ord, $propname) }
multi sub uniprop-bool(Int $code, Stringy $propname) {
    my $prop = %propcodecache{$propname} //= nqp::unipropcode($propname);
    so nqp::getuniprop_bool($code,$prop);
}

proto sub uniprop-str(|) {*}
multi sub uniprop-str(Str $str, Stringy $propname) { uniprop-str($str.ord, $propname) }
multi sub uniprop-str(Int $code, Stringy $propname) {
    my $prop = %propcodecache{$propname} //= nqp::unipropcode($propname);
    nqp::getuniprop_str($code,$prop);
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
    my $prop = %propcodecache{$propname} //= nqp::unipropcode($propname);
    my $pval = %pvalcodecache{$prop ~ $pvalname} //= nqp::unipvalcode($prop, $pvalname);
    so nqp::matchuniprop($code,$prop,$pval);
}
#?endif

# vim: ft=perl6 expandtab sw=4
