my class Cursor {... }
my class Range  {... }
my class Match  {... }
my class IO::Path         { ... }
my class X::Str::Numeric  { ... }
my class X::Str::Match::x { ... }
my class X::Str::Trans::IllegalKey { ... }
my class X::Str::Trans::InvalidArg { ... }


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

    multi method Bool(Str:D:) { self ne '' && self ne '0' }

    multi method Str(Str:D:)     { self }
    multi method Stringy(Str:D:) { self }
    multi method DUMP(Str:D:) { self.perl }

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
        nqp::p6box_s(nqp::substr($sself, 0, $chars - $to_remove))
    }

    method chop(Str:D:) {
        my str $sself = nqp::unbox_s(self);
        nqp::p6box_s(nqp::substr($sself, 0, nqp::chars($sself) - 1))
    }

    method substr(Str:D: $start, $length? is copy) {
        my str $sself  = nqp::unbox_s(self);
        my int $istart = nqp::unbox_i(
            nqp::istype($start, Callable)
                ?? $start(nqp::p6box_i(nqp::chars($sself)))
                !! $start.Int
            );
        my int $ichars = nqp::chars($sself);
        X::OutOfRange.new(
            what    => 'Start argument to substr',
            got     => $start,
            range   => (0..*),
            comment => "use *{$istart} if you want to index relative to the end"
        ).fail
            if $istart < 0;
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
                    $ch     = nqp::islt_i($pos, $eos) && nqp::ord($str, $pos);
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
                return $neg ?? -$Inf !! $Inf;
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

                $result := $result.WHAT === Int && $denom.WHAT === Int
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
        my $icu = $*VM<config><has_icu>;
        for ^self.chars -> $i {
            my $ch = self.substr($i, 1);
            $result ~= %esc{$ch} 
                       //  (   ((!$icu && $ch.ord >= 256)
                               || nqp::iscclass( nqp::const::CCLASS_PRINTING,
                                                  nqp::unbox_s($ch), 0))
                           ?? $ch
                           !! $ch.ord.fmt('\x[%x]')
                           );
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

    method match($pat, 
                  :continue(:$c), :pos(:$p),
                  :global(:$g), :overlap(:$ov), :exhaustive(:$ex), 
                  :st(:nd(:rd(:th(:$nth)))), :$x) {
        my $caller_dollar_slash := nqp::getlexcaller('$/');
        my %opts;
        if $p.defined { %opts<p> = $p }
        else { %opts<c> = $c // 0; }
        my $patrx := $pat ~~ Code ?? $pat !! / "$pat": /;
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
            @matches
        }
        else {
            try $caller_dollar_slash = (@matches[0] // $cur.MATCH_SAVE);
            (@matches[0] // $cur.MATCH_SAVE)
        }
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

            my $real_replacement = ~($replacement ~~ Callable
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
        my int $pos = nqp::findnotcclass(
                          nqp::const::CCLASS_WHITESPACE,
                          $str, 0, nqp::chars($str));
        nqp::p6box_s(nqp::substr($str, $pos));
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

    method words(Str:D: $limit = *) {
        self.comb( / \S+ /, $limit );
    }

    my %enc_type = utf8 => utf8, utf16 => utf16, utf32 => utf32;
    method encode(Str:D $encoding = 'utf8') {
        my $enc      := NORMALIZE_ENCODING($encoding);
        my $enc_type := %enc_type.exists($enc) ?? %enc_type{$enc} !! blob8;
        nqp::encode(nqp::unbox_s(self), nqp::unbox_s($enc), nqp::decont($enc_type.new))
    }

    method capitalize(Str:D:) is DEPRECATED {
        self.subst(:g, rx/\w+/, -> $_ { .Str.lc.ucfirst });
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

        proto method triage_substitution(|) {*}
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
            X::Str::Trans::IllegalKey.new(key => $_).throw;
        }

        proto method increment_index(|) {*}
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
                    0;
                } else {
                    take ~$_;
                }
            }
        }

        my $lsm = LSM.new(:source(self));
        for (@changes) -> $p {
            X::Str::Trans::InvalidArg.new(got => $p).throw unless $p ~~ Pair;
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
            }).eager;

            { :$indent-size, :@indent-chars, :rest(~$rest) };
        });

        # Figure out the amount * should outdent by, we also use this for warnings
        my $common-prefix = min @lines.grep({ .<indent-size> ||  .<rest> ~~ /\S/}).map({ $_<indent-size> });
        return self if $common-prefix === $Inf;

        # Set the actual outdent amount here
        my Int $outdent = $steps ~~ Whatever ?? $common-prefix
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

    method tclc(Str:D:) returns Str:D {
        nqp::p6box_s(nqp::tclc(nqp::unbox_s(self)))
    }

    method path(Str:D:) returns IO::Path:D {
        IO::Path.new(self)
    }
}


multi prefix:<~>(Str:D \a)  returns Str:D { a }
multi prefix:<~>(str $a)    returns str   { $a }

multi infix:<~>(Str:D \a, Str:D \b) returns Str:D {
    nqp::p6box_s(nqp::concat(nqp::unbox_s(a), nqp::unbox_s(b)))
}
multi infix:<~>(str $a, str $b) returns str { nqp::concat($a, $b) }

multi infix:<x>(Str:D $s, Int:D $repetition) returns Str:D {
    $repetition < 0
        ?? ''
        !!  nqp::p6box_s(nqp::x(nqp::unbox_s($s), nqp::unbox_i($repetition)))
}
multi infix:<x>(str $s, int $repetition) returns str {
    nqp::if(nqp::islt_i($repetition, 0), '', nqp::x($s, $repetition))
}

multi infix:<cmp>(Str:D \a, Str:D \b) returns Order:D {
    Order.(nqp::p6box_i(nqp::cmp_s(nqp::unbox_s(a), nqp::unbox_s(b))))
}
multi infix:<cmp>(str $a, str $b) returns Order:D {
    Order.(nqp::p6box_i(nqp::cmp_s($a, $b)))
}

multi infix:<===>(Str:D \a, Str:D \b) returns Bool:D {
    nqp::p6bool(nqp::iseq_s(nqp::unbox_s(a), nqp::unbox_s(b)))
}
multi infix:<===>(str $a, str $b) returns Bool:D {
    nqp::p6bool(nqp::iseq_s($a, $b))
}

multi infix:<leg>(Str:D \a, Str:D \b) returns Order:D {
    Order.(nqp::p6box_i(nqp::cmp_s(nqp::unbox_s(a), nqp::unbox_s(b))))
}
multi infix:<leg>(str $a, str $b) returns Order:D {
    Order.(nqp::p6box_i(nqp::cmp_s($a, $b)))
}

multi infix:<eq>(Str:D \a, Str:D \b) returns Bool:D {
    nqp::p6bool(nqp::iseq_s(nqp::unbox_s(a), nqp::unbox_s(b)))
}
multi infix:<eq>(str $a, str $b) returns Bool:D {
    nqp::p6bool(nqp::iseq_s($a, $b))
}

multi infix:<lt>(Str:D \a, Str:D \b) returns Bool:D {
    nqp::p6bool(nqp::islt_s(nqp::unbox_s(a), nqp::unbox_s(b)))
}
multi infix:<lt>(str $a, str $b) returns Bool:D {
    nqp::p6bool(nqp::islt_s($a, $b))
}

multi infix:<le>(Str:D \a, Str:D \b) returns Bool:D {
    nqp::p6bool(nqp::isle_s(nqp::unbox_s(a), nqp::unbox_s(b)))
}
multi infix:<le>(str $a, str $b) returns Bool:D {
    nqp::p6bool(nqp::isle_s($a, $b))
}

multi infix:<gt>(Str:D \a, Str:D \b) returns Bool:D {
    nqp::p6bool(nqp::isgt_s(nqp::unbox_s(a), nqp::unbox_s(b)))
}
multi infix:<gt>(str $a, str $b) returns Bool:D {
    nqp::p6bool(nqp::isgt_s($a, $b))
}

multi infix:<ge>(Str:D \a, Str:D \b) returns Bool:D {
    nqp::p6bool(nqp::isge_s(nqp::unbox_s(a), nqp::unbox_s(b)))
}
multi infix:<le>(str $a, str $b) returns Bool:D {
    nqp::p6bool(nqp::isle_s($a, $b))
}

multi infix:<~|>(Str:D \a, Str:D \b) returns Str:D {
    nqp::p6box_s(nqp::bitor_s(nqp::unbox_s(a), nqp::unbox_s(b)))
}
multi infix:<~|>(str $a, str $b) returns str { nqp::bitor_s($a, $b) }

multi infix:<~&>(Str:D \a, Str:D \b) returns Str:D {
    nqp::p6box_s(nqp::bitand_s(nqp::unbox_s(a), nqp::unbox_s(b)))
}
multi infix:<~&>(str $a, str $b) returns str { nqp::bitand_s($a, $b) }

multi infix:<~^>(Str:D \a, Str:D \b) returns Str:D {
    nqp::p6box_s(nqp::bitxor_s(nqp::unbox_s(a), nqp::unbox_s(b)))
}
multi infix:<~^>(str $a, str $b) returns str { nqp::bitxor_s($a, $b) }

multi prefix:<~^>(Str \a) {
    fail "prefix:<~^> NYI";   # XXX
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
sub unbase(Int:D $base, Str:D $str) returns Numeric:D {
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
sub unbase_bracket($base, @a) {
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
    @c.map({.chr}).join('');
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

multi sub tclc(Str:D $s) returns Str:D {
    nqp::p6box_s(nqp::tclc(nqp::unbox_s($s)));
}
