my class Str does Stringy {
    method Bool() { self ne '' && self ne '0' }
    
    multi method Str(Str:D:) { self }
    
    method Int() {
        nqp::p6box_i(nqp::unbox_s(self));
    }
    
    method Num() {
        nqp::p6box_n(nqp::unbox_s(self));
    }

    method Numeric() { self.Num }

    multi method ACCEPTS(Str:D: $other) { $other eq self }

    method chomp() {
        my $n_idx = self.chars - 1;
        my $rn_idx = $n_idx - 1;
        self.substr($rn_idx) eq "\r\n" ?? self.substr(0, $rn_idx) !!
        self.substr($n_idx)  eq "\n"   ?? self.substr(0, $n_idx)  !!
        self;
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
