my class X::Buf::AsStr { ... }
my class X::Buf::Pack { ... }
my class X::Buf::Pack::NonASCII { ... }

my role Blob[::T = uint8] does Positional[T] does Stringy is repr('VMArray') is array_type(T) {
    proto method new(|) { * }
    multi method new() {
        nqp::create(self)
    }
    multi method new(@values) {
        my $buf := nqp::create(self);
        my int $n = @values.elems;
        my int $i;
        nqp::setelems($buf, $n);
        while $i < $n {
            nqp::bindpos_i($buf, $i, @values.at_pos($i));
            $i = $i + 1;
        }
        $buf
    }
    multi method new(*@values) {
        self.new(@values)
    }
    
    multi method at_pos(Blob:D: $i) {
        nqp::atpos_i(self, $i.Int)
    }
    multi method at_pos(Blob:D: Int $i) {
        nqp::atpos_i(self, $i)
    }
    multi method at_pos(Blob:D: int $i) {
        nqp::atpos_i(self, $i)
    }
    
    multi method Bool(Blob:D:) {
        nqp::p6bool(nqp::elems(self));
    }

    method elems(Blob:D:) {
        nqp::p6box_i(nqp::elems(self));
    }
    method bytes(Blob:D:) {
        self.elems
    }
    method chars(Blob:D:)       { X::Buf::AsStr.new(method => 'chars').throw }
    multi method Str(Blob:D:)   { X::Buf::AsStr.new(method => 'Str'  ).throw }
    multi method Stringy(Blob:D:) { self }

    method Numeric(Blob:D:) { self.elems }
    method Int(Blob:D:)     { self.elems }
    
    method decode(Blob:D: $encoding = 'utf-8') {
        nqp::p6box_s(nqp::decode(self, NORMALIZE_ENCODING($encoding)))
    }
    
    method list(Blob:D:) {
        my @l;
        my int $n = nqp::elems(self);
        my int $i = 0;
        while $i < $n {
            @l[$i] = nqp::atpos_i(self, $i);
            $i = $i + 1;
        }
        @l;
    }

    multi method gist(Blob:D:) {
        'Buf:0x<' ~ self.list.fmt('%02x', ' ') ~ '>'
    }
    multi method perl(Blob:D:) {
        self.^name ~ '.new(' ~ self.list.join(', ') ~ ')';
    }
    
    method subbuf(Blob:D: $from = 0, $len = self.elems - $from) {
        my $ret := nqp::create(self);
        my int $llen = $len.Int;
        nqp::setelems($ret, $llen);
        my int $i = 0;
        my int $f = $from.Int;
        while $i < $llen {
            nqp::bindpos_i($ret, $i, nqp::atpos_i(self, $f));
            $i = $i + 1;
            $f = $f + 1;
        }
        $ret
    }
    
    method unpack(Blob:D: $template) {
        my @bytes = self.list;
        my @fields;
        for $template.comb(/<[a..zA..Z]>[\d+|'*']?/) -> $unit {
            my $directive = $unit.substr(0, 1);
            my $amount = $unit.substr(1);

            given $directive {
                when 'A' {
                    my $asciistring;
                    if $amount eq '*' {
                        $amount = @bytes.elems;
                    }
                    for ^$amount {
                        $asciistring ~= chr(shift @bytes);
                    }
                    @fields.push($asciistring);
                }
                when 'H' {
                    my $hexstring;
                    while @bytes {
                        my $byte = shift @bytes;
                        $hexstring ~= ($byte +> 4).fmt('%x')
                                    ~ ($byte % 16).fmt('%x');
                    }
                    @fields.push($hexstring);
                }
                when 'x' {
                    if $amount eq '*' {
                        $amount = 0;
                    }
                    elsif $amount eq '' {
                        $amount = 1;
                    }
                    splice @bytes, 0, $amount;
                }
                when 'C' {
                    @fields.push: shift @bytes;
                }
                when 'S' | 'v' {
                    @fields.push: shift(@bytes)
                                 + (shift(@bytes) +< 0x08);
                }
                when 'L' | 'V' {
                    @fields.push: shift(@bytes)
                                 + (shift(@bytes) +< 0x08)
                                 + (shift(@bytes) +< 0x10)
                                 + (shift(@bytes) +< 0x18);
                }
                when 'n' {
                    @fields.push: (shift(@bytes) +< 0x08)
                                 + shift(@bytes);
                }
                when 'N' {
                    @fields.push: (shift(@bytes) +< 0x18)
                                 + (shift(@bytes) +< 0x10)
                                 + (shift(@bytes) +< 0x08)
                                 + shift(@bytes);
                }
                X::Buf::Pack.new(:$directive).throw;
            }
        }

        return |@fields;
    }

    # XXX: the pack.t spectest file seems to require this method
    # not sure if it should be changed to list there...
    method contents(Blob:D:) { self.list }
    
    method encoding() { Any }
}

constant blob8 = Blob[uint8];
constant blob16 = Blob[uint16];
constant blob32 = Blob[uint32];
constant blob64 = Blob[uint64];

my class utf8 does Blob[uint8] is repr('VMArray') {
    method decode(utf8:D: $encoding = 'utf-8') {
        my $enc = NORMALIZE_ENCODING($encoding);
        die "Can not decode a utf-8 buffer as if it were $encoding"
            unless $enc eq 'utf8';
        nqp::p6box_s(nqp::decode(self, 'utf8'))
    }
    method encoding() { 'utf-8' }
    multi method Str(utf8:D:) { self.decode }
}

my class utf16 does Blob[uint16] is repr('VMArray') {
    method decode(utf16:D: $encoding = 'utf-16') {
        my $enc = NORMALIZE_ENCODING($encoding);
        die "Can not decode a utf-16 buffer as if it were $encoding"
            unless $enc eq 'utf16';
        nqp::p6box_s(nqp::decode(self, 'utf16'))
    }
    method encoding() { 'utf-16' }
    multi method Str(utf16:D:) { self.decode }
}

my class utf32 does Blob[uint32] is repr('VMArray') {
    method decode(utf32:D: $encoding = 'utf-32') {
        my $enc = NORMALIZE_ENCODING($encoding);
        die "Can not decode a utf-32 buffer as if it were $encoding"
            unless $enc eq 'utf32';
        nqp::p6box_s(nqp::decode(self, 'utf32'))
    }
    method encoding() { 'utf-32' }
    multi method Str(utf32:D:) { self.decode }
}

my role Buf[::T = uint8] does Blob[T] is repr('VMArray') is array_type(T) {
    # TODO: override at_pos so we get mutability
}

constant buf8 = Buf[uint8];
constant buf16 = Buf[uint16];
constant buf32 = Buf[uint32];
constant buf64 = Buf[uint64];

multi sub pack(Str $template, *@items) {
    my @bytes;
    for $template.comb(/<[a..zA..Z]>[\d+|'*']?/) -> $unit {
        my $directive = $unit.substr(0, 1);
        my $amount = $unit.substr(1);

        given $directive {
            when 'A' {
                my $ascii = shift @items // '';
                for $ascii.comb -> $char {
                    X::Buf::Pack::NonASCII.new(:$char).throw if ord($char) > 0x7f;
                    @bytes.push: ord($char);
                }
                if $amount ne '*' {
                    @bytes.push: 0x20 xx ($amount - $ascii.chars);
                }
            }
            when 'H' {
                my $hexstring = shift @items // '';
                if $hexstring % 2 {
                    $hexstring ~= '0';
                }
                @bytes.push: map { :16($_) }, $hexstring.comb(/../);
            }
            when 'x' {
                if $amount eq '*' {
                    $amount = 0;
                }
                elsif $amount eq '' {
                    $amount = 1;
                }
                @bytes.push: 0x00 xx $amount;
            }
            when 'C' {
                my $number = shift(@items);
                @bytes.push: $number % 0x100;
            }
            when 'S' | 'v' {
                my $number = shift(@items);
                @bytes.push: ($number, $number +> 0x08) >>%>> 0x100;
            }
            when 'L' | 'V' {
                my $number = shift(@items);
                @bytes.push: ($number, $number +> 0x08,
                              $number +> 0x10, $number +> 0x18) >>%>> 0x100;
            }
            when 'n' {
                my $number = shift(@items);
                @bytes.push: ($number +> 0x08, $number) >>%>> 0x100;
            }
            when 'N' {
                my $number = shift(@items);
                @bytes.push: ($number +> 0x18, $number +> 0x10,
                              $number +> 0x08, $number) >>%>> 0x100;
            }
            X::Buf::Pack.new(:$directive).throw;
        }
    }

    return Buf.new(@bytes);
}

multi infix:<~>(Blob:D $a, Blob:D $b) {
    my $res := ($a.WHAT === $b.WHAT ?? $a !! Buf).new;
    my $adc := nqp::decont($a);
    my $bdc := nqp::decont($b);
    my int $alen = nqp::elems($adc);
    my int $blen = nqp::elems($bdc);
    nqp::setelems($res, $alen + $blen);
    my int $s = 0;
    my int $d = 0;
    while $s < $alen {
        nqp::bindpos_i($res, $d, nqp::atpos_i($adc, $s));
        $s = $s + 1;
        $d = $d + 1;
    }
    $s = 0;
    while $s < $blen {
        nqp::bindpos_i($res, $d, nqp::atpos_i($bdc, $s));
        $s = $s + 1;
        $d = $d + 1;
    }
    $res
}

multi prefix:<~^>(Blob:D $a) {
    $a ~~ Blob[int16] ?? $a.new($a.list.map: 0xFFFF - *) !!
    $a ~~ Blob[int32] ?? $a.new($a.list.map: 0xFFFFFFFF - *) !!
                         $a.new($a.list.map: 0xFF - *);
}

multi sub infix:<~&>(Blob:D $a, Blob:D $b) {
    my $minlen := $a.elems min $b.elems;
    my @anded-contents = $a.list[^$minlen] >>+&<< $b.list[^$minlen];
    @anded-contents.push: 0 xx ($a.elems - @anded-contents.elems);
    @anded-contents.push: 0 xx ($b.elems - @anded-contents.elems);
    ($a.WHAT === $b.WHAT ?? $a !! Buf).new(@anded-contents);
}

multi sub infix:<~|>(Blob:D $a, Blob:D $b) {
    my $minlen = $a.elems min $b.elems;
    my @ored-contents = $a.list[^$minlen] «+|» $b.list[^$minlen];
    @ored-contents.push: $a.list[@ored-contents.elems ..^ $a.elems];
    @ored-contents.push: $b.list[@ored-contents.elems ..^ $b.elems];
    ($a.WHAT === $b.WHAT ?? $a !! Buf).new(@ored-contents);
}

multi sub infix:<~^>(Blob:D $a, Blob:D $b) {
    my $minlen = $a.elems min $b.elems;
    my @xored-contents = $a.list[^$minlen] «+^» $b.list[^$minlen];
    @xored-contents.push: $a.list[@xored-contents.elems ..^ $a.elems];
    @xored-contents.push: $b.list[@xored-contents.elems ..^ $b.elems];
    ($a.WHAT === $b.WHAT ?? $a !! Buf).new(@xored-contents);
}

multi infix:<eqv>(Blob:D $a, Blob:D $b) {
    if $a.WHAT === $b.WHAT && $a.elems == $b.elems {
        my int $n  = $a.elems;
        my int $i  = 0;
        my Mu $da := nqp::decont($a);
        my Mu $db := nqp::decont($b);
        while $i < $n {
            return False unless nqp::iseq_i(nqp::atpos_i($da, $i), nqp::atpos_i($db, $i));
            $i = $i + 1;
        }
        True
    }
    else {
        False
    }
}

multi sub infix:<cmp>(Blob:D $a, Blob:D $b) {
    [||] $a.list Z<=> $b.list or $a.elems <=> $b.elems
}

multi sub infix:<eq>(Blob:D $a, Blob:D $b) {
    $a.elems == $b.elems && $a.list eq $b.list
}

multi sub infix:<ne>(Blob:D $a, Blob:D $b) {
    not $a eq $b;
}

multi sub infix:<lt>(Blob:D $a, Blob:D $b) {
    ($a cmp $b) == -1
}

multi sub infix:<gt>(Blob:D $a, Blob:D $b) {
    ($a cmp $b) ==  1
}

multi sub infix:<le>(Blob:D $a, Blob:D $b) {
    ($a cmp $b) !=  1
}

multi sub infix:<ge>(Blob:D $a, Blob:D $b) {
    ($a cmp $b) != -1
}
