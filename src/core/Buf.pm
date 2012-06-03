# the real Buf should be something parametric and much more awesome.
# this is merely a placeholder until people who know their stuff build
# the Real Thing.

my class X::Buf::AsStr { ... };
my class X::Buf::Pack  { ... };
my class X::Buf::Pack::NonASCII  { ... };

my class Buf does Positional {
    has Mu $!buffer;
    method BUILD() {
        $!buffer := pir::new__PS('ByteBuffer');
        1;
    }
    method new(*@codes) {
        my $new := self.bless(*);
        $new!set_codes(@codes);
        $new;
    }
    method !set_codes(@codes) {
        my int $bytes = @codes.elems;
        pir::set__vPI($!buffer, $bytes);
        my int $i = 0;
        while $i < $bytes {
            nqp::bindpos_i($!buffer, $i, nqp::unbox_i(@codes[$i]));
            $i = $i + 1;
        }
        self;
    }

    method at_pos(Buf:D: Int:D $idx) {
        nqp::p6box_i(nqp::atpos_i($!buffer, nqp::unbox_i($idx)));
    }

    multi method Bool(Buf:D:) {
        nqp::p6bool(nqp::elems($!buffer));
    }

    method elems(Buf:D:) {
        nqp::p6box_i(nqp::elems($!buffer));
    }
    method bytes(Buf:D:) { self.elems }
    method chars()       { X::Buf::AsStr.new(method => 'chars').throw }
    multi method Str()   { X::Buf::AsStr.new(method => 'Str'  ).throw }


    method Numeric { self.elems }
    method Int     { self.elems }

    method list() {
        my @l;
        my int $bytes = nqp::elems($!buffer);
        my int $i = 0;
        while $i < $bytes {
            @l[$i] = nqp::p6box_i(nqp::atpos_i($!buffer, $i));
            $i = $i + 1;
        }
        @l;
    }

    method gist() {
        'Buf:0x<' ~ self.list.fmt('%02x', ' ') ~ '>'
    }

    method decode(Str:D $encoding = 'utf8') {
        nqp::p6box_s $!buffer.get_string(
            nqp::unbox_s PARROT_ENCODING($encoding)
        );
    }

    method subbuf(Buf:D: $from = 0, $len = self.elems) {
        my $ret := nqp::create(self);
        nqp::bindattr($ret, Buf, '$!buffer',
            nqp::substr($!buffer.get_string('binary'),
                nqp::unbox_i($from.Int),
                nqp::unbox_i($len.Int)
            )
        );
        $ret;
    }

    method unpack(Buf:D: $template) {
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
    method contents(Buf:D:) { self.list }
}

multi infix:<eqv>(Buf:D $a, Buf:D $b) {
    $a.WHAT === $b.WHAT && $a.list eqv $b.list;
}
multi prefix:<~^>(Buf:D $a) {
    Buf.new($a.list.map: 255 - *);
}
multi infix:<~>(Buf:D $a, Buf:D $b) {
    my Buf $r := nqp::create(Buf);
    my Mu $br := pir::new__PS('ByteBuffer');

    my Mu $ba := nqp::getattr(nqp::p6decont($a), Buf, '$!buffer');
    my Mu $bb := nqp::getattr(nqp::p6decont($b), Buf, '$!buffer');
    pir::set__vPs($br, nqp::concat_s($ba.get_string('binary'), $bb.get_string('binary')));
    nqp::bindattr($r, Buf, '$!buffer', $br);
    $r;
}
multi sub infix:<~&>(Buf:D $a, Buf:D $b) {
    my $minlen := $a.elems min $b.elems;
    my @anded-contents = $a.list[^$minlen] >>+&<< $b.list[^$minlen];
    @anded-contents.push: 0 xx ($a.elems - @anded-contents.elems);
    @anded-contents.push: 0 xx ($b.elems - @anded-contents.elems);
    Buf.new(@anded-contents);
}


multi sub infix:<~|>(Buf:D $a, Buf:D $b) {
    my $minlen = $a.elems min $b.elems;
    my @ored-contents = $a.list[^$minlen] «+|» $b.list[^$minlen];
    @ored-contents.push: $a.list[@ored-contents.elems ..^ $a.elems];
    @ored-contents.push: $b.list[@ored-contents.elems ..^ $b.elems];
    Buf.new(@ored-contents);
}

multi sub infix:<~^>(Buf:D $a, Buf:D $b) {
    my $minlen = $a.elems min $b.elems;
    my @xored-contents = $a.list[^$minlen] «+^» $b.list[^$minlen];
    @xored-contents.push: $a.list[@xored-contents.elems ..^ $a.elems];
    @xored-contents.push: $b.list[@xored-contents.elems ..^ $b.elems];
    Buf.new(@xored-contents);
}

multi sub infix:<cmp>(Buf:D $a, Buf:D $b) {
    [||] $a.list Z<=> $b.list or $a.elems <=> $b.elems
}
multi sub infix:<eq>(Buf:D $a, Buf:D $b) {
    $a.elems == $b.elems && $a.list eq $b.list
}
multi sub infix:<ne>(Buf:D $a, Buf:D $b) {
    not $a eq $b;
}
multi sub infix:<lt>(Buf:D $a, Buf:D $b) {
    ($a cmp $b) == -1
}
multi sub infix:<gt>(Buf:D $a, Buf:D $b) {
    ($a cmp $b) ==  1
}
multi sub infix:<le>(Buf:D $a, Buf:D $b) {
    ($a cmp $b) !=  1
}
multi sub infix:<ge>(Buf:D $a, Buf:D $b) {
    ($a cmp $b) != -1
}

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
