role Buf[::T = Int] does Stringy does Positional {
    has T @.contents;

    multi method new(*@contents) {
        self.bless(*, :contents(@contents.list));
    }

    multi method decode($encoding is copy = 'UTF-8') {
        $encoding .= lc;
        if $encoding eq 'utf-8' {
            $encoding = 'utf8';
        }
        my @contents = @.contents;
        my $str = ~Q:PIR {
            $P0 = find_lex '@contents'

            .local pmc bb
            .local string s
            bb = new ['ByteBuffer']
            .local pmc it
            .local int i
            it = iter $P0
            i = 0
          loop:
            unless it goto done
            $P1 = shift it
            $I1 = $P1
            bb[i] = $I1
            inc i
            goto loop
          done:
            $P1 = find_lex '$encoding'
            $S1 = $P1
            s = bb.'get_string'($S1)
            %r = box s
        };
        return $str;
    }

    multi method unpack($template) {
        my @bytes = @.contents;
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
                    @fields.push:   shift @bytes;
                }
                when 'S' | 'v' {
                    @fields.push:   shift(@bytes)
                                 + (shift(@bytes) +< 0x08);
                }
                when 'L' | 'V' {
                    @fields.push:   shift(@bytes)
                                 + (shift(@bytes) +< 0x08)
                                 + (shift(@bytes) +< 0x10)
                                 + (shift(@bytes) +< 0x18);
                }
                when 'n' {
                    @fields.push:  (shift(@bytes) +< 0x08)
                                 +  shift(@bytes);
                }
                when 'N' {
                    @fields.push:  (shift(@bytes) +< 0x18)
                                 + (shift(@bytes) +< 0x10)
                                 + (shift(@bytes) +< 0x08)
                                 +  shift(@bytes);
                }
                die "Unrecognized directive $directive";
            }
        }

        return |@fields;
    }

    multi method elems() {
        @.contents.elems;
    }

    multi method postcircumfix:<[ ]>($index) {
        @.contents[$index];
    }
}

our multi sub infix:<eqv>(Buf $a, Buf $b) {
    return $a.contents eqv $b.contents;
}

our multi sub prefix:<~^>(Buf $a) {
    my @inverted-contents = $a.contents »+^» 0xFF;
    Buf.new(|@inverted-contents);
}

our multi sub infix:<~&>(Buf $a, Buf $b) {
    my $minlen = $a.elems min $b.elems;
    my @anded-contents = $a.contents[^$minlen] «+&» $b.contents[^$minlen];
    @anded-contents.push: 0 xx ($a.elems - @anded-contents.elems);
    @anded-contents.push: 0 xx ($b.elems - @anded-contents.elems);
    Buf.new(|@anded-contents);
}

our multi sub infix:<~|>(Buf $a, Buf $b) {
    my $minlen = $a.elems min $b.elems;
    my @ored-contents = $a.contents[^$minlen] «+|» $b.contents[^$minlen];
    @ored-contents.push: $a.contents[@ored-contents.elems ..^ $a.elems];
    @ored-contents.push: $b.contents[@ored-contents.elems ..^ $b.elems];
    Buf.new(|@ored-contents);
}

our multi sub infix:<~^>(Buf $a, Buf $b) {
    my $minlen = $a.elems min $b.elems;
    my @xored-contents = $a.contents[^$minlen] «+^» $b.contents[^$minlen];
    @xored-contents.push: $a.contents[@xored-contents.elems ..^ $a.elems];
    @xored-contents.push: $b.contents[@xored-contents.elems ..^ $b.elems];
    Buf.new(|@xored-contents);
}

our multi sub pack(Str $template, *@items) {
    my @bytes;
    for $template.comb(/<[a..zA..Z]>[\d+|'*']?/) -> $unit {
        my $directive = $unit.substr(0, 1);
        my $amount = $unit.substr(1);

        given $directive {
            when 'A' {
                my $ascii = shift @items // '';
                for $ascii.comb -> $char {
                    die "Non-ASCII character $char" if ord($char) > 0x7f;
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
                @bytes.push: ($number,         $number +> 0x08,
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
            die "Unrecognized directive $directive";
        }
    }

    return Buf.new(@bytes);
}
