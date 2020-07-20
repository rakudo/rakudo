use nqp;

package EXPORT::cached {
    multi sub trait_mod:<is>(Routine $r, :$cached!) {
        my %cache;
        $r.wrap(-> |c {
            my $key := c.gist;
            %cache.EXISTS-KEY($key)
              ?? %cache{$key}
              !! (%cache{$key} := callsame);
        });
    }

    multi sub trait_mod:<is>(Method $m, :$cached!) {
        X::NYI.new(:feature("'is cached' on methods")).throw;
    }

    OUR::{'&trait_mod:<is>'} := &trait_mod:<is>;
}

package EXPORT::macros {
    OUR::<EXPERIMENTAL-MACROS> := True;
}

package EXPORT::smallnatives {
    our native int1 is repr('P6int') is Int is nativesize( 1) { }
    our native int2 is repr('P6int') is Int is nativesize( 2) { }
    our native int4 is repr('P6int') is Int is nativesize( 4) { }
    our native uint1 is repr('P6int') is Int is nativesize( 1) is unsigned { }
    our native   bit is repr('P6int') is Int is nativesize( 1) is unsigned { }
    our native uint2 is repr('P6int') is Int is nativesize( 2) is unsigned { }
    our native uint4 is repr('P6int') is Int is nativesize( 4) is unsigned { }
}

package EXPORT::pack {
    proto sub pack($, |) {*}
    multi sub pack(Str $template, *@items) {
        pack($template.comb(/<[a..zA..Z]>[\d+|'*']?/), @items)
    }

    multi sub pack(@template, *@items) {
        my @bytes;
        for @template -> $unit {
            my $directive = substr($unit,0,1);
            my $amount    = substr($unit,1);

            given $directive {
                when 'A' {
                    my $ascii = shift @items // '';
                    my $data = $ascii.ords.cache;
                    if $amount eq '*' {
                        $amount = $data.elems;
                    }
                    if $amount eq '' {
                        $amount = 1;
                    }
                    for (@$data, 0x20 xx *).flat[^$amount] -> $byte {
                        X::Buf::Pack::NonASCII.new(:char($byte.chr)).throw if $byte > 0x7f;
                        @bytes.push: $byte;
                    }
                }
                when 'a' {
                    my $data = shift @items // Buf.new;
                    $data.=encode if nqp::istype($data,Str);
                    if $amount eq '*' {
                        $amount = $data.elems;
                    }
                    if $amount eq '' {
                        $amount = 1;
                    }
                    for (@$data, 0 xx *).flat[^$amount] -> $byte {
                        @bytes.push: $byte;
                    }
                }
                when 'H' {
                    my $hexstring = shift @items // '';
                    if $hexstring.chars % 2 {
                        $hexstring ~= '0';
                    }
                    @bytes.append: map { :16($_) }, $hexstring.comb(/../);
                }
                when 'x' {
                    if $amount eq '*' {
                        $amount = 0;
                    }
                    elsif $amount eq '' {
                        $amount = 1;
                    }
                    @bytes.append: 0x00 xx $amount;
                }
                when 'C' {
                    my $number = shift(@items);
                    @bytes.push: $number % 0x100;
                }
                when 'S' | 'v' {
                    my $number = shift(@items);
                    @bytes.append: ($number, $number +> 0x08) >>%>> 0x100;
                }
                when 'L' | 'V' {
                    my $number = shift(@items);
                    @bytes.append: ($number, $number +> 0x08,
                                  $number +> 0x10, $number +> 0x18) >>%>> 0x100;
                }
                when 'n' {
                    my $number = shift(@items);
                    @bytes.append: ($number +> 0x08, $number) >>%>> 0x100;
                }
                when 'N' {
                    my $number = shift(@items);
                    @bytes.append: ($number +> 0x18, $number +> 0x10,
                                  $number +> 0x08, $number) >>%>> 0x100;
                }
                X::Buf::Pack.new(:$directive).throw;
            }
        }

        return Buf.new(@bytes);
    }

    proto sub unpack(|) {*}
    multi sub unpack(Blob:D \blob, Str:D $template) {
        unpack(blob, $template.comb(/<[a..zA..Z]>[\d+|'*']?/))
    }
    multi sub unpack(Blob:D \blob, @template) {
        my @bytes = blob.list;
        my @fields;
        for @template -> $unit {
            my $directive = substr($unit,0,1);
            my $amount    = substr($unit,1);
            my $pa = $amount eq ''  ?? 1            !!
                     $amount eq '*' ?? @bytes.elems !! +$amount;

            given $directive {
                when 'a' | 'A' | 'Z' {
                    @fields.push: @bytes.splice(0, $pa).map(&chr).join;
                }
                when 'H' {
                    my str $hexstring = '';
                    for ^$pa {
                        my $byte = shift @bytes;
                        $hexstring ~= ($byte +> 4).fmt('%x')
                                    ~ ($byte % 16).fmt('%x');
                    }
                    @fields.push($hexstring);
                }
                when 'x' {
                    splice @bytes, 0, $pa;
                }
                when 'C' {
                    @fields.append: @bytes.splice(0, $pa);
                }
                when 'S' | 'v' {
                    for ^$pa {
                        last if @bytes.elems < 2;
                        @fields.append: shift(@bytes)
                                    + (shift(@bytes) +< 0x08);
                    }
                }
                when 'L' | 'V' {
                    for ^$pa {
                        last if @bytes.elems < 4;
                        @fields.append: shift(@bytes)
                                    + (shift(@bytes) +< 0x08)
                                    + (shift(@bytes) +< 0x10)
                                    + (shift(@bytes) +< 0x18);
                    }
                }
                when 'n' {
                    for ^$pa {
                        last if @bytes.elems < 2;
                        @fields.append: (shift(@bytes) +< 0x08)
                                    + shift(@bytes);
                    }
                }
                when 'N' {
                    for ^$pa {
                        last if @bytes.elems < 4;
                        @fields.append: (shift(@bytes) +< 0x18)
                                    + (shift(@bytes) +< 0x10)
                                    + (shift(@bytes) +< 0x08)
                                    + shift(@bytes);
                    }
                }
                X::Buf::Pack.new(:$directive).throw;
            }
        }

        return |@fields;
    }

    BEGIN OUR::<EXPERIMENTAL-PACK> := &unpack;  # BEGIN is needed somehow
    OUR::{'&pack'}           := &pack;
}

package EXPORT::collation {
    # this is no longer experimental, but keep the package to prevent
    # code that caters to this and earlier versions of compilers from
    # breaking
    #
    # XXX TODO: should be fine to remove on 2019-12. There is also a test
    # in t/02-rakudo/99-misc.t that will need to be removed too at the time
}

# vim: expandtab shiftwidth=4
