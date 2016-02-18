my class X::Buf::AsStr          { ... }
my class X::Buf::Pack           { ... }
my class X::Buf::Pack::NonASCII { ... }
my class X::Cannot::Lazy        { ... }
my class X::Experimental        { ... }

my role Blob[::T = uint8] does Positional[T] does Stringy is repr('VMArray') is array_type(T) {
    multi method new() {
        nqp::create(self)
    }
    multi method new(@values) {
        my $buf := nqp::create(self);
        # the fast route
        if nqp::istype(@values,Iterable) {
            my $iter := @values.iterator;
            my $pulled;
            nqp::push_i($buf,$pulled)
              until ($pulled := $iter.pull-one) =:= IterationEnd;
        }

        # the slow route
        else {
            my int $n = @values.elems;
            my int $i = -1;
            nqp::setelems($buf, $n);
            nqp::bindpos_i($buf, $i, @values.AT-POS($i))
              while nqp::islt_i($i = nqp::add_i($i,1),$n);
        }
        $buf
    }
    multi method new(*@values) {
        self.new(@values)
    }

    multi method EXISTS-POS(Blob:D: int \pos) {
        nqp::p6bool(
          nqp::islt_i(pos,nqp::elems(self)) && nqp::isge_i(pos,0)
        );
    }
    multi method EXISTS-POS(Blob:D: Int:D \pos) {
        pos < nqp::elems(self) && pos >= 0;
    }

    multi method AT-POS(Blob:D: int \pos) {
        fail X::OutOfRange.new(
          :what($*INDEX // 'Index'),
          :got(pos),
          :range("0..{nqp::elems(self)-1}")
        ) if nqp::isge_i(pos,nqp::elems(self)) || nqp::islt_i(pos,0);
        nqp::atpos_i(self, pos);
    }
    multi method AT-POS(Blob:D: Int:D \pos) {
        my int $pos = nqp::unbox_i(pos);
        fail X::OutOfRange.new(
          :what($*INDEX // 'Index'),
          :got(pos),
          :range("0..{nqp::elems(self)-1}")
        ) if nqp::isge_i($pos,nqp::elems(self)) || nqp::islt_i($pos,0);
        nqp::atpos_i(self,$pos);
    }

    multi method Bool(Blob:D:) {
        nqp::p6bool(nqp::elems(self));
    }

    method elems(Blob:D:) {
        nqp::p6box_i(nqp::elems(self));
    }
    method bytes(Blob:D:) {
        ceiling(self.elems * ::T.^nativesize / 8);
    }
    method chars(Blob:D:)       { X::Buf::AsStr.new(method => 'chars').throw }
    multi method Str(Blob:D:)   { X::Buf::AsStr.new(method => 'Str'  ).throw }
    multi method Stringy(Blob:D:) { X::Buf::AsStr.new(method => 'Stringy' ).throw }

    method Numeric(Blob:D:) { self.elems }
    method Int(Blob:D:)     { self.elems }

    method decode(Blob:D: $encoding = 'utf-8') {
        nqp::p6box_s(
          nqp::decode(self, Rakudo::Internals.NORMALIZE_ENCODING($encoding)))
    }

    method list(Blob:D:) {
        my int $n = nqp::elems(self);
        my $list := nqp::list;
        nqp::setelems($list,$n);
        my int $i = -1;
        nqp::bindpos($list,$i,nqp::atpos_i(self, $i))
          while nqp::islt_i($i = nqp::add_i($i,1),$n);
        $list
    }

    multi method gist(Blob:D:) {
        self.^name ~ ':0x<' ~ self.list.fmt('%02x', ' ') ~ '>'
    }
    multi method perl(Blob:D:) {
        self.^name ~ '.new(' ~ self.list.join(', ') ~ ')';
    }

    method subbuf(Blob:D: $from, $length?) {

        my int $elems = nqp::elems(self);
        X::OutOfRange.new(
          what => "Len element to subbuf",
          got  => $length,
          range => "0..$elems",
        ).fail if $length.DEFINITE && $length < 0;

        my int $pos;
        my int $todo;
        if nqp::istype($from,Range) {
            ($pos,my int $max) = $from.int-bounds;
            $todo = $max - $pos + 1;
        }
        else {
            $pos = nqp::istype($from, Callable) ?? $from($elems) !! $from.Int;
            $todo = $length.DEFINITE
              ?? $length.Int min $elems - $pos
              !! $elems - $pos;
        }

        X::OutOfRange.new(
          what    => 'From argument to subbuf',
          got     => $from.gist,
          range   => "0..$elems",
          comment => "use *{$pos} if you want to index relative to the end",
        ).fail if $pos < 0;
        X::OutOfRange.new(
          what => 'From argument to subbuf',
          got  => $from.gist,
          range => "0..$elems",
        ).fail if $pos > $elems;

        my $subbuf := nqp::create(self);
        if $todo {
            nqp::setelems($subbuf, $todo);
            my int $i = -1;
            $pos = $pos - 1;
            nqp::bindpos_i($subbuf,$i,nqp::atpos_i(self,($pos = $pos + 1)))
              while ($i = $i + 1) < $todo;
        }
        $subbuf
    }

    proto method unpack(|) { * }
    multi method unpack(Blob:D: Str:D $template) {
        nqp::isnull(nqp::getlexcaller('EXPERIMENTAL-PACK')) and X::Experimental.new(
            feature => "the 'unpack' method",
            use     => "pack"
        ).throw;
        self.unpack($template.comb(/<[a..zA..Z]>[\d+|'*']?/))
    }
    multi method unpack(Blob:D: @template) {
        nqp::isnull(nqp::getlexcaller('EXPERIMENTAL-PACK')) and X::Experimental.new(
            feature => "the 'unpack' method",
            use     => "pack"
        ).throw;
        my @bytes = self.list;
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
        my $enc = Rakudo::Internals.NORMALIZE_ENCODING($encoding);
        die "Can not decode a utf-8 buffer as if it were $encoding"
            unless $enc eq 'utf8';
        nqp::p6box_s(nqp::decode(self, 'utf8'))
    }
    method encoding() { 'utf-8' }
    multi method Str(utf8:D:) { self.decode }
    multi method Stringy(utf8:D:) { self.decode }
}

my class utf16 does Blob[uint16] is repr('VMArray') {
    method decode(utf16:D: $encoding = 'utf-16') {
        my $enc = Rakudo::Internals.NORMALIZE_ENCODING($encoding);
        die "Can not decode a utf-16 buffer as if it were $encoding"
            unless $enc eq 'utf16';
        nqp::p6box_s(nqp::decode(self, 'utf16'))
    }
    method encoding() { 'utf-16' }
    multi method Str(utf16:D:) { self.decode }
    multi method Stringy(utf16:D:) { self.decode }
}

my class utf32 does Blob[uint32] is repr('VMArray') {
    method decode(utf32:D: $encoding = 'utf-32') {
        my $enc = Rakudo::Internals.NORMALIZE_ENCODING($encoding);
        die "Can not decode a utf-32 buffer as if it were $encoding"
            unless $enc eq 'utf32';
        nqp::p6box_s(nqp::decode(self, 'utf32'))
    }
    method encoding() { 'utf-32' }
    multi method Str(utf32:D:) { self.decode }
    multi method Stringy(utf32:D:) { self.decode }
}

my role Buf[::T = uint8] does Blob[T] is repr('VMArray') is array_type(T) {
    multi method AT-POS(Buf:D: int \pos) is raw {
        fail X::OutOfRange.new(
          :what($*INDEX // 'Index'),:got(pos),:range<0..Inf>)
            if nqp::islt_i(pos,0);
        nqp::atposref_i(self, pos);
    }
    multi method AT-POS(Buf:D: Int:D \pos) is raw {
        my int $pos = nqp::unbox_i(pos);
        fail X::OutOfRange.new(
          :what($*INDEX // 'Index'),:got(pos),:range<0..Inf>)
            if nqp::islt_i($pos,0);
        nqp::atposref_i(self,$pos);
    }

    multi method ASSIGN-POS(Buf:D: int \pos, Mu \assignee) {
        X::OutOfRange.new(
          :what($*INDEX // 'Index'),:got(pos),:range<0..Inf>).throw
            if nqp::islt_i(pos,0);
        nqp::bindpos_i(self,\pos,assignee)
    }
    multi method ASSIGN-POS(Buf:D: Int:D \pos, Mu \assignee) {
        my int $pos = nqp::unbox_i(pos);
        fail X::OutOfRange.new(
          :what($*INDEX // 'Index'),:got(pos),:range<0..Inf>)
            if nqp::islt_i($pos,0);
        nqp::bindpos_i(self,$pos,assignee)
    }

    multi method push(Buf:D: Mu $value) {
        nqp::push_i(self,$value);
        self
    }
    multi method push(Buf:D: *@values) {
        self!append(@values,'push')
    }

    multi method append(Buf:D: Mu $value) {
        nqp::push_i(self,$value);
        self
    }
    multi method append(Buf:D: @values) {
        self!append(@values,'append')
    }
    multi method append(Buf:D: *@values) {
        self!append(@values,'append')
    }

    method !append(Buf:D: @values, $action) {
        fail X::Cannot::Lazy.new(:$action,:what(self.^name))
          if @values.is-lazy;

        my     $list := nqp::getattr(@values,List,'$!reified');
        my int $elems = nqp::elems($list);
        my int $i     = -1;
        nqp::push_i(self,nqp::atpos($list,$i))
          while nqp::islt_i($i = $i + 1,$elems);
        self
    }
}

constant buf8 = Buf[uint8];
constant buf16 = Buf[uint16];
constant buf32 = Buf[uint32];
constant buf64 = Buf[uint64];

proto sub pack(|) { * }
multi sub pack(Str $template, *@items) {
    nqp::isnull(nqp::getlexcaller('EXPERIMENTAL-PACK')) and X::Experimental.new(
        feature => "the 'pack' function",
        use     => "pack"
    ).throw;
    pack($template.comb(/<[a..zA..Z]>[\d+|'*']?/), @items)
}

multi sub pack(@template, *@items) {
    nqp::isnull(nqp::getlexcaller('EXPERIMENTAL-PACK')) and X::Experimental.new(
        feature => "the 'pack' function",
        use     => "pack"
    ).throw;
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
                    $amount = $data.cache.elems;
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

multi sub infix:<~>(Blob:D $a, Blob:D $b) {
    my $res := ($a.WHAT === $b.WHAT ?? $a !! Buf).new;
    my $adc := nqp::decont($a);
    my $bdc := nqp::decont($b);
    my int $alen = nqp::elems($adc);
    my int $blen = nqp::elems($bdc);

    nqp::splice($res, $adc, 0, $alen);
    nqp::splice($res, $bdc, $alen, $blen);

    $res
}

multi sub prefix:<~^>(Blob:D $a) {
    $a ~~ Blob[int16] ?? $a.new($a.list.map: 0xFFFF - *) !!
    $a ~~ Blob[int32] ?? $a.new($a.list.map: 0xFFFFFFFF - *) !!
                         $a.new($a.list.map: 0xFF - *);
}

multi sub infix:<~&>(Blob:D $a, Blob:D $b) {
    my $minlen := $a.elems min $b.elems;
    my @anded-contents = $a.list[^$minlen] >>+&<< $b.list[^$minlen];
    @anded-contents.append: 0 xx ($a.elems - @anded-contents.elems);
    @anded-contents.append: 0 xx ($b.elems - @anded-contents.elems);
    ($a.WHAT === $b.WHAT ?? $a !! Buf).new(@anded-contents);
}

multi sub infix:<~|>(Blob:D $a, Blob:D $b) {
    my $minlen = $a.elems min $b.elems;
    my @ored-contents = $a.list[^$minlen] «+|» $b.list[^$minlen];
    @ored-contents.append: $a.list[@ored-contents.elems ..^ $a.elems];
    @ored-contents.append: $b.list[@ored-contents.elems ..^ $b.elems];
    ($a.WHAT === $b.WHAT ?? $a !! Buf).new(@ored-contents);
}

multi sub infix:<~^>(Blob:D $a, Blob:D $b) {
    my $minlen = $a.elems min $b.elems;
    my @xored-contents = $a.list[^$minlen] «+^» $b.list[^$minlen];
    @xored-contents.append: $a.list[@xored-contents.elems ..^ $a.elems];
    @xored-contents.append: $b.list[@xored-contents.elems ..^ $b.elems];
    ($a.WHAT === $b.WHAT ?? $a !! Buf).new(@xored-contents);
}

multi sub infix:<eqv>(Blob:D $a, Blob:D $b) {
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

sub subbuf-rw($b is rw, $from = 0, $elems = $b.elems - $from) is rw {
    my Blob $subbuf = $b.subbuf($from, $elems);
    Proxy.new(
        FETCH   => sub ($) { $subbuf },
        STORE   => sub ($, Blob $new) {
            $b = $b.subbuf(0, $from)
               ~ $new
               ~ $b.subbuf($from + $elems);
        }
    );
}

# vim: ft=perl6 expandtab sw=4
