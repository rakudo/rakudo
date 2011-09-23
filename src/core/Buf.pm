# the real Buf should be something parametric and much more awesome.
# this is merely a placeholder until people who know their stuff build
# the Real Thing.

class Buf does Positional {
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
    method !set_codes(*@codes) {
        pir::set__vPI($!buffer, nqp::unbox_i(@codes.elems));
        for @codes.keys -> $k {
            pir::set__1Qii($!buffer, nqp::unbox_i($k), nqp::unbox_i(@codes[$k]));
        }
        self;
    }

    method at_key(Buf:D: Int:D $idx) {
        nqp::p6box_i(pir::set__IQi($!buffer, nqp::unbox_i($idx)));
    }

    method elems(Buf:D:) {
        nqp::p6box_i(nqp::elems($!buffer));
    }

    method Numeric { self.elems }
    method Int     { self.elems }

    method list() {
        my @l;
        for ^nqp::p6box_i(nqp::elems($!buffer)) -> $k {
            @l.push: self.at_key($k);
        }
        @l;
    }

    method gist() {
        'Buf:0x<' ~ self.list.fmt('%02x', ' ') ~ '>'
    }

    method decode(Str:D $encoding = 'utf8') {
        nqp::p6box_s $!buffer.get_string(
            nqp::unbox_s $encoding.lc
        );
    }
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

    my Mu $ba := nqp::getattr(pir::perl6_decontainerize__PP($a), Buf, '$!buffer');
    my Mu $bb := nqp::getattr(pir::perl6_decontainerize__PP($b), Buf, '$!buffer');
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

