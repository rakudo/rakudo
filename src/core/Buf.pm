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

    # append_inplace is a big hack, but seems to be necessary
    # performance wise for reading binary data in chunks
    method append_inplace(Buf:D: Buf:D $other) {
        my Mu $other_buf := nqp::getattr(pir::perl6_decontainerize__PP($other), Buf, '$!buffer');
        pir::set__vPs($!buffer, nqp::concat_s(
                  $!buffer.get_string('binary'),
                $other_buf.get_string('binary')
            )
        );
        self;
    }
}

multi infix:<eqv>(Buf:D $a, Buf:D $b) {
    $a.WHAT === $b.WHAT && $a.list eqv $b.list;
}
multi prefix:<~^>(Buf:D $a) {
    Buf.new($a.list.map: 255 - *);
}
