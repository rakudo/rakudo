my class NFC is repr('VMArray') is array_type(uint32) { ... }
my class NFD is repr('VMArray') is array_type(uint32) { ... }
my class NFKC is repr('VMArray') is array_type(uint32) { ... }
my class NFKD is repr('VMArray') is array_type(uint32) { ... }

my class X::InvalidCodepoint { ... }
my class Encoding::Registry { ... }

my class Uni does Positional[uint32] does Stringy is repr('VMArray') is array_type(uint32) {

    multi method new(Uni:) { nqp::create(self) }
    multi method new(Uni: *@codes) {
        @codes.elems;  # reify and test for lazy sequences
        my $uni        := nqp::create(self);
        my $codepoints := nqp::getattr(@codes,List,'$!reified');
        my $code;

        nqp::while(
          nqp::elems($codepoints),
          nqp::if(nqp::isgt_i($code = nqp::shift($codepoints), 0x10ffff)
                  || (nqp::isle_i(0xd800, $code) && nqp::isle_i($code, 0xdfff))
                  || nqp::islt_i($code, 0),
            X::InvalidCodepoint.new(:$code).throw,
            nqp::push_i($uni,$code)));

        $uni
    }
    # array[uint32] candidate added in core_epilogue

    my class UniList does PredictiveIterator {
        has $!uni;
        has int $!els;
        has int $!i;
        method !SET-SELF(\uni) {
            $!uni := uni;
            $!i    = -1;
            $!els  = nqp::elems(uni);
            self
        }
        method new (\uni) { nqp::create(self)!SET-SELF: uni }
        method pull-one {
            nqp::islt_i(++$!i,$!els)
              ?? nqp::atpos_u($!uni, $!i)
              !! IterationEnd
        }
        method skip-one {
            nqp::islt_i(++$!i,$!els)
        }
        method push-all(\target --> IterationEnd) {
            my     $uni := $!uni; # lexicals faster than attrs
            my int $els  = $!els;
            my int $i    = $!i;
            nqp::while(
              nqp::islt_i(++$i,$els),
              target.push: nqp::atpos_u($uni, $i)
            );
            $!i = $i;
        }
        method count-only(--> Int:D) {
            nqp::p6box_i($!els - $!i - nqp::islt_i($!i,$!els))
        }
        method sink-all(--> IterationEnd) { $!i = $!els }
    }
    method list(Uni:D:) { Seq.new(UniList.new(self)) }

    method Uni(Uni:D:) {
        self
    }

    method NFC(Uni:D:) {
        nqp::normalizecodes(self, nqp::const::NORMALIZE_NFC, nqp::create(NFC))
    }

    method NFD(Uni:D:) {
        nqp::normalizecodes(self, nqp::const::NORMALIZE_NFD, nqp::create(NFD))
    }

    method NFKC(Uni:D:) {
        nqp::normalizecodes(self, nqp::const::NORMALIZE_NFKC, nqp::create(NFKC))
    }

    method NFKD(Uni:D:) {
        nqp::normalizecodes(self, nqp::const::NORMALIZE_NFKD, nqp::create(NFKD))
    }

    multi method Str(Uni:D:) {
        nqp::strfromcodes(self)
    }

    multi method Bool(Uni:D:) {
        nqp::hllbool(nqp::elems(self));
    }

    method codes(Uni:D:)   { nqp::elems(self) }
    method elems(Uni:D:)   { nqp::elems(self) }
    method Numeric(Uni:D:) { nqp::elems(self) }
    method Int(Uni:D:)     { nqp::elems(self) }

    multi method ACCEPTS(Uni:D: Uni:D $other --> Bool:D) {
        nqp::hllbool(
          nqp::iseq_i(nqp::elems(self),nqp::elems($other))
            && nqp::stmts(
                 (my int $i = -1),
                 (my uint $elems = nqp::elems(self)),
                 nqp::while(
                   nqp::islt_i(++$i,$elems)
                     && nqp::iseq_i(
                          nqp::atpos_u(self,$i),
                          nqp::atpos_u($other,$i)
                        ),
                   nqp::null
                 ),
                 nqp::iseq_i($i,$elems)
               )
        )
    }

    multi method EXISTS-POS(Uni:D: uint $pos) {
        nqp::hllbool(nqp::islt_i($pos,nqp::elems(self)))
    }
    multi method EXISTS-POS(Uni:D: Int:D $pos) {
        $pos < nqp::elems(self) && $pos >= 0;
    }

    multi method AT-POS(Uni:D: uint $pos) {
        nqp::isge_i($pos,nqp::elems(self))
          ?? X::OutOfRange.new(
               :what($*INDEX // 'Index'),
               :got($pos),
               :range("0..{nqp::elems(self)-1}")
             ).Failure
          !! nqp::atpos_u(self,$pos)
    }
    multi method AT-POS(Uni:D: Int:D $pos) {
        nqp::isge_i($pos,nqp::elems(self)) || nqp::islt_i($pos,0)
          ?? X::OutOfRange.new(
               :what($*INDEX // 'Index'),
               :got($pos),
               :range("0..{nqp::elems(self)-1}")
             ).Failure
          !! nqp::atpos_u(self,$pos)
    }

    multi method gist(Uni:D:) {
        self.^name ~ ':0x<' ~ self.list.fmt('%04x', ' ') ~ '>'
    }

    multi method raku(Uni:D:) {
        'Uni.new(' ~ self.list.fmt('0x%04x', ', ') ~ ')' ~
            (self.WHAT === Uni ?? '' !! '.' ~ self.^name);
    }

    method encode(Str:D $encoding = 'utf8',
      :$replacement, Bool() :$translate-nl = False, :$strict --> Blob:D) {
        my $encoder = Encoding::Registry.find($encoding)
            .encoder(:$replacement, :$translate-nl, :$strict);
        my $buf = Buf.new;
        for self.list -> $character {
            $buf.append($encoder.encode-chars($character.chr));
        }
        $buf;
    }
}

my class NFD is Uni {
    method new(|) {
        die "Cannot create an NFD directly"; # XXX typed, better message
    }

    method NFD() { self }
}

my class NFC is Uni {
    method new(|) {
        die "Cannot create an NFC directly"; # XXX typed, better message
    }

    method NFC() { self }
}

my class NFKD is Uni {
    method new(|) {
        die "Cannot create an NFKD directly"; # XXX typed, better message
    }

    method NFKD() { self }
}

my class NFKC is Uni {
    method NFKC() { self }

    method new(|) {
        die "Cannot create an NFKC directly"; # XXX typed, better message
    }
}

multi sub infix:<cmp>(Uni:D $a, Uni:D $b) {
    my uint $elems-a = nqp::elems($a);
    my uint $elems-b = nqp::elems($b);
    my uint $elems   = nqp::islt_i($elems-a,$elems-b) ?? $elems-a !! $elems-b;

    my int $i = -1;
    nqp::until(
      nqp::isge_i(++$i,$elems)
        || (my $res = nqp::cmp_i(nqp::atpos_u($a,$i),nqp::atpos_u($b,$i))),
      nqp::null
    );
    ORDER($res || nqp::cmp_i($elems-a,$elems-b))
}

# vim: expandtab shiftwidth=4
