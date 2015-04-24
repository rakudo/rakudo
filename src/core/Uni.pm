my class NFC is repr('VMArray') is array_type(uint32) { ... }
my class NFD is repr('VMArray') is array_type(uint32) { ... }
my class NFKC is repr('VMArray') is array_type(uint32) { ... }
my class NFKD is repr('VMArray') is array_type(uint32) { ... }

my class Uni does Positional[uint32] does Stringy is repr('VMArray') is array_type(uint32) {
    method new(*@codes) {
        my $uni := nqp::create(self);
        my int $n = @codes.elems;
        loop (my int $i = 0; $i < $n; $i++) {
            nqp::bindpos_i($uni, $i, @codes.AT-POS($i));
        }
        $uni
    }

    method list(Uni:D:) {
        gather {
            my int $n = nqp::elems(self);
            loop (my int $i = 0; $i < $n; $i++) {
                take nqp::atpos_i(self, $i);
            }
        }
    }

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

    method codes(Uni:D:) { nqp::elems(self) }

    multi method EXISTS-POS(Uni:D: int \pos) {
        nqp::p6bool(
          nqp::islt_i(pos,nqp::elems(self)) && nqp::isge_i(pos,0)
        );
    }
    multi method EXISTS-POS(Uni:D: Int:D \pos) {
        pos < nqp::elems(self) && pos >= 0;
    }

    multi method AT-POS(Uni:D: int \pos) {
        fail X::OutOfRange.new(
          :what<Index>,
          :got(pos),
          :range("0..{nqp::elems(self)-1}")
        ) if nqp::isge_i(pos,nqp::elems(self)) || nqp::islt_i(pos,0);
        nqp::atpos_i(self, pos);
    }
    multi method AT-POS(Uni:D: Int:D \pos) {
        my int $pos = nqp::unbox_i(pos);
        fail X::OutOfRange.new(
          :what<Index>,
          :got(pos),
          :range("0..{nqp::elems(self)-1}")
        ) if nqp::isge_i($pos,nqp::elems(self)) || nqp::islt_i($pos,0);
        nqp::atpos_i(self,$pos);
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
        die "Cannot create an NFD directly"; # XXX typed, better message
    }

    method NFC() { self }
}

my class NFKD is Uni {
    method new(|) {
        die "Cannot create an NFD directly"; # XXX typed, better message
    }

    method NFKD() { self }
}

my class NFKC is Uni {
    method NFKC() { self }

    method new(|) {
        die "Cannot create an NFD directly"; # XXX typed, better message
    }
}
