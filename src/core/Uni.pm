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

    method list() {
        gather {
            my int $n = nqp::elems(self);
            loop (my int $i = 0; $i < $n; $i++) {
                take nqp::atpos_i(self, $i);
            }
        }
    }

    method Uni() {
        self
    }

    method NFC() {
        nqp::normalizecodes(self, nqp::const::NORMALIZE_NFC, nqp::create(NFC))
    }

    method NFD() {
        nqp::normalizecodes(self, nqp::const::NORMALIZE_NFD, nqp::create(NFD))
    }

    method NFKC() {
        nqp::normalizecodes(self, nqp::const::NORMALIZE_NFKC, nqp::create(NFKC))
    }

    method NFKD() {
        nqp::normalizecodes(self, nqp::const::NORMALIZE_NFKD, nqp::create(NFKD))
    }

    method Str() {
        nqp::strfromcodes(self)
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
