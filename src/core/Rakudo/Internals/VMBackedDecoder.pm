my class Rakudo::Internals::VMBackedDecoder is repr('Decoder') {
    method new(str $encoding) {
        nqp::decoderconfigure(nqp::create(self), $encoding, nqp::hash())
    }

    method add-bytes(Blob:D $bytes --> Nil) {
        nqp::decoderaddbytes(self, nqp::decont($bytes));
    }

    method consume-available-chars() returns Str {
        nqp::decodertakeavailablechars(self)
    }

    method consume-all-chars() returns Str {
        nqp::decodertakeallchars(self)
    }
}
