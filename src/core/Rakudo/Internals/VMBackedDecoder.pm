my class Supply { ... }

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

augment class Rakudo::Internals {
    method BYTE_SUPPLY_DECODER(Supply:D $bin-supply, Str:D $enc) {
        my $norm-enc = self.NORMALIZE_ENCODING($enc);
        supply {
            my $decoder = Rakudo::Internals::VMBackedDecoder.new($norm-enc);
            whenever $bin-supply {
                $decoder.add-bytes($_);
                my $available = $decoder.consume-available-chars();
                emit $available if $available ne '';
                LAST {
                    with $decoder {
			my $rest = .consume-all-chars();
                        emit $rest if $rest ne '';
                    }
                }
            }
        }
    }
}
