my class Encoding::Decoder::Builtin is repr('Decoder') does Encoding::Decoder {
    method new(str $encoding, :$translate-nl, :$replacement, :$strict) {
        nqp::decoderconfigure(nqp::create(self), $encoding,
            nqp::hash(
                'translate_newlines', $translate-nl ?? 1 !! 0,
                'replacement', $replacement.defined ?? nqp::unbox_s($replacement) !! nqp::null_s(),
                'config', $strict ?? 0 !! 1
                # Config set to 0 uses the decoder's new default, which is strict
                # decoding. Setting to 1 uses the 6.c specced functionality where
                # unmapped codepoints will still decode, e.g. codepoint 129, which
                # in windows-1252 does not exist.

                # In 6.d, 'config' will default to 0
            )
        )
    }

    method add-bytes(Blob:D $bytes --> Nil) {
        nqp::decoderaddbytes(self, nqp::decont($bytes));
    }

    method consume-available-chars(--> Str:D) {
        nqp::decodertakeavailablechars(self)
    }

    method consume-all-chars(--> Str:D) {
        nqp::decodertakeallchars(self)
    }

    method consume-exactly-chars(int $chars, Bool:D :$eof = False --> Str) {
        my str $result = $eof
            ?? nqp::decodertakecharseof(self, $chars)
            !! nqp::decodertakechars(self, $chars);
        nqp::isnull_s($result) ?? Str !! $result
    }

    method set-line-separators(@seps --> Nil) {
        my $sep-strs := nqp::list_s();
        nqp::push_s($sep-strs, .Str) for @seps;
        nqp::decodersetlineseps(self, $sep-strs);
    }

    method consume-line-chars(Bool:D :$chomp = False, Bool:D :$eof = False --> Str) {
        my str $line = nqp::decodertakeline(self, $chomp, $eof);
        nqp::isnull_s($line) ?? Str !! $line
    }

    method is-empty() {
        nqp::hllbool(nqp::decoderempty(self))
    }

    method bytes-available() {
        nqp::decoderbytesavailable(self)
    }

    method consume-exactly-bytes(int $bytes --> Blob) {
        nqp::ifnull(nqp::decodertakebytes(self, nqp::create(buf8.^pun), $bytes), Blob)
    }
}

my class Supply { ... }
my class Encoding::Registry { ... }
augment class Rakudo::Internals {
    method BYTE_SUPPLY_DECODER(Supply:D $bin-supply, Str:D $enc, :$translate-nl) {
        supply {
            my $decoder = Encoding::Registry.find($enc).decoder(:$translate-nl);
            my $valid = True;
            whenever $bin-supply {
                $decoder.add-bytes($_);
                my $available;
                {
                    $available = $decoder.consume-available-chars();
                    CATCH {
                        $valid = False;
                    }
                }
                emit $available if $available ne '';
                LAST {
                    # XXX The `with` is required due to a bug where the
                    # LAST phaser is not properly scoped if we don't get
                    # any bytes. Since that means there's nothing to emit
                    # anyway, we'll not worry about this case for now.
                    #
                    # --- or at least that was the the idea before we fixed
                    # that bug: https://colabti.org/irclogger/irclogger_log/perl6?date=2016-12-07#l1192
                    # and tried removing the `with` in 58cdfd8, but then the error
                    # `No such method 'consume-all-chars' for invocant of type 'Any`
                    # started popping up on Proc::Async tests, so...
                    # there may be some other bug affecting this?
                    if $valid {
                        with $decoder {
                            my $rest = .consume-all-chars();
                            emit $rest if $rest ne '';
                        }
                    }
                }
            }
        }
    }
}

# vim: expandtab shiftwidth=4
