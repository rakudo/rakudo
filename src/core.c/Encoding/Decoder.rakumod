role Encoding::Decoder {
    method add-bytes(Blob:D $bytes --> Nil) { ... }
    method consume-available-chars(--> Str:D) { ... }
    method consume-all-chars(--> Str:D) { ... }
    method consume-exactly-chars(int $chars, Bool:D :$eof = False --> Str) { ... }
    method set-line-separators(@seps --> Nil) { ... }
    method consume-line-chars(Bool:D :$chomp = False, Bool:D :$eof = False --> Str) { ... }
    method is-empty(--> Bool) { ... }
    method bytes-available(--> Int:D) { ... }
    method consume-exactly-bytes(int $bytes --> Blob) { ... }
}

# vim: expandtab shiftwidth=4
