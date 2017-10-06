role Encoding {
    method name(--> Str) { ... }
    method alternative-names() { () }
    method encoder(*%options --> Encoding::Encoder) { ... }
    method decoder(*%options --> Encoding::Decoder) { ... }
}

# vim: ft=perl6 expandtab sw=4
