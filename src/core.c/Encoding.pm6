role Encoding {
    method name(--> Str) { ... }
    method alternative-names() { Empty }
    method encoder(*%options --> Encoding::Encoder) { ... }
    method decoder(*%options --> Encoding::Decoder) { ... }
}

# vim: expandtab shiftwidth=4
