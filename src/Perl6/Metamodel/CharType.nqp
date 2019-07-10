# Handles type declarations that really map down to string types of some kind,
# containing information about the native type used to represent characters, so
# they should be composed as a stringy representation.
role Perl6::Metamodel::CharType {
    has int $!char_type;
    has int $!has_char_type;

    method has_char_type($obj) {
        $!has_char_type ?? 1 !! 0
    }

    method char_type($obj) {
        $!char_type
    }

    method set_char_type($obj, $type) {
        $!char_type     := $type;
        $!has_char_type := 1;
    }
}

