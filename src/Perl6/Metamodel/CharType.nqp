# Handles type declarations that really map down to string types of some kind,
# containing information about the native type used to represent characters, so
# they should be composed as a stringy representation.
role Perl6::Metamodel::CharType {
    has int $!char_type;
    has int $!is_char_type;

    method is_char_type($obj) {
        $!is_char_type ?? 1 !! 0
    }

    method raw_char_type($obj) {
        $!char_type
    }

    method char_type($obj) {
        if $!char_type == nqp::const::P6STR_C_TYPE_CHAR {
            'char'
        }
        elsif $!char_type == nqp::const::P6STR_C_TYPE_WCHAR_T {
            'wchar_t'
        }
        elsif $!char_type == nqp::const::P6STR_C_TYPE_CHAR16_T {
            'char16_t'
        }
        elsif $!char_type == nqp::const::P6STR_C_TYPE_CHAR32_T {
            'char32_t'
        }
        else {
            ''
        }
    }

    method set_char_type($obj, $type) {
        if $type eq 'char' {
            $!char_type := nqp::const::P6STR_C_TYPE_CHAR;
        }
        elsif $type eq 'wchar_t' {
            $!char_type := nqp::const::P6STR_C_TYPE_WCHAR_T;
        }
        elsif $type eq 'char16_t' {
            $!char_type := nqp::const::P6STR_C_TYPE_CHAR16_T;
        }
        elsif $type eq 'char32_t' {
            $!char_type := nqp::const::P6STR_C_TYPE_CHAR32_T;
        }
        else {
            nqp::die("The CharType REPR received an unknown native character type");
        }
    }
}

