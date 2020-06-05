# Handles type declarations that really map down to array types of some kind,
# and thus should be composed as an array-ish representation.
role Perl6::Metamodel::ArrayType {
    has int $!is_array_type;
    has $!array_type;

    method is_array_type($obj) {
        $!is_array_type
    }

    method array_type($obj) {
        $!array_type
    }

    method set_array_type($obj, $type) {
        $!is_array_type := 1;
        $!array_type := $type;
    }
}

# vim: expandtab sw=4
