# Handles type declarations that really map down to array types of some kind,
# and thus should be composed as an array-ish representation.
role Perl6::Metamodel::ArrayType {
    has     $!array_type;
    has int $!is_array_type;

    method    array_type($XXX?) { $!array_type    }
    method is_array_type($XXX?) { $!is_array_type }

    method set_array_type($XXX, $type) {
        $!array_type    := $type;
        $!is_array_type := 1;
    }
}

# vim: expandtab sw=4
