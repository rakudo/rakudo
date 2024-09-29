#- Metamodel::ArrayType --------------------------------------------------------
# Handles type declarations that really map down to array types of some kind,
# and thus should be composed as an array-ish representation.
role Perl6::Metamodel::ArrayType {
    has $!array_type;
#?if jvm
    has int $!is_array_type;
#?endif

    method TWEAK(*%_) {
        $!array_type := nqp::null;
    }

    method array_type($XXX?) { $!array_type    }
    method is_array_type($XXX?) {
#?if !jvm
        nqp::not_i(nqp::isnull($!array_type))
#?endif
#?if jvm
        $!is_array_type
#?endif
    }
    method set_array_type($XXX, $type) {
        $!array_type := $type;
#?if jvm
        $!is_array_type := 1;
#?endif
    }
}

# vim: expandtab sw=4
