# Implemented by meta-objects that don't do inheritance per se,
# but want to base themselves on another type and mostly behave
# like they support it.
role Perl6::Metamodel::BaseType {
    has $!base_type;
    
    method set_base_type($obj, $base_type) {
        $!base_type := $base_type;
    }
}
