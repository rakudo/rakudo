role Perl6::Metamodel::Naming {
    has $!name;
    method set_name($obj, $name) {
        $!name := $name
    }
    method name($obj) {
        $!name
    }
}
