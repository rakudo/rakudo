role Perl6::Metamodel::Naming {
    has $!name;
    has $!shortname;
    method set_name($obj, $name) {
        $!name := $name;
        my @parts := nqp::split('::', $name);
#?if moar
        nqp::setdebugtypename($obj, $name);
#?endif
        $!shortname := @parts ?? @parts[nqp::elems(@parts) - 1] !! '';
    }
    method name($obj) {
        $!name
    }
    method shortname($obj) {
        $!shortname
    }
}
