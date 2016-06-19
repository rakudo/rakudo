role Perl6::Metamodel::Naming {
    has $!name;
    has $!shortname;
    method set_name($obj, $name) {
        $!name := $name;
        my @parts := nqp::split('::', $name);
        nqp::setdebugtypename($obj, $name);
        $!shortname := @parts ?? @parts[nqp::elems(@parts) - 1] !! '';
    }
    method set_shortname($obj, $shortname) {
        $!shortname := $shortname;
    }
    method name($obj) {
        $!name
    }
    method shortname($obj) {
        $!shortname
    }
}
