role Perl6::Metamodel::Naming {
    has $!name;
    has $!shortname;
    method set_name($obj, $name) {
        $!name := $name;
        nqp::setdebugtypename($obj, $name);
        if $name {
            my @names := nqp::split('[', $name);
            if nqp::elems(@names) > 1 {
                my @main := nqp::split('::', @names[0]);
                my @sub  := nqp::split('::', @names[1]);
                $!shortname :=
                  @main[nqp::elems(@main) - 1]
                    ~ '[' ~ @sub[nqp::elems(@sub) - 1];
            }
            else {
                my @parts := nqp::split('::', $name);
                $!shortname := @parts[nqp::elems(@parts) - 1];
            }
        }
        else {
            $!shortname := '';
        }
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
