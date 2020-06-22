role Perl6::Metamodel::BoolificationProtocol {
    has $!boolification_mode;

    method get_boolification_mode($obj) {
        $!boolification_mode
    }

    method set_boolification_mode($obj, $mode) {
        $!boolification_mode := $mode;
    }

    method publish_boolification_spec($obj) {
        if $!boolification_mode == 0 {
            my $meth := self.find_method($obj, 'Bool', :no_fallback(1));
            if nqp::defined($meth) {
                nqp::setboolspec($obj, 0, $meth)
            }
            else {
                # Default to "not a type object" if we've no available method.
                nqp::setboolspec($obj, 5, nqp::null())
            }
        }
        else {
            nqp::setboolspec($obj, $!boolification_mode, nqp::null())
        }
    }
}

# vim: expandtab sw=4
