#- Metamodel::BoolificationProtocol --------------------------------------------
role Perl6::Metamodel::BoolificationProtocol {
    has int $!boolification_mode;

    method get_boolification_mode($XXX?) {
        $!boolification_mode
    }

    method set_boolification_mode($XXX, int $mode) {
        $!boolification_mode := $mode;
    }

    # XXX shouldn't this be part of set_boolification_mode??
    method publish_boolification_spec($class) {

        # No mode set (yet)
        if $!boolification_mode == 0 {
            my $method := self.find_method($class, 'Bool', :no_fallback);
            # Default to "not a type object" if we've no available method.
            nqp::setboolspec($class, $method ?? 0 !! 5, $method)
        }

        # Specific mode set
        else {
            nqp::setboolspec($class, $!boolification_mode, nqp::null)
        }
    }
}

# vim: expandtab sw=4
