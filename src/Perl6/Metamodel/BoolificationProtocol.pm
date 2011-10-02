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
            if pir::defined($meth) {
                pir::set_boolification_spec__0PiP($obj, 0, $meth)
            }
            else {
                # Default to "not a type object" if we've no available method.
                pir::set_boolification_spec__0PiP($obj, 5, pir::null__P())
            }
        }
        else {
            pir::set_boolification_spec__0PiP($obj, $!boolification_mode, pir::null__P())
        }
    }
}
