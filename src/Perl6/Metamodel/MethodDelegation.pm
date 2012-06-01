role Perl6::Metamodel::MethodDelegation {
    my $delegate_type;
    
    method delegate_methods_to($type) {
        $delegate_type := $type
    }
    
    method delegating_methods_to() {
        $delegate_type
    }
    
    method find_method($obj, $name) {
        nqp::findmethod($delegate_type, $name)
    }
}
