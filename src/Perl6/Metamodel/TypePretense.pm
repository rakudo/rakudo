role Perl6::Metamodel::TypePretense {
    my @pretending;
    
    method pretend_to_be(@types) {
        @pretending := @types;
    }
    
    method prentending_to_be() {
        @pretending
    }
    
    method type_check($obj, $checkee) {
        if $obj =:= $checkee {
            return 1;
        }
        for self.prentending_to_be() {
            if $checkee =:= $_ {
                return 1;
            }
        }
    }
}
