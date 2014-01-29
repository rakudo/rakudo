role Perl6::Metamodel::TypePretense {
    my @pretending;
    
    method pretend_to_be(@types) {
        @pretending := @types;
    }
    
    method pretending_to_be() {
        @pretending
    }
    
    method type_check($obj, $checkee) {
        if $obj =:= $checkee {
            1
        }
        else {
            for self.pretending_to_be() {
                if $checkee =:= $_ {
                    return 1;
                }
            }
            0
        }
    }
}
