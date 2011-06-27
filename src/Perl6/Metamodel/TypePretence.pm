role Perl6::Metamodel::TypePretence {
    my @pretending;
    
    method pretend_to_be(@types) {
        @pretending := @types;
    }
    
    method prentending_to_be() {
        @pretending
    }
}
