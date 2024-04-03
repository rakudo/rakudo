#- Metamodel::TypePretense -----------------------------------------------------
# Handle those classes that pretend to be some type without actually
# inheriting from them
role Perl6::Metamodel::TypePretense {
    my @pretending;

    method pretending_to_be()    { @pretending           }
    method pretend_to_be(@types) { @pretending := @types }

    method type_check($target, $checkee) {
        nqp::eqaddr($target, $checkee)
          || self.checkee_eqaddr_list($checkee, self.pretending_to_be)
    }
}

# vim: expandtab sw=4
