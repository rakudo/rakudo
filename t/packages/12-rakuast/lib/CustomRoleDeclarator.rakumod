# A module whose EXPORTHOW adds a role declarator with a meta-object it
# keeps to itself.
my class CustomRoleHOW is Metamodel::ParametricRoleHOW { }
my package EXPORTHOW {
    package DECLARE {
        constant custom-role = CustomRoleHOW;
    }
}
